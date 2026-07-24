#!/usr/bin/env python
import argparse
import hashlib
import json
import os
import re
import subprocess
import zipfile
from pathlib import Path, PurePosixPath

MAX_FILENAME_LEN = 255
MAX_BASENAME_LEN = 143
MAX_PATH_LEN = 4096


def sha(text: str, n: int = 12) -> str:
    return hashlib.sha1(text.encode("utf-8")).hexdigest()[:n]


def shorten_filename(filename: str, max_len: int = MAX_BASENAME_LEN) -> str:
    p = Path(filename)
    suffix = p.suffix
    stem = p.stem

    if len(filename.encode("utf-8")) <= max_len:
        return filename

    tag = "__" + sha(filename)
    keep = max_len - len(suffix.encode("utf-8")) - len(tag.encode("utf-8"))
    keep = max(keep, 16)

    result = f"{stem[:keep]}{tag}{suffix}"
    while len(result.encode("utf-8")) > max_len and keep > 16:
        keep -= 1
        result = f"{stem[:keep]}{tag}{suffix}"

    return result


def shorten_dirname(dirname: str, max_len: int = 80) -> str:
    if len(dirname.encode("utf-8")) <= max_len:
        return dirname

    tag = "__" + sha(dirname)
    keep = max(max_len - len(tag.encode("utf-8")), 12)

    result = dirname[:keep] + tag
    while len(result.encode("utf-8")) > max_len and keep > 12:
        keep -= 1
        result = dirname[:keep] + tag

    return result


def sanitize_parts(member_name: str) -> list[str]:
    # ZIP paths conventionally use "/", but normalize Windows-style paths too.
    member_name = member_name.replace("\\", "/")

    parts = []
    for part in PurePosixPath(member_name).parts:
        if part in ("", ".", "/", ".."):
            continue
        parts.append(part)

    return parts


def build_safe_path(base_dir: Path, member_name: str) -> Path:
    parts = sanitize_parts(member_name)
    if not parts:
        parts = ["unnamed"]

    dirs = [shorten_dirname(part) for part in parts[:-1]]
    filename = shorten_filename(parts[-1])

    candidate = base_dir.joinpath(*dirs, filename)

    if len(str(candidate).encode("utf-8")) > MAX_PATH_LEN:
        dirs = [sha(directory, 16) for directory in dirs]
        filename = shorten_filename(filename, max_len=100)
        candidate = base_dir.joinpath(*dirs, filename)

    return candidate


def ensure_unique_safe(path: Path) -> Path:
    parent = path.parent
    stem = path.stem
    suffix = path.suffix
    candidate = path
    counter = 1

    while True:
        if len(candidate.name.encode("utf-8")) > MAX_FILENAME_LEN:
            candidate = parent / shorten_filename(candidate.name, max_len=120)
            stem = candidate.stem
            suffix = candidate.suffix

        if len(str(candidate).encode("utf-8")) > MAX_PATH_LEN:
            candidate = parent / shorten_filename(candidate.name, max_len=100)
            stem = candidate.stem
            suffix = candidate.suffix

        if not candidate.exists():
            return candidate

        extra = f"_{counter}"
        max_stem = max(100 - len(extra.encode("utf-8")) - len(suffix.encode("utf-8")), 16)
        candidate = parent / f"{stem[:max_stem]}{extra}{suffix}"
        counter += 1


def gh_api_json(endpoint: str) -> dict:
    result = subprocess.run(
        ["gh", "api", "--method", "GET", endpoint],
        check=True,
        capture_output=True,
        text=True,
    )
    return json.loads(result.stdout)


def get_run_metadata(owner: str, repo: str, run_id: int) -> dict:
    return gh_api_json(f"/repos/{owner}/{repo}/actions/runs/{run_id}")


def workflow_yaml_name(owner: str, repo: str, run: dict) -> str:
    # The run object records the workflow path associated with this specific run.
    workflow_path = run.get("path")

    # Fallback for API responses which do not include path.
    if not workflow_path and run.get("workflow_id"):
        workflow = gh_api_json(
            f"/repos/{owner}/{repo}/actions/workflows/{run['workflow_id']}"
        )
        workflow_path = workflow.get("path")

    if not workflow_path:
        return "unknown-workflow"

    name = PurePosixPath(workflow_path).name
    if name.endswith(".yaml"):
        name = name[:-5]
    elif name.endswith(".yml"):
        name = name[:-4]

    return shorten_dirname(name or "unknown-workflow")


def download_logs(owner: str, repo: str, run_id: int, zip_path: Path) -> None:
    temp_path = zip_path.with_suffix(zip_path.suffix + ".partial")

    try:
        with open(temp_path, "wb") as output:
            subprocess.run(
                [
                    "gh",
                    "api",
                    "--method",
                    "GET",
                    f"/repos/{owner}/{repo}/actions/runs/{run_id}/logs",
                ],
                check=True,
                stdout=output,
            )

        if not zipfile.is_zipfile(temp_path):
            raise RuntimeError(
                f"GitHub did not return a valid ZIP archive for run {run_id}: {temp_path}"
            )

        os.replace(temp_path, zip_path)
    finally:
        temp_path.unlink(missing_ok=True)


def extract_zip(zip_path: Path, output_dir: Path) -> None:
    output_dir.mkdir(parents=True, exist_ok=True)

    with zipfile.ZipFile(zip_path, "r") as zf:
        for info in zf.infolist():
            if info.is_dir():
                continue

            target = build_safe_path(output_dir, info.filename)
            target.parent.mkdir(parents=True, exist_ok=True)
            target = ensure_unique_safe(target)

            with zf.open(info) as src, open(target, "wb") as dst:
                while chunk := src.read(1024 * 1024):
                    dst.write(chunk)

            original_name = PurePosixPath(info.filename.replace("\\", "/")).name
            action = "Renamed" if target.name != original_name else "Extracted"
            print(f"{action}: {info.filename} -> {target}")


def parse_owner_repo(value: str) -> tuple[str, str]:
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", value):
        raise argparse.ArgumentTypeError(
            "owner/repo must contain exactly one slash and valid GitHub name characters"
        )

    return tuple(value.split("/", 1))


def get_all_jobs(owner: str, repo: str, run_id: int) -> list[dict]:
    """Fetch every job across all attempts, following pagination."""
    jobs = []
    page = 1
    while True:
        endpoint = (
            f"/repos/{owner}/{repo}/actions/runs/{run_id}/jobs"
            f"?filter=all&per_page=100&page={page}"
        )
        payload = gh_api_json(endpoint)
        batch = payload.get("jobs", [])
        jobs.extend(batch)
        if len(batch) < 100:
            break
        page += 1
    return jobs


def download_job_log(owner: str, repo: str, job_id: int, target: Path) -> bool:
    """Download a single job's log. Returns False if no log is available."""
    target.parent.mkdir(parents=True, exist_ok=True)
    result = subprocess.run(
        [
            "gh", "api", "--method", "GET",
            f"/repos/{owner}/{repo}/actions/jobs/{job_id}/logs",
        ],
        capture_output=True,
    )

    if result.returncode != 0:
        stderr = result.stderr.decode("utf-8", "replace")
        # 404 => no log for this job (skipped/queued/expired). Not fatal.
        if "HTTP 404" in stderr:
            return False
        raise RuntimeError(
            f"Failed to download log for job {job_id}: {stderr.strip()}"
        )

    target.write_bytes(result.stdout)
    return True


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Download and safely unpack GitHub Actions run logs."
    )
    parser.add_argument("owner_repo", type=parse_owner_repo, help="GitHub repository, e.g. abc/cdf")
    parser.add_argument("run_id", type=int, help="GitHub Actions workflow run ID")
    args = parser.parse_args()

    owner, repo = args.owner_repo
    run_id = args.run_id

    print(f"Fetching workflow metadata for {owner}/{repo} run {run_id}...")
    run = get_run_metadata(owner, repo, run_id)
    workflow_name = workflow_yaml_name(owner, repo, run)

    output_dir = Path(f"{workflow_name}__run_{run_id}__logs")
    output_dir.mkdir(parents=True, exist_ok=True)

    jobs = get_all_jobs(owner, repo, run_id)
    print(f"Found {len(jobs)} jobs (all attempts).")

    downloaded = 0
    skipped = 0
    for job in jobs:
        job_id = job["id"]

        # Jobs that never produced logs will 404; skip common non-run states early.
        if job.get("status") == "queued" or job.get("conclusion") == "skipped":
            print(f"Skipping job {job_id} ({job['name']}): {job.get('conclusion') or job.get('status')}")
            skipped += 1
            continue

        label = f"{job.get('run_attempt', 1)}__{job['name']}__{job.get('conclusion') or job.get('status')}"
        filename = shorten_filename(f"{label}.txt")
        target = ensure_unique_safe(output_dir / filename)

        print(f"Downloading log for job {job_id}: {job['name']}")
        if download_job_log(owner, repo, job_id, target):
            downloaded += 1
        else:
            print(f"  no log available for job {job_id}, skipping")
            skipped += 1

    print(f"\nDone: {downloaded} logs downloaded, {skipped} jobs skipped.")


if __name__ == "__main__":
    main()
