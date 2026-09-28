#!/usr/bin/env python

import hashlib
import shutil
import subprocess
import sys
import uuid
from collections import defaultdict
from pathlib import Path


def normalized_name(name: str) -> str:
    return "".join(character if character.isalnum() else "_"
                   for character in name.lower())


def file_hash(path: Path) -> str:
    digest = hashlib.sha256()

    with path.open("rb") as file:
        while chunk := file.read(1024 * 1024):
            digest.update(chunk)

    return digest.hexdigest()


def plan_names(
    files: list[Path],
    occupied_names: set[str],
) -> dict[Path, str]:
    groups: dict[tuple[str, str], list[Path]] = defaultdict(list)

    for path in files:
        groups[(normalized_name(path.stem), path.suffix)].append(path)

    used_names = occupied_names | {
        f"{base}{suffix}"
        for base, suffix in groups
    }
    planned: dict[Path, str] = {}

    for (base, suffix), group in sorted(groups.items()):
        group.sort(key=lambda path: path.name)
        normalized_filename = f"{base}{suffix}"

        if len(group) == 1 and normalized_filename not in occupied_names:
            planned[group[0]] = normalized_filename
            continue

        hashes = {path: file_hash(path) for path in group}
        paths_by_hash: dict[str, list[Path]] = defaultdict(list)

        for path, digest in hashes.items():
            paths_by_hash[digest].append(path)

        duplicate_counters: dict[str, int] = defaultdict(int)

        for path in group:
            digest = hashes[path]

            if len(paths_by_hash[digest]) > 1:
                while True:
                    duplicate_counters[digest] += 1
                    candidate = (f"{base}_{digest}_"
                                 f"{duplicate_counters[digest]}{suffix}")

                    if candidate not in used_names:
                        break
            else:
                candidate = ""

                for length in range(1, len(digest) + 1):
                    candidate = f"{base}_{digest[:length]}{suffix}"

                    if candidate not in used_names:
                        break
                else:
                    counter = 1

                    while True:
                        candidate = f"{base}_{digest}_{counter}{suffix}"

                        if candidate not in used_names:
                            break

                        counter += 1

            planned[path] = candidate
            used_names.add(candidate)

    return planned


def rename_files(
    directory: Path,
    planned: dict[Path, str],
) -> None:
    temporary: list[tuple[Path, Path]] = []
    destination_names = set(planned.values())

    for source, destination_name in planned.items():
        if source.name == destination_name:
            continue

        while True:
            temporary_path = (directory / f".normalize-{uuid.uuid4().hex}")

            if (not temporary_path.exists()
                    and temporary_path.name not in destination_names):
                break

        source.rename(temporary_path)
        temporary.append((temporary_path, directory / destination_name))

    for temporary_path, destination in temporary:
        temporary_path.rename(destination)


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit(f"Usage: {sys.argv[0]} DIRECTORY")

    directory = Path(sys.argv[1]).expanduser().resolve(strict=True)

    if not directory.is_dir():
        raise NotADirectoryError(directory)

    backup = directory / "original.bak"

    if backup.exists():
        raise FileExistsError(backup)

    entries = list(directory.iterdir())
    files = sorted(
        (entry for entry in entries if entry.is_file()),
        key=lambda path: path.name,
    )
    occupied_names = {entry.name for entry in entries if entry not in files}
    occupied_names.add(backup.name)

    backup.mkdir()

    for path in files:
        shutil.copy2(
            path,
            backup / path.name,
            follow_symlinks=False,
        )

    planned = plan_names(files, occupied_names)
    rename_files(directory, planned)

    subprocess.run(
        ["trash-put", str(backup)],
        check=True,
    )


if __name__ == "__main__":
    main()
