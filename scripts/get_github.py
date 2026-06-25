#!/usr/bin/env python
# /// script
# dependencies = [
#   "mistletoe",
# ]
# ///


from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Final, Sequence

from mistletoe import Document


QUERY: Final[str] = """
query($owner:String!, $name:String!, $number:Int!) {
  repository(owner:$owner, name:$name) {
    pullRequest(number:$number) {
      title
      url
      number
      author { login }
      createdAt
      body
      files(first: 100) {
        nodes {
          path
          additions
          deletions
        }
      }
      comments(first: 100) {
        nodes {
          author { login }
          createdAt
          body
          url
        }
      }
      reviewThreads(first: 100) {
        nodes {
          isResolved
          isOutdated
          path
          line
          startLine
          originalLine
          comments(first: 100) {
            nodes {
              author { login }
              createdAt
              body
              url
              diffHunk
              path
              line
              originalLine
              replyTo { id }
              pullRequestReview { id }
            }
          }
        }
      }
      reviews(first: 100) {
        nodes {
          id
          author { login }
          state
          submittedAt
          createdAt
          body
          url
        }
      }
    }
  }
}
"""


@dataclass(frozen=True)
class PullRequestRef:
    owner: str
    repo: str
    number: int


class MarkdownBuilder:
    def __init__(self) -> None:
        self.parts: list[str] = []

    def heading(self, level: int, text: str) -> None:
        self.parts.append(f"{'#' * level} {text}\n")

    def paragraph(self, text: str) -> None:
        self.parts.append(f"{text}\n")

    def bullet(self, text: str) -> None:
        self.parts.append(f"- {text}\n")

    def fenced_code(self, code: str, language: str = "") -> None:
        self.parts.append(f"```{language}\n")
        self.parts.append(code.rstrip("\n"))
        self.parts.append("\n```\n")

    def markdown_block(self, text: str) -> None:
        stripped = text.strip()
        if stripped:
            Document(stripped)
            self.parts.append(stripped)
            self.parts.append("\n")

    def blank(self) -> None:
        self.parts.append("\n")

    def build(self) -> str:
        return "".join(self.parts).rstrip() + "\n"


def run(cmd: Sequence[str]) -> str:
    result: subprocess.CompletedProcess[str] = subprocess.run(
        list(cmd),
        check=True,
        text=True,
        capture_output=True,
    )
    return result.stdout


def parse_pr_url(url: str) -> PullRequestRef:
    match = re.match(
        r"^https://github\.com/(?P<owner>[^/]+)/(?P<repo>[^/]+)/pull/(?P<number>\d+)(?:/.*)?$",
        url.strip(),
    )
    if not match:
        raise ValueError(f"Unsupported PR URL: {url}")

    return PullRequestRef(
        owner=match.group("owner"),
        repo=match.group("repo"),
        number=int(match.group("number")),
    )


def gh_graphql(owner: str, name: str, pr_number: int) -> dict[str, Any]:
    out = run([
        "gh",
        "api",
        "graphql",
        "-F",
        f"owner={owner}",
        "-F",
        f"name={name}",
        "-F",
        f"number={pr_number}",
        "-f",
        f"query={QUERY}",
    ])
    return json.loads(out)


def gh_pr_diff(pr_url: str) -> str:
    return run(["gh", "pr", "diff", pr_url])


def text(value: str | None) -> str:
    return value or ""


def author_name(node: dict[str, Any]) -> str:
    author = node.get("author")
    if isinstance(author, dict) and author.get("login"):
        return f"@{author['login']}"
    return "unknown"


def node_timestamp(node: dict[str, Any]) -> str:
    for key in ("submittedAt", "createdAt"):
        value = node.get(key)
        if value:
            return str(value)
    return ""


def thread_timestamp(thread: dict[str, Any]) -> str:
    timestamps = [
        str(c["createdAt"])
        for c in thread.get("comments", {}).get("nodes", [])
        if c.get("createdAt")
    ]
    return min(timestamps) if timestamps else ""


def format_thread_location(thread: dict[str, Any]) -> str:
    if thread.get("startLine") is not None and thread.get("line") is not None:
        return f" lines {thread['startLine']}-{thread['line']}"
    if thread.get("line") is not None:
        return f" line {thread['line']}"
    if thread.get("originalLine") is not None:
        return f" original line {thread['originalLine']}"
    return ""


def format_comment_location(comment: dict[str, Any], fallback_path: str) -> str:
    path = comment.get("path") or fallback_path
    if comment.get("line") is not None:
        return f"`{path}` line {comment['line']}"
    if comment.get("originalLine") is not None:
        return f"`{path}` original line {comment['originalLine']}"
    return f"`{path}`"


def render_review(md: MarkdownBuilder, review: dict[str, Any]) -> None:
    ts = node_timestamp(review)
    md.heading(
        3,
        f"Review by {author_name(review)} — {review['state']} — {ts}",
    )
    review_body = text(review.get("body"))
    if review_body.strip():
        md.markdown_block(review_body)
    else:
        md.paragraph("_No summary body (state-only review)_")
    md.paragraph(f"Source: {review['url']}")
    md.blank()


def render_thread(md: MarkdownBuilder, level: 3, thread: dict[str, Any]) -> None:
    clarify = ""
    if thread.get("isResolved"):
      clarify += " resolved"

    if thread.get("isOutdated"):
      clarify += " outdated"

    clarify = clarify.strip()
    if clarify:
      clarify = f" ({clarify})"

    resolved = "resolved" if thread.get("isResolved") else "unresolved"
    md.heading(
        level,
        f"Thread{clarify}: `{thread['path']}`{format_thread_location(thread)}",
    )
    md.blank()

    thread_comments: list[dict[str, Any]] = list(
        thread.get("comments", {}).get("nodes", [])
    )
    if not thread_comments:
        md.paragraph("_No inline comments_")
        md.blank()
        return

    # Preserve reply-chain order: comments under a thread always appear in
    # the order they were posted, regardless of which review they belong to.
    thread_comments.sort(key=lambda c: str(c.get("createdAt") or ""))

    for index, comment in enumerate(thread_comments):
        label = "Comment" if index == 0 else "Reply"
        md.heading(
            level + 1,
            f"{label} by {author_name(comment)} — {comment['createdAt']} — "
            f"{format_comment_location(comment, thread['path'])}",
        )

        if index == 0:
            diff_hunk = text(comment.get("diffHunk"))
            if diff_hunk.strip():
                md.fenced_code(diff_hunk, "diff")

        comment_body = text(comment.get("body"))
        if comment_body.strip():
            md.markdown_block(comment_body)
        else:
            md.paragraph("_Empty inline comment_")

        md.paragraph(f"Source: {comment['url']}")
        md.blank()

def build_markdown(pr: dict[str, Any], pr_diff: str) -> str:
    md = MarkdownBuilder()

    md.heading(1, f"PR #{pr['number']}: {pr['title']}")
    md.bullet(f"URL: {pr['url']}")
    md.bullet(f"Author: {author_name(pr)}")
    md.bullet(f"Created: {pr['createdAt']}")
    md.blank()

    md.heading(2, "Description")
    body = text(pr.get("body"))
    if body.strip():
        md.markdown_block(body)
    else:
        md.paragraph("_No description_")
    md.blank()

    md.heading(2, "Diff")
    md.fenced_code(pr_diff, "diff")
    md.blank()

    md.heading(2, "Changed Files")
    files: list[dict[str, Any]] = pr.get("files", {}).get("nodes", [])
    if not files:
        md.paragraph("_None_")
    else:
        for file_node in files:
            md.bullet(
                f"`{file_node['path']}` (+{file_node['additions']} / -{file_node['deletions']})"
            )
    md.blank()

    md.heading(2, "General Comments")
    comments: list[dict[str, Any]] = pr.get("comments", {}).get("nodes", [])
    if not comments:
        md.paragraph("_None_")
    else:
        for comment in comments:
            md.heading(3, f"{author_name(comment)} — {comment['createdAt']}")
            comment_body = text(comment.get("body"))
            if comment_body.strip():
                md.markdown_block(comment_body)
            else:
                md.paragraph("_Empty_")
            md.paragraph(f"Source: {comment['url']}")
            md.blank()

    md.heading(2, "Reviews and Review Threads (chronological)")

    reviews: list[dict[str, Any]] = pr.get("reviews", {}).get("nodes", [])
    threads: list[dict[str, Any]] = pr.get("reviewThreads", {}).get("nodes", [])

    # Map each thread to the review id of its first comment.
    def thread_review_id(thread: dict[str, Any]) -> str | None:
        for c in thread.get("comments", {}).get("nodes", []):
            review = c.get("pullRequestReview")
            if isinstance(review, dict) and review.get("id"):
                return review["id"]
        return None

    threads_by_review: dict[str | None, list[dict[str, Any]]] = {}
    for thread in threads:
        threads_by_review.setdefault(thread_review_id(thread), []).append(thread)

    # Order reviews chronologically; render each review then its threads.
    ordered_reviews = sorted(
        enumerate(reviews),
        key=lambda item: (node_timestamp(item[1]) or "", item[0]),
    )

    if not reviews and not threads:
        md.paragraph("_None_")
    else:
        for _order, review in ordered_reviews:
            render_review(md, review)
            child_threads = threads_by_review.get(review["id"], [])
            child_threads.sort(key=lambda t: thread_timestamp(t) or "")
            for thread in child_threads:
                render_thread(md, 4, thread)

        # Threads with no associated review (rare) go last.
        for thread in threads_by_review.get(None, []):
            render_thread(md, 3, thread)

    return md.build()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Export a GitHub PR discussion and diff to Markdown.",
    )
    parser.add_argument(
        "pr_url",
        help="Full GitHub pull request URL",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=None,
        help="Output file path",
    )
    args = parser.parse_args()

    try:
        ref = parse_pr_url(args.pr_url)
        data = gh_graphql(ref.owner, ref.repo, ref.number)
        pr: dict[str, Any] = data["data"]["repository"]["pullRequest"]
        pr_diff = gh_pr_diff(args.pr_url)
    except subprocess.CalledProcessError as exc:
        print(exc.stderr, file=sys.stderr)
        sys.exit(exc.returncode)
    except Exception as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)

    output_path = args.output or Path(
        f"{ref.owner}-{ref.repo}-pr-{ref.number}-discussion.md"
    )

    markdown = build_markdown(pr, pr_diff)
    output_path.write_text(markdown, encoding="utf-8")
    print(f"Wrote {output_path}")


if __name__ == "__main__":
    main()
