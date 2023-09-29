#!/usr/bin/env python

import re
import click
import git
from rich import print
from rich.console import Console

console = Console()

@click.group()
def cli():
    pass

@cli.command()
@click.option('--branch', default='develop', help='Branch to compare with the current branch.')
@click.option('--repo-path', default='.', help='Path to the git repository.')
@click.option('--ignore-path', '-i', multiple=True, help='Regex pattern for paths to ignore. This option can be provided multiple times.')
def allchange(branch, repo_path, ignore_path):
    """List all changes between the current branch and another branch."""
    repo = git.Repo(repo_path)
    diff = repo.git.diff(f"{branch}...", unified=0)

    # Compile the provided regex patterns for path ignoring
    ignore_patterns = [re.compile(pattern) for pattern in ignore_path]

    # Patterns to extract the filename and line number
    filename_pattern = re.compile(r"^\+\+\+ b/(.*)")
    lineno_pattern = re.compile(r"@@ -(\d+),\d+ \+(\d+),\d+ @@")
    file_name = None
    lineno = 0

    # Check if a given path should be ignored
    def should_ignore(path):
        return any(pattern.match(path) for pattern in ignore_patterns)

    # Extract and print filename:line for each added or deleted line
    for line in diff.split('\n'):
        if line.startswith('+++'):
            match = filename_pattern.match(line)
            if match:
                file_name = match.group(1)
        elif line.startswith('@@'):
            match = lineno_pattern.match(line)
            if match:
                lineno = int(match.group(2))
        elif line.startswith('+') and not line.startswith('+++'):
            if not should_ignore(file_name):
                console.print(f"[green]{file_name}[/green]:[yellow]{lineno}[/yellow]+ [blue]{line[1:].strip()}[/blue]")
                lineno += 1
        elif line.startswith('-') and not line.startswith('---'):
            if not should_ignore(file_name):
                console.print(f"[red]{file_name}[/red]:[yellow]{lineno}[/yellow]- [red]{line[1:].strip()}[/red]")
                # We don't increment lineno for deletions, because the line number should reference the original file state.

if __name__ == "__main__":
    cli()
