#!/usr/bin/env python

import click
import os
import json
from collections import Counter


def load_history(cache_file):
    if os.path.exists(cache_file):
        with open(cache_file, "r") as f:
            return Counter(json.load(f))
    return Counter()


def save_history(counter, cache_file):
    with open(cache_file, "w") as f:
        json.dump(dict(counter), f)


@click.group()
def cli():
    pass


@cli.command()
@click.argument("cache_file", type=click.Path())
def get(cache_file):
    """Sorts the piped-in lines by their frequency of appearance, as stored in a cache file."""
    # Load history from cache file
    history = load_history(cache_file)

    # Read options from stdin
    options = [
        line.strip() for line in click.get_text_stream("stdin").readlines()
    ]

    # Sort options based on frequency
    sorted_options = sorted(options, key=lambda x: -history[x])
    click.echo("\n".join(sorted_options))


@cli.command()
@click.argument("cache_file", type=click.Path())
@click.argument("selected_value")
def store(cache_file, selected_value):
    """Stores the frequency of the selected value in a cache file."""
    # Load history from cache file
    history = load_history(cache_file)

    # Update history
    history[selected_value] += 1

    # Save updated history
    save_history(history, cache_file)


if __name__ == "__main__":
    cli()
