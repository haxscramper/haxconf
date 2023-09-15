#!/usr/bin/env python

import click
import os
import json
from datetime import datetime
from collections import defaultdict

def load_history(cache_file):
    result = defaultdict(lambda: {"count": 0, "time": 0})

    if os.path.exists(cache_file):
        with open(cache_file, "r") as f:
            for key, value in json.load(f).items():
                result[key] = value

    return result


def save_history(counter, cache_file):
    with open(cache_file, "w") as f:
        json.dump(counter, f, indent=2)

@click.group()
def cli():
    pass


@cli.command()
@click.argument("cache_file", type=click.Path())
@click.option("--order", type=click.Choice(["frequency", "time"]))
def get(cache_file, order):
    """Sorts the piped-in lines by their frequency of appearance, as stored in a cache file."""
    # Load history from cache file
    history = load_history(cache_file)

    # Read options from stdin
    options = [
        line.strip() for line in click.get_text_stream("stdin").readlines()
    ]

    def get_value(key):
        if order == "time":
            return -history[key]["time"]

        else:
            return -history[key]["count"]

    # Sort options based on frequency
    sorted_options = sorted(options, key=lambda x: get_value(x))
    click.echo("\n".join(sorted_options))

@cli.command()
@click.argument("cache_file", type=click.Path())
@click.argument("selected_value")
def store(cache_file, selected_value):
    """Stores the frequency of the selected value in a cache file."""
    # Load history from cache file
    history = load_history(cache_file)

    # Update history
    history[selected_value]["count"] += 1
    history[selected_value]["time"] = datetime.now().timestamp()

    # Save updated history
    save_history(history, cache_file)


if __name__ == "__main__":
    cli()
