#!/usr/bin/env python
import sys
from ruamel.yaml import YAML
from ruamel.yaml.scalarstring import LiteralScalarString


def force_literal_style(data):
    if isinstance(data, dict):
        for key in data:
            force_literal_style(data[key])
            if isinstance(data[key], str) and '\n' in data[key]:
                data[key] = LiteralScalarString(data[key])
    elif isinstance(data, list):
        for i, item in enumerate(data):
            force_literal_style(item)
            if isinstance(item, str) and '\n' in item:
                data[i] = LiteralScalarString(item)


def process_file(path):
    yaml = YAML()
    yaml.preserve_quotes = False
    yaml.default_flow_style = False
    yaml.width = 4096

    with open(path, 'r') as f:
        data = yaml.load(f)

    force_literal_style(data)

    with open(path, 'w') as f:
        yaml.dump(data, f)

    print(f"Processed: {path}")


for path in sys.argv[1:]:
    try:
        process_file(path)
    except Exception as e:
        print(f"Error processing {path}: {e}", file=sys.stderr)
