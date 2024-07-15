#!/usr/bin/env python

import re
import sys
from pathlib import Path
from datetime import datetime
from dateutil import parser


def convert_date(date_str: str) -> str:
    try:
        dt = parser.parse(date_str)
        return dt.strftime("[%Y-%m-%d %H:%M:%S%z]")
    except ValueError:
        return date_str


date_patterns = [
    r"\d{1,2}(?:th|st|nd|rd)?\s[A-Z][a-z]+(?:,\s\d{4})?",  # 15th July, 2024 or 15 July 2024
    r"[A-Z][a-z]+\s\d{1,2},\s\d{4}\s\d{1,2}:\d{2}\s[APM]{2}",  # July 15, 2024 10:02 AM
    r"\d{4}-\d{2}-\d{2}",  # 2024-07-15
    r"\d{2}/\d{2}/\d{4}",  # 07/15/2024
    r"\d{1,2}\s[A-Z][a-z]+\s\d{4}"  # 15 July 2024
]

combined_pattern = re.compile(
    "|".join(f"({pattern})" for pattern in date_patterns), )


def process_file(input_path: Path, output_path: Path) -> None:
    with input_path.open("r", encoding="utf-8") as infile, output_path.open(
            "w", encoding="utf-8") as outfile:
        for line in infile:
            line = combined_pattern.sub(lambda x: convert_date(x.group()),
                                        line)
            outfile.write(line)


if __name__ == "__main__":
    input_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])
    process_file(input_file, output_file)
