#!/usr/bin/env python

import sys
import os
import re
from urllib.parse import urlparse
from newspaper import Article, Config
from ebooklib import epub
from pathlib import Path
import bs4
from dominate import tags
from typing import List
import logging


def is_html(content: str) -> bool:
    """ Check if the string is HTML content. """
    if "<br/>" in content or "<div " in content:
        soup = bs4.BeautifulSoup(content, "html.parser")
        return bool(soup.find())
    return False


def html_to_plaintext(html_content: str) -> str:
    """ Convert HTML content to plaintext while preserving line breaks and paragraphs. """
    soup = bs4.BeautifulSoup(html_content, "html.parser")

    for script in soup(["script", "style"]):
        script.extract()

    for br in soup.find_all("br"):
        br.replace_with("\n")

    for p in soup.find_all("p"):
        p.append("\n")

    return soup.get_text()


def slugify(value: str) -> str:
    value = re.sub(r"[^\w\s-]", "", value).strip().lower()
    return re.sub(r"[-\s]+", "-", value)


def fetch_article(path: Path) -> str:
    plaintext: str = path.read_text()
    if is_html(plaintext):
        plaintext = html_to_plaintext(plaintext)

    content = tags.div()
    for line in plaintext.splitlines():
        clean = line.strip()
        if clean:
            content.add(tags.p(clean))

    return content.render()


def main():
    if len(sys.argv) != 3:
        print("Usage: python script.py <input> <output>")
        sys.exit(1)

    input_file = Path(sys.argv[1])
    output_file = Path(sys.argv[2])

    files = [input_file] if input_file.is_file() else sorted(
        input_file.rglob("*.txt"))
    chapters: List[epub.EpubHtml] = []

    book = epub.EpubBook()
    book.add_metadata("DC", "subject", "article")

    for file in files:
        article = fetch_article(file)
        xhtml_name = f"{file.parent.name}_{file.stem}.xhtml"
        relpath = file.relative_to(input_file)
        chapter = epub.EpubHtml(
            file_name=xhtml_name,
            lang="en",
            title=str(relpath if file != input_file else file.stem),
        )

        chapter.content = article
        book.add_item(chapter)
        chapters.append(chapter)

        # dbg_path = Path("/tmp").joinpath(relpath)
        # dbg_path.parent.mkdir(parents=True, exist_ok=True)
        # dbg_path.with_suffix(".original.txt").write_text(file.read_text())
        # dbg_path.with_suffix(".converted.txt").write_text(article)
        print(f"{relpath}")

    book.toc = [
        epub.Link(chapter.file_name, chapter.title, chapter.file_name)
        for chapter in chapters
    ]
    book.add_item(epub.EpubNcx())
    book.add_item(epub.EpubNav())

    book.spine = ["nav", *chapters]
    epub.write_epub(output_file, book, {})

    print(f"EPUB saved to {output_file}")


if __name__ == "__main__":
    main()
