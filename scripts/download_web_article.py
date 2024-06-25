#!/usr/bin/env python

import sys
import os
import re
from urllib.parse import urlparse
from newspaper import Article
from ebooklib import epub


def slugify(value: str) -> str:
    value = re.sub(r"[^\w\s-]", "", value).strip().lower()
    return re.sub(r"[-\s]+", "-", value)


def fetch_article(url: str) -> dict:
    article = Article(url)
    article.download()
    article.parse()

    title = article.title if article.title else None
    author = article.authors[0] if article.authors else "Unknown Author"
    content = article.text

    return {"title": title, "author": author, "content": content}


def create_epub(article: dict, output_path: str):
    book = epub.EpubBook()
    book.set_title(article["title"])
    book.add_author(article["author"])

    chapter = epub.EpubHtml(title=article["title"],
                            file_name="chap_01.xhtml",
                            lang="en")
    chapter.content = f"<h1>{article['title']}</h1><p>{article['content']}</p>"
    book.add_item(chapter)

    book.toc = (epub.Link("chap_01.xhtml", article["title"], "intro"), )
    book.add_item(epub.EpubNcx())
    book.add_item(epub.EpubNav())
    book.spine = ["nav", chapter]

    epub.write_epub(output_path, book, {})


def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <url>")
        sys.exit(1)

    url = sys.argv[1]
    article = fetch_article(url)

    if article["title"]:
        file_name = slugify(article["title"])
    else:
        parsed_url = urlparse(url)
        file_name = os.path.basename(parsed_url.path) or "article"
        file_name = slugify(file_name)

    output_path = f"/tmp/{file_name}.epub"
    create_epub(article, output_path)

    print(f"EPUB saved to {output_path}")


if __name__ == "__main__":
    main()
