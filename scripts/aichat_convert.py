#!/usr/bin/env python

from pathlib import Path
from typing import List, Dict, Any
import yaml
import markdown
from dominate import document
from dominate.tags import *
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from pygments.util import ClassNotFound
from dominate.util import raw
import re


def parse_yaml_file(file_path: Path) -> Dict[str, Any]:
    """Parse YAML file and return content."""
    with open(file_path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f)


def highlight_code_blocks(content: str) -> str:
    """Apply syntax highlighting to code blocks."""

    def replace_code_block(match):
        lang = match.group(1) or "text"
        code = match.group(2)
        try:
            lexer = get_lexer_by_name(lang)
        except ClassNotFound:
            lexer = get_lexer_by_name("text")
        formatter = HtmlFormatter(cssclass="highlight")
        return highlight(code, lexer, formatter)

    pattern = r"```(\w+)?\n(.*?)\n```"
    return re.sub(pattern, replace_code_block, content, flags=re.DOTALL)


def create_session_html(session_data: Dict[str, Any],
                        output_path: Path) -> None:
    """Create HTML file for a single session."""
    doc = document(title=f"Session: {session_data.get('model', 'Unknown')}")

    with doc.head:
        style("""
            body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
            .message { margin: 20px 0; padding: 15px; border-radius: 5px; }
            .user { background-color: #e3f2fd; }
            .assistant { background-color: #f5f5f5; }
            .role { font-weight: bold; margin-bottom: 10px; }
            .highlight { background-color: #f8f8f8; padding: 10px; border-radius: 3px; overflow-x: auto; }
            pre { white-space: pre-wrap; }
        """)
        style(HtmlFormatter().get_style_defs(".highlight"))

    with doc:
        h1(f"Model: {session_data.get('model', 'Unknown')}")

        for msg in session_data.get("compressed_messages",
                                    session_data.get("messages", [])):
            role = msg.get("role", "unknown")
            content = msg.get("content", "")

            with div(cls=f"message {role}"):
                div(role.capitalize(), cls="role")
                highlighted_content = highlight_code_blocks(content)
                content_html = markdown.markdown(highlighted_content,
                                                 extensions=["fenced_code"])
                raw(content_html)

    with open(output_path, "w", encoding="utf-8") as f:
        f.write(doc.render())


def create_index_html(session_files: List[Path], output_dir: Path) -> None:
    """Create index.html file listing all sessions."""
    doc = document(title="AI Chat Sessions")

    with doc.head:
        style("""
            body { font-family: Arial, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
            .session-list { list-style-type: none; padding: 0; }
            .session-item { margin: 10px 0; padding: 10px; background-color: #f5f5f5; border-radius: 5px; }
            .session-link { text-decoration: none; color: #1976d2; font-weight: bold; }
            .session-link:hover { text-decoration: underline; }
        """)

    with doc:
        h1("AI Chat Sessions")
        session_list = ul(cls="session-list")

        for session_file in sorted(session_files):
            html_name = session_file.stem + ".html"
            with session_list:
                li(a(session_file.stem, href=html_name, cls="session-link"),
                   cls="session-item")

    with open(output_dir / "index.html", "w", encoding="utf-8") as f:
        f.write(doc.render())


def main() -> None:
    """Main function to convert YAML session files to HTML."""
    config_dir = Path.home() / ".config" / "aichat" / "sessions"
    output_dir = Path.home() / ".local" / "aichat-render" / "sessions"

    # Create output directory
    output_dir.mkdir(parents=True, exist_ok=True)

    # Find all YAML files
    yaml_files = list(config_dir.glob("*.yaml")) + list(
        config_dir.glob("*.yml"))

    if not yaml_files:
        print("No YAML files found in session directory")
        return

    # Process each YAML file
    for yaml_file in yaml_files:
        session_data = parse_yaml_file(yaml_file)
        output_path = output_dir / (yaml_file.stem + ".html")
        create_session_html(session_data, output_path)
        print(f"Converted: {yaml_file.name} -> {output_path.name}")

    # Create index file
    create_index_html(yaml_files, output_dir)
    print(f"Created index.html with {len(yaml_files)} sessions")


if __name__ == "__main__":
    main()
