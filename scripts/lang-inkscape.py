#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.9"
# dependencies = ["zstandard"]
# ///

from loguru import logger

import argparse
import subprocess
import xml.etree.ElementTree as ET
from pathlib import Path
import math

SVG_NAMESPACE = "http://www.w3.org/2000/svg"
INKSCAPE_NAMESPACE = "http://www.inkscape.org/namespaces/inkscape"
SODIPODI_NAMESPACE = "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"

ET.register_namespace("", SVG_NAMESPACE)
ET.register_namespace("inkscape", INKSCAPE_NAMESPACE)
ET.register_namespace("sodipodi", SODIPODI_NAMESPACE)


def parse_view_box(root: ET.Element) -> tuple[float, float, float, float]:
    value = root.get("viewBox")

    if value is None:
        width = parse_length(root.get("width", "1000"))
        height = parse_length(root.get("height", "1000"))
        return 0.0, 0.0, width, height

    values = value.replace(",", " ").split()

    if len(values) != 4:
        raise ValueError(f"Invalid SVG viewBox: {value!r}")

    return tuple(float(item) for item in values)


def parse_length(value: str) -> float:
    value = value.strip()

    for suffix in ("px", "pt", "pc", "mm", "cm", "in"):
        if value.endswith(suffix):
            value = value[: -len(suffix)]
            break

    return float(value)


def format_number(value: float) -> str:
    return format(value, ".12g")


def parse_style(element: ET.Element) -> dict[str, str]:
    declarations: dict[str, str] = {}

    for declaration in element.get("style", "").split(";"):
        if ":" not in declaration:
            continue

        name, value = declaration.split(":", 1)
        declarations[name.strip()] = value.strip()

    return declarations


def has_white_background(
    root: ET.Element,
    x: float,
    y: float,
    width: float,
    height: float,
) -> bool:
    rectangle_tag = f"{{{SVG_NAMESPACE}}}rect"
    white_values = {
        "white",
        "#fff",
        "#ffffff",
        "rgb(255,255,255)",
        "rgb(100%,100%,100%)",
    }

    for element in root.iter(rectangle_tag):
        style = parse_style(element)
        fill = element.get("fill", style.get("fill", "")).lower()
        opacity = element.get("opacity", style.get("opacity", "1"))
        fill_opacity = element.get(
            "fill-opacity",
            style.get("fill-opacity", "1"),
        )

        if fill not in white_values:
            continue

        if opacity == "0" or fill_opacity == "0":
            continue

        rectangle_width_value = element.get("width")
        rectangle_height_value = element.get("height")

        if rectangle_width_value is None or rectangle_height_value is None:
            continue

        try:
            rectangle_x = parse_length(element.get("x", "0"))
            rectangle_y = parse_length(element.get("y", "0"))
            rectangle_width = parse_length(rectangle_width_value)
            rectangle_height = parse_length(rectangle_height_value)
        except ValueError:
            continue

        if (
            math.isclose(rectangle_x, x)
            and math.isclose(rectangle_y, y)
            and math.isclose(rectangle_width, width)
            and math.isclose(rectangle_height, height)
        ):
            return True

    return False


def remove_generated_backgrounds(root: ET.Element) -> None:
    rectangle_tag = f"{{{SVG_NAMESPACE}}}rect"

    for parent in root.iter():
        for element in list(parent):
            if (
                element.tag == rectangle_tag
                and element.get("id") == "org-inkscape-background"
            ):
                parent.remove(element)


def add_white_background(
    root: ET.Element,
    x: float,
    y: float,
    width: float,
    height: float,
) -> None:
    if has_white_background(root, x, y, width, height):
        return

    background = ET.Element(
        f"{{{SVG_NAMESPACE}}}rect",
        {
            "id": "org-inkscape-background",
            "x": format_number(x),
            "y": format_number(y),
            "width": format_number(width),
            "height": format_number(height),
            "fill": "#ffffff",
            "stroke": "none",
        },
    )

    root.insert(0, background)


def expand_canvas(path: Path) -> None:
    tree = ET.parse(path)
    root = tree.getroot()

    remove_generated_backgrounds(root)

    x, y, width, height = parse_view_box(root)

    expanded_x = x - width / 2.0 - 200.0
    expanded_y = y - height / 2.0 - 200.0
    expanded_width = width * 2.0 + 400.0
    expanded_height = height * 2.0 + 400.0

    root.set(
        "viewBox",
        " ".join(
            format_number(value)
            for value in (
                expanded_x,
                expanded_y,
                expanded_width,
                expanded_height,
            )
        ),
    )
    root.set("width", f"{format_number(expanded_width)}px")
    root.set("height", f"{format_number(expanded_height)}px")

    page_tag = f"{{{INKSCAPE_NAMESPACE}}}page"

    for page in root.iter(page_tag):
        page.set("x", format_number(expanded_x))
        page.set("y", format_number(expanded_y))
        page.set("width", format_number(expanded_width))
        page.set("height", format_number(expanded_height))

    tree.write(path, encoding="utf-8", xml_declaration=True)


def trim_canvas(path: Path) -> None:
    tree = ET.parse(path)
    root = tree.getroot()

    view_box_x, view_box_y, _, _ = parse_view_box(root)

    remove_generated_backgrounds(root)

    rectangle_tag = f"{{{SVG_NAMESPACE}}}rect"

    for element in list(root):
        if (
            element.tag == rectangle_tag
            and element.get("id") == "org-inkscape-background"
        ):
            root.remove(element)

    tree.write(path, encoding="utf-8", xml_declaration=True)

    result = subprocess.run(
        [
            "inkscape",
            "--query-all",
            str(path),
        ],
        check=True,
        capture_output=True,
        text=True,
    )

    bounding_boxes: list[tuple[float, float, float, float]] = []

    for line in result.stdout.splitlines():
        fields = line.rsplit(",", 4)

        if len(fields) != 5:
            continue

        _, relative_x, relative_y, width, height = fields

        bounding_boxes.append(
            (
                view_box_x + float(relative_x),
                view_box_y + float(relative_y),
                float(width),
                float(height),
            )
        )

    if not bounding_boxes:
        logger.error(f"ERR: SVG does not contain any drawable elements: {path.read_text()}")
        return

    minimum_x = min(x for x, _, _, _ in bounding_boxes)
    minimum_y = min(y for _, y, _, _ in bounding_boxes)
    maximum_x = max(x + width for x, _, width, _ in bounding_boxes)
    maximum_y = max(y + height for _, y, _, height in bounding_boxes)

    width = maximum_x - minimum_x
    height = maximum_y - minimum_y

    root.set(
        "viewBox",
        " ".join(
            format_number(value)
            for value in (
                minimum_x,
                minimum_y,
                width,
                height,
            )
        ),
    )
    root.set("width", f"{format_number(width)}px")
    root.set("height", f"{format_number(height)}px")

    page_tag = f"{{{INKSCAPE_NAMESPACE}}}page"

    for page in root.iter(page_tag):
        page.set("x", format_number(minimum_x))
        page.set("y", format_number(minimum_y))
        page.set("width", format_number(width))
        page.set("height", format_number(height))

    add_white_background(
        root,
        minimum_x,
        minimum_y,
        width,
        height,
    )

    tree.write(path, encoding="utf-8", xml_declaration=True)


def edit_svg(path: Path, existing: bool) -> Path:
    copy_path = Path("/tmp/tmp.svg")
    copy_path.write_text(path.read_text())
    if existing:
        expand_canvas(copy_path)

    logger.info("Starting inkscape")

    import uuid

    subprocess.run(
        [
            "inkscape",
            # give each inkscape instance a unique ID, otherwise the 
            # startup might reuse an existing one in some way,
            # and the processing instantly exits. 
            f"--app-id-tag=instance_{uuid.uuid4().hex}",
            str(copy_path),
        ],
        check=True,
    )

    logger.info(copy_path)
    trim_canvas(copy_path)
    return copy_path

def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)

    edit_parser = subparsers.add_parser("edit")
    edit_parser.add_argument("--existing", action="store_true")
    edit_parser.add_argument("source", type=Path)

    render_parser = subparsers.add_parser("render")
    render_parser.add_argument("source", type=Path)
    render_parser.add_argument("output", type=Path)

    arguments = parser.parse_args()

    result = edit_svg(arguments.source, arguments.existing)
    Path(arguments.source).write_text(result.read_text())


if __name__ == "__main__":
    main()
