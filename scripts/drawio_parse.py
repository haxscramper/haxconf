#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "pydantic>=2.8",
# ]
# ///

from __future__ import annotations

import argparse
import json
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict
from enum import Enum
from pathlib import Path

from pydantic import BaseModel, ConfigDict, Field


def parse_style(style: str | None) -> dict[str, str]:
    if not style:
        return {}
    result: dict[str, str] = {}
    for item in style.split(";"):
        if not item:
            continue
        if "=" in item:
            k, v = item.split("=", 1)
            result[k] = v
        else:
            result[item] = "1"
    return result


def parse_bool_flag(value: str | None) -> bool:
    return value in {"1", "true", "True"}


def strip_tag(tag: str) -> str:
    if "}" in tag:
        return tag.rsplit("}", 1)[1]
    return tag


class ObjectKind(str, Enum):
    CELL = "cell"


class CellRole(str, Enum):
    ROOT = "root"
    LAYER = "layer"
    VERTEX = "vertex"
    EDGE = "edge"
    CELL = "cell"


class CellCategory(str, Enum):
    ROOT = "root"
    LAYER = "layer"
    EDGE = "edge"
    POOL = "pool"
    LANE_OR_SWIMLANE = "lane_or_swimlane"
    SHAPE = "shape"
    TEXT = "text"
    VERTEX = "vertex"
    ORPHAN = "orphan"
    CELL = "cell"


class MxPoint(BaseModel):
    model_config = ConfigDict(extra="forbid")

    x: float | None = None
    y: float | None = None


class MxGeometry(BaseModel):
    model_config = ConfigDict(extra="forbid")

    x: float | None = None
    y: float | None = None
    width: float | None = None
    height: float | None = None
    relative: bool | None = None
    as_name: str | None = Field(default=None, alias="as")
    points: list[MxPoint] = Field(default_factory=list)
    source_point: MxPoint | None = None
    target_point: MxPoint | None = None
    offset: MxPoint | None = None


class DrawioObject(BaseModel):
    model_config = ConfigDict(extra="forbid")

    id: str
    parent: str | None = None
    value: str | None = None
    style: str | None = None
    style_map: dict[str, str] = Field(default_factory=dict)

    kind: ObjectKind = ObjectKind.CELL
    role: CellRole = CellRole.CELL

    vertex: bool = False
    edge: bool = False
    source: str | None = None
    target: str | None = None

    geometry: MxGeometry | None = None
    children: list["DrawioObject"] = Field(default_factory=list)

    category: CellCategory
    shape_name: str | None = None


class RootBundle(BaseModel):
    model_config = ConfigDict(extra="forbid")

    file_attrs: dict[str, str] = Field(default_factory=dict)
    diagram_attrs: dict[str, str] = Field(default_factory=dict)
    graph_model_attrs: dict[str, str] = Field(default_factory=dict)

    objects_by_id: dict[str, DrawioObject]
    roots: list[DrawioObject]


def to_float(value: str | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value)
    except ValueError:
        return None


def parse_geometry(geom_elem: ET.Element | None) -> MxGeometry | None:
    if geom_elem is None:
        return None

    geometry = MxGeometry(
        x=to_float(geom_elem.get("x")),
        y=to_float(geom_elem.get("y")),
        width=to_float(geom_elem.get("width")),
        height=to_float(geom_elem.get("height")),
        relative=parse_bool_flag(geom_elem.get("relative")) if geom_elem.get("relative") is not None else None,
        **({"as": geom_elem.get("as")} if geom_elem.get("as") is not None else {}),
    )

    for child in geom_elem:
        tag = strip_tag(child.tag)
        if tag == "Array" and child.get("as") == "points":
            for point_elem in child:
                if strip_tag(point_elem.tag) == "mxPoint":
                    geometry.points.append(
                        MxPoint(
                            x=to_float(point_elem.get("x")),
                            y=to_float(point_elem.get("y")),
                        )
                    )
        elif tag == "mxPoint":
            as_name = child.get("as")
            point = MxPoint(
                x=to_float(child.get("x")),
                y=to_float(child.get("y")),
            )
            if as_name == "sourcePoint":
                geometry.source_point = point
            elif as_name == "targetPoint":
                geometry.target_point = point
            elif as_name == "offset":
                geometry.offset = point

    return geometry


def categorize_cell(
    *,
    cell_id: str,
    parent: str | None,
    vertex: bool,
    edge: bool,
    style_map: dict[str, str],
) -> tuple[CellCategory, str | None]:
    if cell_id == "0":
        return CellCategory.ROOT, None
    if cell_id == "1":
        return CellCategory.LAYER, None
    if edge:
        return CellCategory.EDGE, None
    if vertex and style_map.get("swimlane") == "1":
        if style_map.get("childLayout") == "stackLayout":
            return CellCategory.POOL, None
        return CellCategory.LANE_OR_SWIMLANE, None
    if vertex:
        shape = style_map.get("shape")
        if shape:
            return CellCategory.SHAPE, shape
        if style_map.get("text") == "1":
            return CellCategory.TEXT, None
        return CellCategory.VERTEX, None
    if parent is None:
        return CellCategory.ORPHAN, None
    return CellCategory.CELL, None


def parse_mxcell(cell_elem: ET.Element) -> DrawioObject:
    cell_id = cell_elem.attrib["id"]
    parent = cell_elem.get("parent")
    value = cell_elem.get("value")
    style = cell_elem.get("style")
    style_map = parse_style(style)

    vertex = parse_bool_flag(cell_elem.get("vertex"))
    edge = parse_bool_flag(cell_elem.get("edge"))

    if cell_id == "0":
        role = CellRole.ROOT
    elif cell_id == "1":
        role = CellRole.LAYER
    elif edge:
        role = CellRole.EDGE
    elif vertex:
        role = CellRole.VERTEX
    else:
        role = CellRole.CELL

    geom_elem = next((c for c in cell_elem if strip_tag(c.tag) == "mxGeometry"), None)
    geometry = parse_geometry(geom_elem)
    category, shape_name = categorize_cell(
        cell_id=cell_id,
        parent=parent,
        vertex=vertex,
        edge=edge,
        style_map=style_map,
    )

    return DrawioObject(
        id=cell_id,
        parent=parent,
        value=value,
        style=style,
        style_map=style_map,
        role=role,
        vertex=vertex,
        edge=edge,
        source=cell_elem.get("source"),
        target=cell_elem.get("target"),
        geometry=geometry,
        category=category,
        shape_name=shape_name,
    )


def parse_drawio_xml(xml_text: str) -> RootBundle:
    root = ET.fromstring(xml_text)
    if strip_tag(root.tag) != "mxfile":
        raise ValueError("Expected root element <mxfile>")

    diagram = next((c for c in root if strip_tag(c.tag) == "diagram"), None)
    if diagram is None:
        raise ValueError("Missing <diagram> element")

    graph_model = next((c for c in diagram if strip_tag(c.tag) == "mxGraphModel"), None)
    if graph_model is None:
        raise ValueError("Missing <mxGraphModel> element")

    root_elem = next((c for c in graph_model if strip_tag(c.tag) == "root"), None)
    if root_elem is None:
        raise ValueError("Missing <root> element inside <mxGraphModel>")

    objects_by_id: dict[str, DrawioObject] = {}
    children_by_parent: dict[str, list[DrawioObject]] = defaultdict(list)

    for child in root_elem:
        if strip_tag(child.tag) != "mxCell":
            continue
        obj = parse_mxcell(child)
        if obj.id in objects_by_id:
            raise ValueError(f"Duplicate mxCell id: {obj.id}")
        objects_by_id[obj.id] = obj
        if obj.parent is not None:
            children_by_parent[obj.parent].append(obj)

    for obj in objects_by_id.values():
        obj.children = children_by_parent.get(obj.id, [])

    roots: list[DrawioObject] = []
    for obj in objects_by_id.values():
        if obj.parent is None or obj.parent not in objects_by_id:
            roots.append(obj)

    return RootBundle(
        file_attrs=dict(root.attrib),
        diagram_attrs=dict(diagram.attrib),
        graph_model_attrs=dict(graph_model.attrib),
        objects_by_id=objects_by_id,
        roots=roots,
    )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Parse draw.io XML into Pydantic objects and build parent/child tree."
    )
    parser.add_argument("input", type=Path, help="Path to draw.io XML file")
    args = parser.parse_args()

    xml_text = args.input.read_text(encoding="utf-8")
    bundle = parse_drawio_xml(xml_text)
    payload = bundle.model_dump(mode="json", by_alias=True)
    json.dump(payload, sys.stdout, indent=2, ensure_ascii=False)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())