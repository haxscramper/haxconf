#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "pydantic>=2.8",
#   "asciimatics",
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

import html
import re


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
    style: str | None = Field(default=None, exclude=True)
    style_map: dict[str, str] = Field(default_factory=dict, exclude=True)

    kind: ObjectKind = ObjectKind.CELL
    role: CellRole = CellRole.CELL

    vertex: bool = False
    edge: bool = False
    source: str | None = None
    target: str | None = None

    geometry: MxGeometry | None = None
    subnodes: list[str] = Field(default_factory=list)

    category: CellCategory
    shape_name: str | None = None


class RootBundle(BaseModel):
    model_config = ConfigDict(extra="forbid")

    file_attrs: dict[str, str] = Field(default_factory=dict)
    diagram_attrs: dict[str, str] = Field(default_factory=dict)
    graph_model_attrs: dict[str, str] = Field(default_factory=dict)

    objects_by_id: dict[str, DrawioObject]
    roots: list[DrawioObject]

    def object_value(self, object_id: str) -> str | None:
        obj = self.objects_by_id.get(object_id)
        return obj.value if obj else None


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
    subnodes_by_parent: dict[str, list[str]] = defaultdict(list)

    for child in root_elem:
        if strip_tag(child.tag) != "mxCell":
            continue
        obj = parse_mxcell(child)
        if obj.id in objects_by_id:
            raise ValueError(f"Duplicate mxCell id: {obj.id}")
        objects_by_id[obj.id] = obj
        if obj.parent is not None:
            subnodes_by_parent[obj.parent].append(obj.id)

    for obj in objects_by_id.values():
        obj.subnodes = subnodes_by_parent.get(obj.id, [])

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


def rootbundle_to_ascii(bundle: RootBundle, multiplier: float = 0.2) -> str:
    if multiplier <= 0:
        raise ValueError("multiplier must be > 0")

    objects = list(bundle.objects_by_id.values())

    def _strip_value(v: str | None) -> str:
        if not v:
            return ""
        txt = re.sub(r"<br\s*/?>", " ", v, flags=re.I)
        txt = re.sub(r"<[^>]+>", "", txt)
        return html.unescape(txt).strip()

    def _is_edge(o: DrawioObject) -> bool:
        return bool(o.edge or o.role == CellRole.EDGE or o.category == CellCategory.EDGE)

    # --- absolute origin resolution (parent + local) ---
    origin_cache: dict[str, tuple[float, float]] = {}

    def _abs_origin(obj: DrawioObject) -> tuple[float, float]:
        if obj.id in origin_cache:
            return origin_cache[obj.id]

        g = obj.geometry
        lx = g.x if (g and g.x is not None) else 0.0
        ly = g.y if (g and g.y is not None) else 0.0

        if obj.parent and obj.parent in bundle.objects_by_id:
            parent = bundle.objects_by_id[obj.parent]
            px, py = _abs_origin(parent)
            res = (px + lx, py + ly)
        else:
            res = (lx, ly)

        origin_cache[obj.id] = res
        return res

    def _geom_rect(o: DrawioObject) -> tuple[float, float, float, float] | None:
        g = o.geometry
        if not g:
            return None
        ox, oy = _abs_origin(o)
        w = g.width if g.width is not None else 1.0
        h = g.height if g.height is not None else 1.0
        return (ox, oy, w, h)

    def _obj_center(o: DrawioObject) -> tuple[float, float] | None:
        r = _geom_rect(o)
        if not r:
            return None
        x, y, w, h = r
        return (x + w / 2.0, y + h / 2.0)


    def _f(style: dict[str, str], key: str) -> float | None:
        v = style.get(key)
        if v is None or v == "":
            return None
        try:
            return float(v)
        except ValueError:
            return None

    def _anchor_from_style(edge_obj: DrawioObject, is_source: bool) -> tuple[float, float] | None:
        node_id = edge_obj.source if is_source else edge_obj.target
        if not node_id or node_id not in bundle.objects_by_id:
            return None
        node = bundle.objects_by_id[node_id]
        rect = _geom_rect(node)
        if not rect:
            return None
        x, y, w, h = rect
        sm = edge_obj.style_map

        # draw.io keys
        px = _f(sm, "exitX" if is_source else "entryX")
        py = _f(sm, "exitY" if is_source else "entryY")
        dx = _f(sm, "exitDx" if is_source else "entryDx") or 0.0
        dy = _f(sm, "exitDy" if is_source else "entryDy") or 0.0

        if px is not None and py is not None:
            return (x + px * w + dx, y + py * h + dy)

        # fallback: pick side facing the opposite node
        other_id = edge_obj.target if is_source else edge_obj.source
        if other_id and other_id in bundle.objects_by_id:
            c0 = _obj_center(node)
            c1 = _obj_center(bundle.objects_by_id[other_id])
            if c0 and c1:
                ox, oy = c0
                tx, ty = c1
                if abs(tx - ox) > abs(ty - oy):
                    return (x + (w if tx > ox else 0.0), y + h / 2.0)
                else:
                    return (x + w / 2.0, y + (h if ty > oy else 0.0))

        c = _obj_center(node)
        return c

    def _edge_points(o: DrawioObject) -> list[tuple[float, float]]:
        g = o.geometry
        pts: list[tuple[float, float]] = []

        src = _anchor_from_style(o, True)
        if src:
            pts.append(src)
        elif g and g.source_point and g.source_point.x is not None and g.source_point.y is not None:
            pts.append((g.source_point.x, g.source_point.y))

        if g:
            for p in g.points:
                if p.x is not None and p.y is not None:
                    pts.append((p.x, p.y))

        dst = _anchor_from_style(o, False)
        if dst:
            pts.append(dst)
        elif g and g.target_point and g.target_point.x is not None and g.target_point.y is not None:
            pts.append((g.target_point.x, g.target_point.y))

        # If orthogonal and no explicit waypoints, add Manhattan bend(s)
        if len(pts) == 2 and o.style_map.get("edgeStyle") == "orthogonalEdgeStyle":
            (x1, y1), (x2, y2) = pts
            my = (y1 + y2) / 2.0
            pts = [(x1, y1), (x1, my), (x2, my), (x2, y2)]

        return pts

    # bounds
    min_x = float("inf")
    min_y = float("inf")
    max_x = float("-inf")
    max_y = float("-inf")

    for obj in objects:
        rect = _geom_rect(obj)
        if not rect:
            continue
        x, y, w, h = rect
        min_x = min(min_x, x)
        min_y = min(min_y, y)
        max_x = max(max_x, x + w)
        max_y = max(max_y, y + h)

    for obj in objects:
        if not _is_edge(obj):
            continue
        for x, y in _edge_points(obj):
            min_x = min(min_x, x)
            min_y = min(min_y, y)
            max_x = max(max_x, x)
            max_y = max(max_y, y)

    if min_x == float("inf"):
        return ""

    pad = 2
    width = max(3, int(round((max_x - min_x) * multiplier)) + pad * 2 + 1)
    height = max(3, int(round((max_y - min_y) * multiplier)) + pad * 2 + 1)

    def sx(x: float) -> int:
        return int(round((x - min_x) * multiplier)) + pad

    def sy(y: float) -> int:
        return int(round((y - min_y) * multiplier)) + pad

    grid = [[" "] * width for _ in range(height)]

    def put(x: int, y: int, ch: str):
        if 0 <= x < width and 0 <= y < height:
            grid[y][x] = ch

    def draw_line(x1: int, y1: int, x2: int, y2: int, ch: str):
        dx = abs(x2 - x1)
        dy = -abs(y2 - y1)
        sx_ = 1 if x1 < x2 else -1
        sy_ = 1 if y1 < y2 else -1
        err = dx + dy
        x, y = x1, y1
        while True:
            put(x, y, ch)
            if x == x2 and y == y2:
                break
            e2 = 2 * err
            if e2 >= dy:
                err += dy
                x += sx_
            if e2 <= dx:
                err += dx
                y += sy_

    # draw non-edges
    for obj in objects:
        if _is_edge(obj):
            continue
        rect = _geom_rect(obj)
        if not rect:
            continue
        x, y, w, h = rect
        X, Y = sx(x), sy(y)
        W = max(1, int(round(w * multiplier)))
        H = max(1, int(round(h * multiplier)))
        x2 = X + W - 1
        y2 = Y + H - 1

        put(X, Y, "+"); put(x2, Y, "+"); put(X, y2, "+"); put(x2, y2, "+")
        for xx in range(X + 1, x2):
            put(xx, Y, "-")
            put(xx, y2, "-")
        for yy in range(Y + 1, y2):
            put(X, yy, "|")
            put(x2, yy, "|")

        label = _strip_value(bundle.object_value(obj.id))
        if label:
            avail = max(0, x2 - X - 1)
            for i, c in enumerate(label[:avail]):
                put(X + 1 + i, Y + 1, c)

    # draw edges last
    for obj in objects:
        if not _is_edge(obj):
            continue
        pts = _edge_points(obj)
        for (ax, ay), (bx, by) in zip(pts, pts[1:]):
            draw_line(sx(ax), sy(ay), sx(bx), sy(by), "*")

    return "\n".join("".join(row).rstrip() for row in grid).rstrip() + "\n"


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Parse draw.io XML into Pydantic objects and build parent/subnode tree."
    )
    parser.add_argument("input", type=Path, help="Path to draw.io XML file")
    args = parser.parse_args()

    xml_text = args.input.read_text(encoding="utf-8")
    bundle = parse_drawio_xml(xml_text)
    payload = bundle.model_dump(mode="json", by_alias=True, exclude_defaults=True)
    json.dump(payload, sys.stdout, indent=2, ensure_ascii=False)
    sys.stdout.write("\n")
    ascii_diagram = rootbundle_to_ascii(bundle, multiplier=0.15)
    print(ascii_diagram)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())