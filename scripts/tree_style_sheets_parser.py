#!/usr/bin/env python

import struct
from dataclasses import dataclass
from typing import List, Optional, List
from pprint import pprint
from enum import Enum
import zlib
import io


@dataclass
class FileHeader:
    magic: str
    version: int


@dataclass
class Image:
    ident: str
    display_scale: float
    image_len: int
    image_data: bytes


@dataclass
class String:
    text: str


@dataclass
class CellText:
    text: String
    relative_size: int
    image_index: int
    style_bits: int
    last_edit: int


@dataclass
class CellGrid:
    xs: int
    ys: int
    border_color: int
    user_grid_outer_spacing: int
    vertical_text_and_grid: int
    folded: int
    column_widths: List[int]
    cells: List['Cell']


class CellContentType(Enum):
    TS_TEXT = 0
    TS_GRID = 1
    TS_BOTH = 2
    TS_NEITHER = 3


class CellType(Enum):
    CT_DATA = 0
    CT_CODE = 1
    CT_VARD = 2
    CT_VIEWH = 3
    CT_VARU = 4
    CT_VIEWV = 5


@dataclass
class Cell:
    celltype: CellType
    cellcolor: int
    textcolor: int
    drawstyle: int
    cellcontents: CellContentType
    text: Optional[CellText] = None
    grid: Optional[CellGrid] = None


@dataclass
class File:
    header: FileHeader
    images: List[Image]
    root: Cell
    tagnames: List[str]


def print_hexdump(bytes_io: io.BytesIO):
    main_start = bytes_io.tell()
    def format_hex_line(offset, bytes_chunk):
        hex_part = ' '.join(f'{byte:02x}' for byte in bytes_chunk).ljust(49)
        ascii_part = ''.join(chr(byte) if 32 <= byte < 127 else '.' for byte in bytes_chunk)
        return f'{offset:08x}  {hex_part} |{ascii_part}|'

    offset = 0
    chunk_size = 16

    while True:
        current_pos = bytes_io.tell()  # Save current position
        bytes_chunk = bytes_io.read(chunk_size)
        if not bytes_chunk:
            break
        bytes_io.seek(current_pos)  # Restore position

        print(format_hex_line(offset, bytes_chunk))
        offset += chunk_size
        bytes_io.seek(offset)  # Advance to the next chunk

    bytes_io.seek(main_start)



def read_string(file) -> String:
    length = struct.unpack('<I', file.read(4))[0]
    text = file.read(length).decode('utf-8') if length else ''
    return String(text)



def read_cell(file) -> Cell:
    cell_header = struct.unpack('<B 2I 2B', file.read(11))
    celltype, cellcolor, textcolor, drawstyle, cellcontents = cell_header
    cell = Cell(
        celltype=CellType(celltype),
        cellcolor=cellcolor,
        textcolor=textcolor,
        drawstyle=drawstyle,
        cellcontents=CellContentType(cellcontents),
    )

    if cell.cellcontents in [CellContentType.TS_TEXT,
                             CellContentType.TS_BOTH]:  # TS_TEXT or TS_BOTH
        text = read_string(file)
        print_hexdump(file)
        relativesize, imageindex, stylebits, lastedit = struct.unpack(
            '<3I Q', file.read(20))
        cell.text = CellText(
            text,
            relativesize,
            imageindex,
            stylebits,
            lastedit,
        )

    if cell.cellcontents in [CellContentType.TS_GRID,
                             CellContentType.TS_BOTH]:  # TS_GRID or TS_BOTH
        xs, ys, bordercolor, user_grid_outer_spacing = struct.unpack('<4I', file.read(16))
        vertical_text_and_grid, folded = struct.unpack('<2b', file.read(2))

        column_widths = list(struct.unpack(f'<{xs}I', file.read(4 * xs)))
        cell.grid = CellGrid(
            xs,
            ys,
            bordercolor,
            user_grid_outer_spacing,
            vertical_text_and_grid,
            folded,
            column_widths,
            cells=[]
        )

        cell.grid = [read_cell(file) for _ in range(xs * ys)]

    return cell


def parse_file(filename: str) -> File:
    with open(filename, 'rb') as file:
        header_data = struct.unpack('<4sc', file.read(5))
        file_header = FileHeader(
            magic=header_data[0].decode(),
            version=ord(header_data[1].decode()),
        )
        pprint(file_header)
        assert file_header.magic == "TSFF", file_header

        images = []

        def at_image():
            next_byte = file.peek(1)[:1]
            return next_byte in (b'I', b'J')

        while at_image():
            ident, display_scale, image_len = struct.unpack(
                '<c d q', file.read(17))
            image_data = file.read(image_len)
            images.append(
                Image(
                    ident=ident.decode(),
                    display_scale=display_scale,
                    image_len=image_len,
                    image_data=image_data,
                ))

        document_delimier = struct.unpack("<c", file.read(1))[0]
        assert document_delimier == b'D', document_delimier
        zlib_content = file.read()
        zlib_io = io.BytesIO(zlib.decompress(zlib_content))

        root = read_cell(zlib_io)
        tagnames = None

        # Handling of 'Cell' and 'String' structures is omitted due to complexity

        return File(header=file_header,
                    images=images,
                    root=root,
                    tagnames=tagnames)


# Example usage
file = parse_file('/home/maxim_artemov/Documents/tree_sheets_test_1.cts')
pprint(file)
