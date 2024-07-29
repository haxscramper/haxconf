#!/usr/bin/env python

import struct
from dataclasses import dataclass, asdict
from typing import List, Optional, List, Any
from pprint import pprint
from enum import IntEnum
import zlib
import io
import argparse
import json

def dataclass_to_dict(dataclass_obj: Any) -> dict:
    return asdict(dataclass_obj)

def dict_to_dataclass(dict_obj: dict, dataclass_type: Any) -> Any:
    return dataclass_type(**dict_obj)

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

    @staticmethod
    def from_stream(file):
        ident, display_scale, image_len = struct.unpack(
            "<c d q", file.read(17))
        image_data = file.read(image_len)
        return Image(
            ident=ident.decode(),
            display_scale=display_scale,
            image_len=image_len,
            image_data=image_data,
        )

    def to_stream(self, file):
        image_header = struct.pack("<c d q", self.ident.encode(),
                                   self.display_scale, self.image_len)
        file.write(image_header)
        file.write(self.image_data)


@dataclass
class String:
    text: str

    @staticmethod
    def from_stream(file) -> "String":
        length = struct.unpack("<I", file.read(4))[0]
        text = file.read(length).decode("utf-8") if length else ""
        return String(text)

    def to_stream(self, file):
        encoded_text = self.text.encode("utf-8")
        length = len(encoded_text)
        file.write(struct.pack("<I", length))  # Write string length
        file.write(encoded_text)  # Write string


class CellStyle(IntEnum):
    STYLE_BOLD = 1
    STYLE_ITALIC = 2
    STYLE_FIXED = 4
    STYLE_UNDERLINE = 8
    STYLE_STRIKETHROUGH = 16


@dataclass
class CellText:
    text: String
    relative_size: int
    image_index: int
    style: List[CellStyle]
    last_edit: int

    @staticmethod
    def from_stream(file):
        text = String.from_stream(file)
        relativesize, imageindex, stylebits, lastedit = struct.unpack(
            "<3I Q", file.read(20))

        style = [
            value for value in list(CellStyle)
            if bool(int(value.value) & stylebits)
        ]

        return CellText(
            text,
            relativesize,
            imageindex,
            style=style,
            last_edit=lastedit,
        )

    def to_stream(self, file):
        # Write the text string
        self.text.to_stream(file)

        # Convert the style list to a bitfield
        stylebits = sum(style.value for style in self.style)

        # Pack and write the other attributes
        cell_text_data = struct.pack("<3I Q", self.relative_size,
                                     self.image_index, stylebits,
                                     self.last_edit)
        file.write(cell_text_data)


@dataclass
class CellGrid:
    xs: int
    ys: int
    border_color: int
    user_grid_outer_spacing: int
    vertical_text_and_grid: int
    folded: int
    column_widths: List[int]
    cells: List["Cell"]

    @staticmethod
    def from_stream(file):
        xs, ys, bordercolor, user_grid_outer_spacing = struct.unpack(
            "<4I", file.read(16))
        vertical_text_and_grid, folded = struct.unpack("<2b", file.read(2))

        column_widths = list(struct.unpack(f"<{xs}I", file.read(4 * xs)))
        grid = CellGrid(
            xs,
            ys,
            bordercolor,
            user_grid_outer_spacing,
            vertical_text_and_grid,
            folded,
            column_widths,
            cells=[],
        )

        grid.cells = [Cell.from_stream(file) for _ in range(xs * ys)]
        return grid

    def to_stream(self, file):
        # Pack and write the grid header
        grid_header = struct.pack("<4I 2b", self.xs, self.ys,
                                  self.border_color,
                                  self.user_grid_outer_spacing,
                                  self.vertical_text_and_grid, self.folded)
        file.write(grid_header)

        # Pack and write column widths
        column_widths_format = f"<{self.xs}I"
        column_widths_data = struct.pack(column_widths_format,
                                         *self.column_widths)
        file.write(column_widths_data)

        # Write each cell in the grid
        for cell in self.cells:
            cell.to_stream(file)


class CellContentType(IntEnum):
    TS_TEXT = 0
    TS_GRID = 1
    TS_BOTH = 2
    TS_NEITHER = 3


class CellType(IntEnum):
    CT_DATA = 0
    CT_CODE = 1
    CT_VARD = 2
    CT_VIEWH = 3
    CT_VARU = 4
    CT_VIEWV = 5


class CellDrawStyle(IntEnum):
    DS_GRID = 0
    DS_BLOBSHIER = 1
    DS_BLOBLINE = 3


@dataclass
class Cell:
    celltype: CellType
    cellcolor: int
    textcolor: int
    drawstyle: CellDrawStyle
    cellcontents: CellContentType
    text: Optional[CellText] = None
    grid: Optional[CellGrid] = None
    is_selected: bool = False

    @staticmethod
    def from_stream(file: io.IOBase) -> 'Cell':
        cell_header = struct.unpack("<B 2I 2B", file.read(11))
        celltype, cellcolor, textcolor, drawstyle, cellcontents = cell_header
        # The first cell that is within the selection is marked with the highest bit set
        # to 1 (1 x x x x x x x) in cellcontents.
        is_selected = bool(0b1000_0000 & cellcontents)
        cellcontents = 0b0111_1111 & cellcontents
        cell = Cell(
            celltype=CellType(celltype),
            cellcolor=cellcolor,
            textcolor=textcolor,
            drawstyle=CellDrawStyle(drawstyle),
            cellcontents=CellContentType(cellcontents),
            is_selected=is_selected,
        )

        match cell.cellcontents:
            case CellContentType.TS_TEXT:
                cell.text = CellText.from_stream(file)

            case CellContentType.TS_BOTH:
                cell.text = CellText.from_stream(file)
                cell.grid = CellGrid.from_stream(file)

            case CellContentType.TS_GRID:
                cell.grid = CellGrid.from_stream(file)

        return cell

    def to_stream(self, file: io.IOBase) -> None:
        # Pack the cell header
        cellcontents = self.cellcontents.value
        if self.is_selected:
            cellcontents |= 0b1000_0000  # Set the highest bit if selected
        cell_header = struct.pack("<B 2I 2B", self.celltype.value,
                                  self.cellcolor, self.textcolor,
                                  self.drawstyle.value, cellcontents)
        file.write(cell_header)

        # Write text and grid if they exist
        if self.cellcontents in (CellContentType.TS_TEXT,
                                 CellContentType.TS_BOTH) and self.text:
            self.text.to_stream(file)

        if self.cellcontents in (CellContentType.TS_GRID,
                                 CellContentType.TS_BOTH) and self.grid:
            self.grid.to_stream(file)


@dataclass
class File:
    header: FileHeader = None
    xs: int = 0
    xy: int = 0
    zoomlevel: int = 0
    images: List[Image] = None
    root: Cell = None
    tagnames: List[str] = None

    def read_uncompressed(self, file: io.IOBase):
        header_data = struct.unpack("<4sc", file.read(5))
        xs, xy, zoomlevel = struct.unpack("<3b", file.read(3))
        file_header = FileHeader(
            magic=header_data[0].decode(),
            version=ord(header_data[1].decode()),
        )

        assert file_header.magic == "TSFF", file_header
        assert file_header.version == 23

        images = []

        def at_image():
            next_byte = file.peek(1)[:1]
            return next_byte in (b"I", b"J")

        while at_image():
            images.append(Image.from_stream(file))

        document_delimier = struct.unpack("<c", file.read(1))[0]
        assert document_delimier == b"D", document_delimier

        self.header = file_header
        self.xs = xs
        self.xy = xy
        self.zoomlevel = zoomlevel
        self.images = images

    @staticmethod
    def get_uncompressed(file: io.IOBase) -> bytes:
        result = File()
        result.read_uncompressed(file)
        return zlib.decompress(file.read())

    @staticmethod
    def from_stream(file: io.IOBase) -> 'File':
        result = File()
        result.read_uncompressed(file)
        zlib_io = io.BytesIO(zlib.decompress(file.read()))
        result.root = Cell.from_stream(zlib_io)
        tagnames = None
        return result

    def to_stream_header(self, file: io.IOBase):
        file.write(
            struct.pack("<4sc", self.header.magic.encode(),
                        bytes([self.header.version])))

        file.write(struct.pack("<3b", self.xs, self.xy, self.zoomlevel))

        for image in self.images:
            image.to_stream(file)

        file.write(b"D")

    def to_bytes(self) -> bytes:
        zlib_io = io.BytesIO()
        self.root.to_stream(zlib_io)
        String("").to_stream(zlib_io)
        return zlib_io.getvalue()

    def to_stream(self, file: io.IOBase):
        self.to_stream_header(file)
        file.write(zlib.compress(self.to_bytes(), level=9))


def peek_bytes(bytes_io: io.BytesIO, count: int) -> bytes:
    start = bytes_io.tell()
    result = bytes_io.read(count)
    bytes_io.seek(start)
    return result


def format_hexdump(
    bytes_io: io.IOBase,
    max_lines: int = 10,
    chunk_size: int = 16,
    hex_width: int = 49,
) -> str:
    main_start = bytes_io.tell()
    result = ""

    def format_hex_line(offset, bytes_chunk):
        hex_part = " ".join(f"{byte:02x}" for byte in bytes_chunk).ljust(49)
        ascii_part = "".join(
            chr(byte) if 32 <= byte < 127 else "." for byte in bytes_chunk)
        return f"{offset:08x}  {hex_part} |{ascii_part}|\n"

    offset = 0
    line_idx = 0

    eq = "="
    result += f"{eq * 8}  {main_start:^{hex_width}} {eq * 18}\n"

    while line_idx < max_lines:
        current_pos = bytes_io.tell()  # Save current position
        bytes_chunk = bytes_io.read(chunk_size)
        if not bytes_chunk:
            break
        bytes_io.seek(current_pos)  # Restore position

        result += format_hex_line(offset, bytes_chunk)
        offset += chunk_size
        bytes_io.seek(offset)  # Advance to the next chunk
        line_idx += 1

    bytes_io.seek(main_start)

    return result


def pack(input_file: str, output_file: str) -> None:
    # Implement packing logic here
    print(f"Packing {input_file} into {output_file}")


def unpack(input_file: str, output_file: str) -> None:
    with open(input_file, "rb") as file:
        content = File.from_stream(file)

    with open(output_file, "w") as file:
        # pprint(asdict(content), stream=file)
        file.write(json.dumps(asdict(content), indent=2))

    buffer = io.BytesIO()
    content.to_stream(buffer)
    buffer.seek(0)  # Reset the buffer's file pointer to the beginning

    debug_compressed = True

    # Read the reconverted binary data
    reconverted_data = buffer.getvalue() if debug_compressed else content.to_bytes()

    # Read the original binary data
    with open(input_file, "rb") as file:
        original_data = file.read(
        ) if debug_compressed else File.get_uncompressed(file)

    left_format = format_hexdump(io.BytesIO(original_data),
                                 max_lines=200).split("\n")
    right_format = format_hexdump(io.BytesIO(reconverted_data),
                                  max_lines=200).split("\n")
    left_align = max([len(it) for it in left_format])

    def format_cmp(lhs, rhs: str) -> str:
        return "  " if lhs == rhs else "er"

    concat = "\n".join([
        lhs.ljust(left_align) + f" [{format_cmp(lhs, rhs)}] " + rhs
        for lhs, rhs in zip(left_format, right_format)
    ])

    # Compare the original and reconverted data
    if original_data != reconverted_data:
        print(f"Error: Data loss detected.\n{concat}")


def main():
    parser = argparse.ArgumentParser(description="Pack and Unpack CLI tool")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Subparser for the "pack" command
    pack_parser = subparsers.add_parser("pack", help="Pack a file")
    pack_parser.add_argument("input_file", type=str, help="Input file to pack")
    pack_parser.add_argument("--output",
                             type=str,
                             help="Output file location",
                             required=True)
    pack_parser.set_defaults(func=pack)

    # Subparser for the "unpack" command
    unpack_parser = subparsers.add_parser("unpack", help="Unpack a file")
    unpack_parser.add_argument("input_file",
                               type=str,
                               help="Input file to unpack")
    unpack_parser.add_argument("--output",
                               type=str,
                               help="Output file location",
                               required=True)
    unpack_parser.set_defaults(func=unpack)

    # Parse arguments and call the appropriate function
    args = parser.parse_args()
    args.func(args.input_file, args.output)


if __name__ == "__main__":
    main()
