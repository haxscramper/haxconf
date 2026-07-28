#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.9"
# dependencies = ["zstandard"]
# ///
"""Convert a Tracy profiler capture (.tracy) into a Chrome/Perfetto JSON trace.

Usage: tracy_to_perfetto.py <input.tracy> <output.json>

The .tracy format is a custom binary dump written by tracy::Worker::Write. This
script reimplements the reader side (tracy 0.9.0 - 0.13.3) up to the plot
section, which covers everything the JSON trace format can represent.
"""

import json
import struct
import sys

import zstandard

FILE_BUF_SIZE = 64 * 1024

TRACY_HEADER = b"tr\xfdP"
LZ4_HEADER = b"tlZ4"
ZSTD_HEADER = b"tZst"

MIN_SUPPORTED_VERSION = (0, 9, 0)
MAX_SUPPORTED_VERSION = (0, 13, 3)

GPU_CONTEXT_TYPES = [
    "Invalid",
    "OpenGL",
    "Vulkan",
    "OpenCL",
    "Direct3D12",
    "Direct3D11",
    "Metal",
    "Custom",
    "CUDA",
    "Rocprof",
    "tt_device",
]

MESSAGE_SOURCES = ["User", "Tracy"]
MESSAGE_SEVERITIES = ["Trace", "Debug", "Info", "Warning", "Error", "Fatal"]


def decompress(path):
    """Rebuild the logical byte stream of a .tracy file.

    The payload is a list of length-prefixed compressed blocks, dealt out
    round-robin to N independent compression streams. Every block decompresses
    to exactly one 64 KB reader buffer, so concatenating them in block order
    reproduces the stream the reader sees.
    """
    with open(path, "rb") as f:
        raw = f.read()

    magic = raw[:4]
    if magic == TRACY_HEADER:
        comp_type = raw[4]
        streams = raw[5]
        offset = 6
    elif magic == LZ4_HEADER:
        comp_type, streams, offset = 0, 1, 4
    elif magic == ZSTD_HEADER:
        comp_type, streams, offset = 1, 1, 4
    else:
        raise SystemExit(f"{path}: not a tracy dump (bad magic {magic!r})")

    if comp_type != 1:
        raise SystemExit(
            f"{path}: LZ4-compressed captures are not supported, only zstd. "
            "Re-save the capture with a recent tracy-capture."
        )

    decompressors = [zstandard.ZstdDecompressor().decompressobj() for _ in range(streams)]
    chunks = []
    index = 0
    while offset < len(raw):
        (size,) = struct.unpack_from("<I", raw, offset)
        offset += 4
        block = decompressors[index % streams].decompress(raw[offset : offset + size])
        offset += size
        index += 1
        if len(block) > FILE_BUF_SIZE:
            raise SystemExit(f"{path}: block {index - 1} decompressed past the reader buffer")
        if len(block) < FILE_BUF_SIZE:
            block += b"\0" * (FILE_BUF_SIZE - len(block))
        chunks.append(block)
    return b"".join(chunks)


class Reader:
    """Sequential cursor over the decompressed stream."""

    _U16 = struct.Struct("<H")
    _U32 = struct.Struct("<I")
    _I32 = struct.Struct("<i")
    _U64 = struct.Struct("<Q")
    _I64 = struct.Struct("<q")
    _F32 = struct.Struct("<f")
    _F64 = struct.Struct("<d")

    def __init__(self, data):
        self.data = data
        self.pos = 0

    def unpack(self, fmt):
        values = fmt.unpack_from(self.data, self.pos)
        self.pos += fmt.size
        return values

    def u8(self):
        value = self.data[self.pos]
        self.pos += 1
        return value

    def u16(self):
        return self.unpack(self._U16)[0]

    def u32(self):
        return self.unpack(self._U32)[0]

    def i32(self):
        return self.unpack(self._I32)[0]

    def u64(self):
        return self.unpack(self._U64)[0]

    def i64(self):
        return self.unpack(self._I64)[0]

    def f32(self):
        return self.unpack(self._F32)[0]

    def f64(self):
        return self.unpack(self._F64)[0]

    def int24(self):
        value = int.from_bytes(self.data[self.pos : self.pos + 3], "little")
        self.pos += 3
        return value

    def string_idx(self):
        """StringIdx stores idx+1; zero means inactive."""
        raw = int.from_bytes(self.data[self.pos : self.pos + 3], "little")
        self.pos += 3
        return raw - 1 if raw else None

    def string_ref(self):
        """StringRef: 8 byte payload plus isidx/active bit flags."""
        value = self.unpack(self._U64)[0]
        flags = self.data[self.pos]
        self.pos += 1
        return value, bool(flags & 1), bool(flags & 2)

    def bytes(self, count):
        chunk = self.data[self.pos : self.pos + count]
        self.pos += count
        return chunk

    def string(self):
        return self.bytes(self.u64()).decode("utf-8", "replace")

    def skip(self, count):
        self.pos += count


class SourceLocation:
    __slots__ = ("name", "function", "file", "line", "color")

    def __init__(self, name, function, file, line, color):
        self.name = name
        self.function = function
        self.file = file
        self.line = line
        self.color = color


EMPTY_SOURCE_LOCATION = SourceLocation((0, False, False), (0, False, False), (0, False, False), 0, 0)


class TracyCapture:
    def __init__(self, data):
        self.reader = Reader(data)
        self.version = (0, 0, 0)
        self.pid = 0
        self.last_time = 0
        self.capture_name = ""
        self.capture_program = ""
        self.host_info = ""
        self.string_data = []
        self.string_by_ptr = {}
        self.strings = {}
        self.thread_names = {}
        self.external_names = {}
        self.source_locations = {}
        self.source_location_expand = []
        self.source_location_payload = []
        self.frame_sets = []
        self.messages = []
        self.zone_extra = []
        self.threads = []
        self.gpu_contexts = []
        self.plots = []
        self.parse()

    # -- string / source location resolution, mirroring Worker::GetString ----

    def string_from_ptr(self, ptr):
        return self.strings.get(ptr, "???")

    def string_from_ref(self, ref):
        value, isidx, active = ref
        if isidx:
            return self.string_data[value]
        if active:
            return self.string_from_ptr(value)
        return "???"

    def string_from_idx(self, idx):
        return self.string_data[idx] if idx is not None else None

    def source_location(self, srcloc):
        if srcloc < 0:
            return self.source_location_payload[-srcloc - 1]
        if srcloc != 0x7FFF:
            return self.source_locations[self.source_location_expand[srcloc]]
        return EMPTY_SOURCE_LOCATION

    def zone_name(self, srcloc):
        location = self.source_location(srcloc)
        if location.name[2]:
            return self.string_from_ref(location.name)
        return self.string_from_ref(location.function)

    def thread_name(self, tid):
        name = self.thread_names.get(tid)
        if name is not None:
            return name
        external = self.external_names.get(tid)
        return external[1] if external else str(tid)

    # -- parsing ------------------------------------------------------------

    def parse(self):
        r = self.reader
        self.parse_header()
        self.parse_cpu_topology()
        r.skip(28)  # CrashEvent
        self.parse_frames()
        self.parse_strings()
        self.parse_source_locations()
        self.parse_locks()
        self.parse_messages()
        self.parse_zone_extra()
        self.parse_threads()
        self.parse_gpu()
        self.parse_plots()

    def parse_header(self):
        r = self.reader
        header = r.bytes(8)
        if header[:5] != b"tracy":
            raise SystemExit("not a tracy dump (bad inner header)")
        self.version = (header[5], header[6], header[7])
        if self.version < MIN_SUPPORTED_VERSION:
            raise SystemExit(f"trace version {self.version_string()} is too old to load")
        if self.version > MAX_SUPPORTED_VERSION:
            raise SystemExit(f"trace version {self.version_string()} is newer than supported")
        if self.version < (0, 12, 3):
            r.skip(8)  # m_delay

        r.i64()  # resolution
        r.f64()  # timerMul
        self.last_time = r.i64()
        r.u64()  # frameOffset
        self.pid = r.u64()
        r.i64()  # samplingPeriod
        r.u8()  # cpuArch
        r.u32()  # cpuId
        r.skip(12)  # cpuManufacturer
        if self.version >= (0, 9, 2):
            r.u8()  # onDemand

        self.capture_name = r.string()
        self.capture_program = r.string()
        r.u64()  # captureTime
        r.u64()  # executableTime
        self.host_info = r.string()

    def parse_cpu_topology(self):
        r = self.reader
        has_dies = self.version >= (0, 11, 2)
        for _ in range(r.u64()):
            r.u32()  # packageId
            for _ in range(r.u64()):
                if has_dies:
                    r.u32()  # dieId
                    for _ in range(r.u64()):
                        r.u32()  # coreId
                        r.skip(4 * r.u64())
                else:
                    r.u32()  # coreId
                    r.skip(4 * r.u64())

    def parse_frames(self):
        r = self.reader
        for _ in range(r.u64()):
            name_ptr = r.u64()
            continuous = r.u8()
            count = r.u64()
            frames = []
            ref = 0
            for _ in range(count):
                ref += r.i64()
                start = ref
                if continuous:
                    end = -1
                else:
                    ref += r.i64()
                    end = ref
                r.i32()  # frameImage
                frames.append((start, end))
            self.frame_sets.append((name_ptr, bool(continuous), frames))

    def parse_strings(self):
        r = self.reader
        count = r.u64()
        self.string_data = [None] * count
        for i in range(count):
            ptr = r.u64()
            text = r.bytes(r.u64()).decode("utf-8", "replace")
            self.string_data[i] = text
            self.string_by_ptr[ptr] = text

        for _ in range(r.u64()):
            ident, ptr = r.u64(), r.u64()
            if ptr in self.string_by_ptr:
                self.strings[ident] = self.string_by_ptr[ptr]

        for _ in range(r.u64()):
            ident, ptr = r.u64(), r.u64()
            if ptr in self.string_by_ptr:
                self.thread_names[ident] = self.string_by_ptr[ptr]

        for _ in range(r.u64()):
            ident, ptr, ptr2 = r.u64(), r.u64(), r.u64()
            if ptr in self.string_by_ptr and ptr2 in self.string_by_ptr:
                self.external_names[ident] = (self.string_by_ptr[ptr], self.string_by_ptr[ptr2])

        r.skip(8 * r.u64())  # localThreadCompress
        r.skip(8 * r.u64())  # externalThreadCompress

    def read_source_location(self):
        r = self.reader
        return SourceLocation(r.string_ref(), r.string_ref(), r.string_ref(), r.u32(), r.u32())

    def parse_source_locations(self):
        r = self.reader
        for _ in range(r.u64()):
            ptr = r.u64()
            self.source_locations[ptr] = self.read_source_location()

        count = r.u64()
        self.source_location_expand = list(struct.unpack_from(f"<{count}Q", r.data, r.pos))
        r.skip(8 * count)

        for _ in range(r.u64()):
            self.source_location_payload.append(self.read_source_location())

        r.skip(10 * r.u64())  # sourceLocationZones counts
        r.skip(10 * r.u64())  # gpuSourceLocationZones counts

    def parse_locks(self):
        """Locks are state machines rather than slices; consume without keeping."""
        r = self.reader
        for _ in range(r.u64()):
            r.skip(4 + 3 + 2 + 1 + 1 + 8 + 8)
            r.skip(8 * r.u64())
            r.skip(12 * r.u64())

    def parse_messages(self):
        r = self.reader
        has_metadata = self.version >= (0, 13, 2)
        ref = 0
        for _ in range(r.u64()):
            ptr = r.u64()
            ref += r.i64()
            text = r.string_ref()
            color = r.u32()
            r.int24()  # callstack
            if has_metadata:
                source, severity = r.u8(), r.u8()
            else:
                source, severity = 0, 2
            self.messages.append((ptr, ref, text, color, source, severity))

    def parse_zone_extra(self):
        r = self.reader
        count = r.u64()
        self.zone_extra = [None] * count
        for i in range(count):
            r.int24()  # callstack
            self.zone_extra[i] = (r.string_idx(), r.string_idx(), r.int24())

    def read_zone_timeline(self, count):
        """Flatten a zone tree into (start, end, srcloc, extra) tuples.

        Layout per zone, from Worker::WriteTimelineImpl: srcloc, start offset,
        extra index, child count, the children, then the end offset.
        """
        r = self.reader
        header = struct.Struct("<hqII")
        unpack = header.unpack_from
        data = r.data
        i64 = struct.Struct("<q").unpack_from

        zones = []
        append = zones.append
        ref = 0
        remaining = [count]
        pending = []
        pos = r.pos
        while remaining:
            if remaining[-1] == 0:
                remaining.pop()
                if pending:
                    (delta,) = i64(data, pos)
                    pos += 8
                    ref += delta
                    zones[pending.pop()][1] = ref
                continue
            remaining[-1] -= 1
            srcloc, start_delta, extra, child_count = unpack(data, pos)
            pos += 18
            ref += start_delta
            append([ref, 0, srcloc, extra])
            pending.append(len(zones) - 1)
            remaining.append(child_count)
        r.pos = pos
        return zones

    def parse_threads(self):
        r = self.reader
        has_group_hint = self.version >= (0, 11, 1)
        r.u64()  # total zone count
        r.u64()  # zoneChildren count
        for _ in range(r.u64()):
            tid = r.u64()
            count = r.u64()
            r.u64()  # kernelSampleCnt
            r.u8()  # isFiber
            if has_group_hint:
                r.i32()  # groupHint

            timeline_size = r.u32()
            zones = self.read_zone_timeline(timeline_size) if timeline_size else []
            if len(zones) != count:
                raise SystemExit(f"thread {tid}: recovered {len(zones)} zones but the capture records {count}")

            message_count = r.u64()
            messages = list(struct.unpack_from(f"<{message_count}Q", r.data, r.pos))
            r.skip(8 * message_count)

            r.skip(11 * r.u64())  # ctxSwitchSamples
            r.skip(11 * r.u64())  # samples

            self.threads.append((tid, zones, messages))

    def read_gpu_timeline(self, count, has_query_id):
        r = self.reader
        header = struct.Struct("<qqh3sHQ")
        unpack = header.unpack_from
        data = r.data
        trailer = struct.Struct("<qqH" if has_query_id else "<qq")
        unpack_trailer = trailer.unpack_from
        trailer_size = trailer.size

        zones = []
        append = zones.append
        ref_cpu = 0
        ref_gpu = 0
        remaining = [count]
        pending = []
        pos = r.pos
        while remaining:
            if remaining[-1] == 0:
                remaining.pop()
                if pending:
                    values = unpack_trailer(data, pos)
                    pos += trailer_size
                    ref_cpu += values[0]
                    ref_gpu += values[1]
                    zones[pending.pop()][1] = ref_gpu
                continue
            remaining[-1] -= 1
            cpu_delta, gpu_delta, srcloc, _, thread, child_count = unpack(data, pos)
            pos += 31
            ref_cpu += cpu_delta
            ref_gpu += gpu_delta
            append([ref_gpu, 0, srcloc, thread])
            pending.append(len(zones) - 1)
            remaining.append(child_count)
        r.pos = pos
        return zones

    def parse_gpu(self):
        r = self.reader
        has_notes = self.version >= (0, 12, 4)
        r.u64()  # total gpu zone count
        r.u64()  # gpuChildren count
        for _ in range(r.u64()):
            r.u64()  # thread
            r.u8()  # calibration
            count = r.u64()
            r.f32()  # period
            ctx_type = r.u8()
            name_idx = r.string_idx()
            r.u64()  # overflow
            if has_notes:
                r.skip(11 * r.u64())  # noteNames: int64 key + StringIdx value

            threads = []
            recovered = 0
            for _ in range(r.u64()):
                tid = r.u64()
                size = r.u64()
                zones = self.read_gpu_timeline(size, has_notes) if size else []
                recovered += len(zones)
                threads.append((tid, zones))
            if recovered != count:
                raise SystemExit(f"gpu context: recovered {recovered} zones but the capture records {count}")

            if has_notes:
                for _ in range(r.u64()):
                    r.u16()  # query id
                    r.skip(16 * r.u64())  # int64 note id + double value

            self.gpu_contexts.append((ctx_type, self.string_from_idx(name_idx), threads))

    def parse_plots(self):
        r = self.reader
        for _ in range(r.u64()):
            r.u8()  # type; memory plots are not written to the dump
            r.u8()  # format
            r.u8()  # showSteps
            r.u8()  # fill
            r.u32()  # color
            name_ptr = r.u64()
            r.f64()  # min
            r.f64()  # max
            r.f64()  # sum
            count = r.u64()
            values = struct.unpack_from("<" + "qd" * count, r.data, r.pos)
            r.skip(16 * count)
            points = []
            ref = 0
            for i in range(count):
                ref += values[i * 2]
                points.append((ref, values[i * 2 + 1]))
            self.plots.append((self.string_from_ptr(name_ptr), points))

    def version_string(self):
        return "{}.{}.{}".format(*self.version)


class TraceWriter:
    """Streams a Chrome JSON trace so the whole event list never lives in memory."""

    def __init__(self, path):
        self.file = open(path, "w")
        self.file.write('{"displayTimeUnit":"ns","traceEvents":[\n')
        self.first = True

    def emit(self, event):
        if self.first:
            self.first = False
        else:
            self.file.write(",\n")
        self.file.write(json.dumps(event, separators=(",", ":")))

    def close(self):
        self.file.write("\n]}\n")
        self.file.close()


def microseconds(nanoseconds):
    return round(nanoseconds / 1000.0, 3)


class ZoneDescriber:
    """Resolves a source location into a name and args, memoised per location."""

    def __init__(self, capture):
        self.capture = capture
        self.cache = {}

    def describe(self, srcloc):
        described = self.cache.get(srcloc)
        if described is None:
            capture = self.capture
            location = capture.source_location(srcloc)
            args = {}
            file = capture.string_from_ref(location.file)
            if file != "???":
                args["loc"] = f"{file}:{location.line}" if location.line else file
            function = capture.string_from_ref(location.function)
            if function != "???":
                args["function"] = function
            if location.color:
                args["color"] = f"#{location.color & 0xFFFFFF:06X}"
            described = (capture.zone_name(srcloc), args)
            self.cache[srcloc] = described
        return described

    def describe_zone(self, srcloc, extra_index):
        """Zone-level name/text/color overrides win over the source location."""
        name, args = self.describe(srcloc)
        if not extra_index:
            return name, args
        text_idx, name_idx, color = self.capture.zone_extra[extra_index]
        if name_idx is not None:
            name = self.capture.string_from_idx(name_idx)
        if text_idx is not None or color:
            args = dict(args)
            if text_idx is not None:
                args["text"] = self.capture.string_from_idx(text_idx)
            if color:
                args["color"] = f"#{color & 0xFFFFFF:06X}"
        return name, args


def convert(capture, writer):
    pid = capture.pid or 1
    describer = ZoneDescriber(capture)
    process_name = capture.capture_program or capture.capture_name or "tracy"

    writer.emit(
        {
            "ph": "M",
            "pid": pid,
            "tid": 0,
            "name": "process_name",
            "args": {"name": process_name},
        }
    )
    writer.emit({"ph": "M", "pid": pid, "tid": 0, "name": "process_sort_index", "args": {"sort_index": 0}})
    if capture.host_info:
        writer.emit(
            {
                "ph": "M",
                "pid": pid,
                "tid": 0,
                "name": "process_labels",
                "args": {"labels": capture.host_info.replace("\n", "; ").strip("; ")},
            }
        )

    used_ids = {tid for tid, _, _ in capture.threads}
    used_ids.update(tid for _, _, threads in capture.gpu_contexts for tid, _ in threads)
    used_ids.add(pid)
    next_tid = max(used_ids) + 1

    message_thread = {}
    for tid, _, message_ptrs in capture.threads:
        for ptr in message_ptrs:
            message_thread[ptr] = tid

    for tid, zones, _ in capture.threads:
        writer.emit(
            {
                "ph": "M",
                "pid": pid,
                "tid": tid,
                "name": "thread_name",
                "args": {"name": capture.thread_name(tid)},
            }
        )
        for start, end, srcloc, extra in zones:
            if end < start:
                end = max(capture.last_time, start)
            name, args = describer.describe_zone(srcloc, extra)
            event = {
                "ph": "X",
                "pid": pid,
                "tid": tid,
                "ts": microseconds(start),
                "dur": microseconds(end - start),
                "name": name,
            }
            if args:
                event["args"] = args
            writer.emit(event)

    orphan_tid = None
    for ptr, time, text, color, source, severity in capture.messages:
        tid = message_thread.get(ptr)
        if tid is None:
            if orphan_tid is None:
                orphan_tid = next_tid
                next_tid += 1
                writer.emit(
                    {
                        "ph": "M",
                        "pid": pid,
                        "tid": orphan_tid,
                        "name": "thread_name",
                        "args": {"name": "Messages"},
                    }
                )
            tid = orphan_tid
        args = {
            "source": MESSAGE_SOURCES[source] if source < len(MESSAGE_SOURCES) else source,
            "severity": MESSAGE_SEVERITIES[severity] if severity < len(MESSAGE_SEVERITIES) else severity,
        }
        if color:
            args["color"] = f"#{color & 0xFFFFFF:06X}"
        writer.emit(
            {
                "ph": "i",
                "s": "t",
                "pid": pid,
                "tid": tid,
                "ts": microseconds(time),
                "name": capture.string_from_ref(text),
                "args": args,
            }
        )

    for name_ptr, continuous, frames in capture.frame_sets:
        if not frames:
            continue
        tid = next_tid
        next_tid += 1
        name = capture.string_from_ptr(name_ptr) if name_ptr else "Frames"
        writer.emit({"ph": "M", "pid": pid, "tid": tid, "name": "thread_name", "args": {"name": name}})
        for position, (start, end) in enumerate(frames):
            if continuous:
                # A continuous frame runs until the next one starts, or, for the
                # final frame, until the end of the capture (Worker::GetFrameEnd).
                end = frames[position + 1][0] if position + 1 < len(frames) else capture.last_time
            if end < start:
                continue
            writer.emit(
                {
                    "ph": "X",
                    "pid": pid,
                    "tid": tid,
                    "ts": microseconds(start),
                    "dur": microseconds(end - start),
                    "name": f"{name} {position}",
                }
            )

    for name, points in capture.plots:
        for time, value in points:
            writer.emit(
                {
                    "ph": "C",
                    "pid": pid,
                    "tid": 0,
                    "ts": microseconds(time),
                    "name": name,
                    "args": {name: value},
                }
            )

    # GPU context pids must not collide with any thread id, or the Perfetto JSON
    # importer folds a same-numbered thread into the context's process.
    gpu_pid = next_tid
    for index, (ctx_type, ctx_name, threads) in enumerate(capture.gpu_contexts):
        gpu_pid += 1
        type_name = GPU_CONTEXT_TYPES[ctx_type] if ctx_type < len(GPU_CONTEXT_TYPES) else str(ctx_type)
        label = ctx_name or f"GPU context {index} ({type_name})"
        writer.emit({"ph": "M", "pid": gpu_pid, "tid": 0, "name": "process_name", "args": {"name": label}})
        writer.emit(
            {
                "ph": "M",
                "pid": gpu_pid,
                "tid": 0,
                "name": "process_sort_index",
                "args": {"sort_index": index + 1},
            }
        )
        for tid, zones in threads:
            writer.emit(
                {
                    "ph": "M",
                    "pid": gpu_pid,
                    "tid": tid,
                    "name": "thread_name",
                    "args": {"name": capture.thread_name(tid)},
                }
            )
            for start, end, srcloc, _ in zones:
                if end < start:
                    end = max(capture.last_time, start)
                name, args = describer.describe(srcloc)
                event = {
                    "ph": "X",
                    "pid": gpu_pid,
                    "tid": tid,
                    "ts": microseconds(start),
                    "dur": microseconds(end - start),
                    "name": name,
                }
                if args:
                    event["args"] = args
                writer.emit(event)


def main(argv):
    if len(argv) != 3:
        raise SystemExit("usage: tracy_to_perfetto.py <input.tracy> <output.json>")

    capture = TracyCapture(decompress(argv[1]))
    writer = TraceWriter(argv[2])
    try:
        convert(capture, writer)
    finally:
        writer.close()


if __name__ == "__main__":
    main(sys.argv)
