#!/usr/bin/env pythonimport os
import shlex
from dataclasses import dataclass, field
from typing import Optional

import lldb

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

FRAME_START: int = 0
FRAME_END: int = 12
AUTO_CONTINUE: bool = True

BREAKPOINT_FUNCTIONS: list[str] = [
    "tt::assert::detail::tt_throw",
]

# Junk substrings / patterns to strip from type names
_TYPE_JUNK: list[str] = [
    ", __gnu_cxx::_S_atomic",
    "__gnu_cxx::_S_atomic",
    "std::__cxx11::",
    "std::__shared_ptr_access<",
    "std::__shared_ptr<",
    "std::_Optional_payload_base<",
    "std::_Optional_base_impl<",
    ">::_Empty_byte",
]

MAX_DEPTH: int = 4
MAX_CHILDREN: int = 64


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass
class ValueNode:
    """Extracted representation of a single value / variable."""

    name: str
    type_name: str
    value: Optional[str] = None
    summary: Optional[str] = None
    error: Optional[str] = None
    children: list["ValueNode"] = field(default_factory=list)


@dataclass
class FrameInfo:
    """Extracted representation of a stack frame."""

    index: int
    function_name: str
    file_path: str
    line: int
    arguments: list[ValueNode] = field(default_factory=list)


@dataclass
class BreakpointReport:
    """Full report for one breakpoint hit."""

    pid: int
    tid: int
    frames: list[FrameInfo] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Type-name cleanup
# ---------------------------------------------------------------------------


def _clean_type_name(raw: str) -> str:
    """Remove well-known noisy substrings from a C++ type name."""
    result: str = raw
    for junk in _TYPE_JUNK:
        result = result.replace(junk, "")
    # Collapse multiple spaces / trailing commas inside angle brackets
    while "  " in result:
        result = result.replace("  ", " ")
    result = result.replace(", >", ">").replace(",>", ">")
    return result.strip()


# ---------------------------------------------------------------------------
# Expression evaluation helper
# ---------------------------------------------------------------------------


def _eval(frame: lldb.SBFrame, expr_text: str) -> lldb.SBValue:
    opts = lldb.SBExpressionOptions()
    opts.SetIgnoreBreakpoints(True)
    opts.SetFetchDynamicValue(lldb.eDynamicCanRunTarget)
    opts.SetTimeoutInMicroSeconds(5_000_000)
    opts.SetTryAllThreads(True)
    return frame.EvaluateExpression(expr_text, opts)


# ---------------------------------------------------------------------------
# Synthetic / libcxx-aware child iteration
# ---------------------------------------------------------------------------


def _has_synthetic_children(val: lldb.SBValue) -> bool:
    """Return True when LLDB provides a synthetic children provider for *val*."""
    synth: lldb.SBValue = val.GetDynamicValue(lldb.eDynamicCanRunTarget)
    if synth.IsValid() and synth.MightHaveChildren():
        return True
    # Check if the value itself already exposes synthetic children
    if val.IsSynthetic() or val.MightHaveChildren():
        num = val.GetNumChildren()
        if num > 0:
            return True
    return False


def _iterate_children(val: lldb.SBValue) -> list[lldb.SBValue]:
    """
    Yield children using synthetic provider when available, otherwise raw children.
    """
    # Prefer the synthetic representation (honours libcxx formatters)
    synth: lldb.SBValue = val.GetDynamicValue(lldb.eDynamicCanRunTarget)
    source: lldb.SBValue = synth if synth.IsValid() and synth.MightHaveChildren() else val

    n: int = min(source.GetNumChildren(), MAX_CHILDREN)
    children: list[lldb.SBValue] = []
    for i in range(n):
        child: lldb.SBValue = source.GetChildAtIndex(i)
        if child.IsValid():
            children.append(child)
    return children


# ---------------------------------------------------------------------------
# Data extraction (SBValue → ValueNode)
# ---------------------------------------------------------------------------


def _extract_value(val: lldb.SBValue, depth: int = 0) -> ValueNode:
    """Recursively convert an SBValue tree into a ValueNode tree."""
    if not val or not val.IsValid():
        return ValueNode(name="<invalid>", type_name="", error="invalid SBValue")

    name: str = val.GetName() or ""
    type_name: str = _clean_type_name(val.GetTypeName() or "")
    raw_value: Optional[str] = val.GetValue()
    summary: Optional[str] = val.GetSummary()
    error_msg: Optional[str] = None

    err: lldb.SBError = val.GetError()
    if err.Fail():
        error_msg = err.GetCString()

    node = ValueNode(
        name=name,
        type_name=type_name,
        value=raw_value,
        summary=summary,
        error=error_msg,
    )

    if depth >= MAX_DEPTH:
        return node

    children_vals: list[lldb.SBValue] = _iterate_children(val)
    for child in children_vals:
        node.children.append(_extract_value(child, depth + 1))

    return node


def _extract_value_via_eval(
    frame: lldb.SBFrame, arg: lldb.SBValue, depth: int = 0
) -> ValueNode:
    """
    Try to re-evaluate the variable through the expression evaluator so that
    synthetic providers and formatters are active, then fall back to the raw
    SBValue.
    """
    name: str = arg.GetName() or ""
    if name:
        ev: lldb.SBValue = _eval(frame, name)
        if ev.IsValid() and ev.GetError().Success():
            node = _extract_value(ev, depth)
            # Preserve the original source name
            node.name = name
            return node

    return _extract_value(arg, depth)


# ---------------------------------------------------------------------------
# Shape synthetic provider
# ---------------------------------------------------------------------------


def _try_shape_extraction(
    frame: lldb.SBFrame, arg_name: str, node: ValueNode
) -> bool:
    """
    If *arg_name* refers to a ``tt::tt_metal::Shape`` (or reference/pointer
    to it), call ``.view()`` to get a ``Span<const uint32_t>`` and replace
    the children with the formatted span contents.

    Returns True if the replacement was performed.
    """
    # Quick heuristic: check if the type looks like Shape
    if "tt::tt_metal::Shape" not in node.type_name and "ShapeBase" not in node.type_name:
        return False

    expr: str = f"({arg_name}).view()"
    span_val: lldb.SBValue = _eval(frame, expr)
    if not span_val.IsValid() or span_val.GetError().Fail():
        err_str: str = (
            span_val.GetError().GetCString()
            if span_val.IsValid()
            else "evaluation returned invalid SBValue"
        )
        node.children = [
            ValueNode(
                name=".view()",
                type_name="",
                error=f"failed to call .view(): {err_str}",
            )
        ]
        return True

    # The span should have pointer + size; try to read elements
    size_expr: str = f"({arg_name}).view().size()"
    size_val: lldb.SBValue = _eval(frame, size_expr)
    if not size_val.IsValid() or size_val.GetError().Fail():
        node.children = [_extract_value(span_val)]
        return True

    count: int = int(size_val.GetValueAsUnsigned(0))
    elements: list[ValueNode] = []
    for i in range(min(count, MAX_CHILDREN)):
        elem_expr: str = f"({arg_name}).view()[{i}]"
        elem_val: lldb.SBValue = _eval(frame, elem_expr)
        if elem_val.IsValid() and elem_val.GetError().Success():
            elements.append(
                ValueNode(
                    name=f"[{i}]",
                    type_name=_clean_type_name(elem_val.GetTypeName() or "uint32_t"),
                    value=elem_val.GetValue(),
                    summary=elem_val.GetSummary(),
                )
            )
        else:
            err_str = (
                elem_val.GetError().GetCString()
                if elem_val.IsValid()
                else "invalid"
            )
            elements.append(
                ValueNode(
                    name=f"[{i}]",
                    type_name="uint32_t",
                    error=f"read error: {err_str}",
                )
            )

    node.children = [
        ValueNode(
            name=".view()",
            type_name=f"Span<const uint32_t> (size={count})",
            children=elements,
        )
    ]
    return True


# ---------------------------------------------------------------------------
# Optional value extraction
# ---------------------------------------------------------------------------


def _try_optional_extraction(
    frame: lldb.SBFrame, arg_name: str, node: ValueNode
) -> bool:
    """
    If *node* looks like an ``std::optional`` that has a value, call
    ``*name`` (dereference) to obtain the contained value.
    """
    if "optional" not in node.type_name.lower():
        return False

    has_val_expr: str = f"({arg_name}).has_value()"
    hv: lldb.SBValue = _eval(frame, has_val_expr)
    if not hv.IsValid() or hv.GetError().Fail():
        return False

    has_value: bool = hv.GetValueAsUnsigned(0) != 0
    if not has_value:
        node.summary = "nullopt"
        node.children = []
        return True

    val_expr: str = f"*({arg_name})"
    contained: lldb.SBValue = _eval(frame, val_expr)
    if contained.IsValid() and contained.GetError().Success():
        child_node: ValueNode = _extract_value(contained, depth=1)
        child_node.name = "value"
        node.children = [child_node]
        node.summary = "Has Value=true"
        return True
    else:
        err_str: str = (
            contained.GetError().GetCString()
            if contained.IsValid()
            else "invalid"
        )
        node.children = [
            ValueNode(name="value", type_name="", error=f"dereference failed: {err_str}")
        ]
        node.summary = "Has Value=true (extraction error)"
        return True


# ---------------------------------------------------------------------------
# Frame / report extraction
# ---------------------------------------------------------------------------


def _extract_frame(frame: lldb.SBFrame, index: int) -> FrameInfo:
    fn_name: str = frame.GetDisplayFunctionName() or frame.GetFunctionName() or "<unknown>"
    le: lldb.SBLineEntry = frame.GetLineEntry()
    if le.IsValid():
        fspec: lldb.SBFileSpec = le.GetFileSpec()
        file_path: str = os.path.join(fspec.GetDirectory() or "", fspec.GetFilename() or "")
        line: int = le.GetLine()
    else:
        file_path = "<no line info>"
        line = 0

    info = FrameInfo(index=index, function_name=fn_name, file_path=file_path, line=line)

    vars_: lldb.SBValueList = frame.GetVariables(True, False, False, True)
    for i in range(vars_.GetSize()):
        arg: lldb.SBValue = vars_.GetValueAtIndex(i)
        node: ValueNode = _extract_value_via_eval(frame, arg)

        arg_name: str = arg.GetName() or ""

        # Enrich optionals
        _try_optional_extraction(frame, arg_name, node)

        # Walk children and try enrichment on each
        _enrich_children(frame, arg_name, node)

        # Top-level shape handling
        _try_shape_extraction(frame, arg_name, node)

        info.arguments.append(node)

    return info


def _enrich_children(
    frame: lldb.SBFrame, parent_expr: str, node: ValueNode, depth: int = 0
) -> None:
    """Recursively try to enrich optional / shape children."""
    if depth > MAX_DEPTH:
        return
    for child in node.children:
        child_expr: str = f"({parent_expr}).{child.name}" if parent_expr and child.name else ""
        if child_expr:
            _try_optional_extraction(frame, child_expr, child)
            _try_shape_extraction(frame, child_expr, child)
        _enrich_children(frame, child_expr, child, depth + 1)


def _extract_report(frame: lldb.SBFrame) -> BreakpointReport:
    thread: lldb.SBThread = frame.GetThread()
    process: lldb.SBProcess = thread.GetProcess()
    report = BreakpointReport(
        pid=process.GetProcessID(),
        tid=thread.GetThreadID(),
    )

    end: int = min(FRAME_END, thread.GetNumFrames())
    for i in range(max(0, FRAME_START), end):
        f: lldb.SBFrame = thread.GetFrameAtIndex(i)
        report.frames.append(_extract_frame(f, i))

    return report


# ---------------------------------------------------------------------------
# Formatting (ValueNode → str)
# ---------------------------------------------------------------------------


def _format_node(node: ValueNode, base_indent: str, depth: int) -> list[str]:
    """Return lines representing *node* at the given *depth*."""
    indent: str = base_indent * depth
    parts: list[str] = []

    header_parts: list[str] = []
    if node.name:
        header_parts.append(node.name)
    if node.type_name:
        header_parts.append(f": {node.type_name}")
    if node.value is not None:
        header_parts.append(f" = {node.value}")
    if node.summary is not None:
        header_parts.append(f" {node.summary}")

    header: str = "".join(header_parts) if header_parts else "<unavailable>"
    parts.append(f"{indent}{header}")

    if node.error:
        error_lines: list[str] = node.error.splitlines()
        child_indent: str = base_indent * (depth + 1)
        parts.append(f"{child_indent}[ERROR] {error_lines[0]}")
        for eline in error_lines[1:]:
            parts.append(f"{child_indent}        {eline}")

    for child in node.children:
        parts.extend(_format_node(child, base_indent, depth + 1))

    return parts


def _format_value_tree(node: ValueNode, base_indent: str = "    ") -> str:
    return "\n".join(_format_node(node, base_indent, 1))


def _format_report(report: BreakpointReport) -> str:
    lines: list[str] = [
        "",
        "=== Breakpoint hit ===",
        f"pid={report.pid} tid={report.tid}",
    ]

    for fi in report.frames:
        lines.append("")
        lines.append(f"frame #{fi.index}: {fi.function_name} @ {fi.file_path}:{fi.line}")
        if not fi.arguments:
            lines.append("    <no arguments>")
        else:
            for arg_node in fi.arguments:
                lines.append(_format_value_tree(arg_node))

    lines.append("")
    lines.append("=== end ===")
    lines.append("")
    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Breakpoint callback
# ---------------------------------------------------------------------------


def _bp_callback(frame: lldb.SBFrame, bp_loc: lldb.SBBreakpointLocation, _dict: dict) -> bool:
    try:
        report: BreakpointReport = _extract_report(frame)
        print(_format_report(report))
    except Exception as exc:
        print(f"\n[lldb-script ERROR] {exc}\n")

    if AUTO_CONTINUE:
        frame.GetThread().GetProcess().Continue()
        return True
    return False


# ---------------------------------------------------------------------------
# Synthetic providers
# ---------------------------------------------------------------------------


class TtslSpanSyntheticProvider:
    """
    Synthetic children provider for ttsl::Span<T>, which is a type alias
    for std::span<T>. Reads _M_ptr and _M_extent._M_extent_value to
    present the span as an array of elements.
    """

    def __init__(self, valobj: lldb.SBValue, internal_dict: dict) -> None:
        self.valobj: lldb.SBValue = valobj
        self.count: int = 0
        self.element_type: Optional[lldb.SBType] = None
        self.element_size: int = 0
        self.data_ptr: int = 0

    def num_children(self) -> int:
        return self.count

    def get_child_index(self, name: str) -> int:
        try:
            if name.startswith("[") and name.endswith("]"):
                return int(name[1:-1])
        except ValueError:
            pass
        return -1

    def get_child_at_index(self, index: int) -> Optional[lldb.SBValue]:
        if index < 0 or index >= self.count:
            return None
        if self.element_type is None:
            return None

        offset: int = index * self.element_size
        addr: int = self.data_ptr + offset
        return self.valobj.CreateValueFromAddress(
            f"[{index}]",
            addr,
            self.element_type,
        )

    def update(self) -> None:
        self.count = 0
        self.data_ptr = 0
        self.element_type = None
        self.element_size = 0

        # _M_ptr holds the pointer to the data
        ptr_val: lldb.SBValue = self.valobj.GetChildMemberWithName("_M_ptr")
        if not ptr_val.IsValid():
            return

        self.data_ptr = ptr_val.GetValueAsUnsigned(0)
        if self.data_ptr == 0:
            return

        # Determine element type from the pointer type
        ptr_type: lldb.SBType = ptr_val.GetType()
        if ptr_type.IsPointerType():
            self.element_type = ptr_type.GetPointeeType()
            self.element_size = self.element_type.GetByteSize()

        # _M_extent is std::__detail::__extent_storage<...> with _M_extent_value
        extent_member: lldb.SBValue = self.valobj.GetChildMemberWithName("_M_extent")
        if extent_member.IsValid():
            extent_val: lldb.SBValue = extent_member.GetChildMemberWithName("_M_extent_value")
            if extent_val.IsValid():
                self.count = int(extent_val.GetValueAsUnsigned(0))
        else:
            # Some implementations store it directly
            extent_val = self.valobj.GetChildMemberWithName("_M_extent_value")
            if extent_val.IsValid():
                self.count = int(extent_val.GetValueAsUnsigned(0))

        # Clamp to avoid runaway
        if self.count > MAX_CHILDREN:
            self.count = MAX_CHILDREN

    def has_children(self) -> bool:
        return self.count > 0


def _ttsl_span_summary(valobj: lldb.SBValue, internal_dict: dict) -> str:
    """Summary string for ttsl::Span — shows size and element values inline."""
    ptr_val: lldb.SBValue = valobj.GetChildMemberWithName("_M_ptr")
    extent_member: lldb.SBValue = valobj.GetChildMemberWithName("_M_extent")
    count: int = 0
    if extent_member.IsValid():
        ev: lldb.SBValue = extent_member.GetChildMemberWithName("_M_extent_value")
        if ev.IsValid():
            count = int(ev.GetValueAsUnsigned(0))

    if count == 0:
        return "size=0 {}"

    # Read up to 8 elements inline for the summary
    data_ptr: int = ptr_val.GetValueAsUnsigned(0) if ptr_val.IsValid() else 0
    if data_ptr == 0:
        return f"size={count} {{...}}"

    ptr_type: lldb.SBType = ptr_val.GetType()
    if not ptr_type.IsPointerType():
        return f"size={count} {{...}}"

    elem_type: lldb.SBType = ptr_type.GetPointeeType()
    elem_size: int = elem_type.GetByteSize()

    max_inline: int = min(count, 8)
    elems: list[str] = []
    for i in range(max_inline):
        addr: int = data_ptr + i * elem_size
        child: lldb.SBValue = valobj.CreateValueFromAddress(f"[{i}]", addr, elem_type)
        if child.IsValid():
            v: Optional[str] = child.GetValue()
            elems.append(v if v else "?")
        else:
            elems.append("?")

    suffix: str = ", ..." if count > max_inline else ""
    return f"size={count} {{{', '.join(elems)}{suffix}}}"


# ---------------------------------------------------------------------------
# Module initialisation
# ---------------------------------------------------------------------------

def _register_synthetic_providers(debugger: lldb.SBDebugger) -> None:
    """Register custom type synthetic providers and summaries."""
    # The regex matches ttsl::Span<...> regardless of template parameter
    debugger.HandleCommand(
        f'type synthetic add -x "^ttsl::Span<.+>$" '
        f'--python-class {__name__}.TtslSpanSyntheticProvider'
    )
    debugger.HandleCommand(
        f'type summary add -x "^ttsl::Span<.+>$" '
        f'--python-function {__name__}._ttsl_span_summary'
    )
    # Also catch the underlying std::span if it shows up with the ttsl alias resolved
    debugger.HandleCommand(
        f'type synthetic add -x "^std::span<.+>$" '
        f'--python-class {__name__}.TtslSpanSyntheticProvider'
    )
    debugger.HandleCommand(
        f'type summary add -x "^std::span<.+>$" '
        f'--python-function {__name__}._ttsl_span_summary'
    )

def __lldb_init_module(debugger: lldb.SBDebugger, internal_dict: dict) -> None:
    # Import libcxx formatters so synthetic providers are available
    debugger.HandleCommand("command script import lldb.formatters")
    debugger.HandleCommand("command script import lldb.formatters.cpp")

    _register_synthetic_providers(debugger)

    python_path: Optional[str] = os.environ.get("LLDB_PYTHON_PATH")
    pytest_target: Optional[str] = os.environ.get("LLDB_PYTEST_TARGET")

    if not python_path:
        print("[lldb-script] LLDB_PYTHON_PATH is not set")
        return
    if not pytest_target:
        print("[lldb-script] LLDB_PYTEST_TARGET is not set")
        return

    target: lldb.SBTarget = debugger.CreateTarget(python_path)
    if not target or not target.IsValid():
        print(f"[lldb-script] Failed to create target for {python_path}")
        return

    for func_name in BREAKPOINT_FUNCTIONS:
        bp: lldb.SBBreakpoint = target.BreakpointCreateByName(func_name)
        if not bp.IsValid():
            print(f"[lldb-script] WARNING: could not set breakpoint on {func_name}")
            continue
        bp.SetScriptCallbackFunction(f"{__name__}._bp_callback")
        print(f"[lldb-script] Breakpoint set on {func_name}")

    print(f"[lldb-script] Frame range: [{FRAME_START}, {FRAME_END})")

    launch_cmd: str = "process launch -- " + " ".join(
        shlex.quote(x) for x in ["-m", "pytest", pytest_target]
    )
    debugger.HandleCommand(launch_cmd)
