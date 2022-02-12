import ./emacs
import std/[
  macros,
  os,
  strutils,
  osproc,
  times,
  dynlib,
  tables
]

var loaded: Table[string, LibHandle]

var subEnv: EmEnv

proc loadFile(env: EmEnv, file: string, recompile: bool = true): bool {.
  emcall: "load-file".} =
  subEnv = env
  setEnv(env)
  env.emLog("loading file", file)
  var nimFile = file
  if not isAbsolute(nimFile):
    for path in env.items(env.symVal("load-path")):
      let path = path as string
      if fileExists(path / file):
        nimFile = path / file
        break

  env.emLog("found ", nimFile)
  assert fileExists(nimFile)
  let (path, name, ext) = splitFile(nimFile)
  let soname = path / name & ".so"
  if not fileExists(soname) or
     getLastModificationTime(soname) < getLastModificationTime(nimFile):
    let res = execCmd(
      "nim c --passc=-g --app=lib -o='$#' '$#'" % [soname, nimFile])

    if res != 0:
      env.emError("compilation of the ", nimFile, " failed")
      return false

  let lib = loadLib(soname)
  if isNil(lib):
    env.emError("failed to load the library")
    return false

  loaded[soname] = lib
  let initf = cast[proc(r: ptr EmRuntime): cint {.cdecl.}](
    lib.symAddr("emacs_module_init"))

  if isNil(initf):
    return false

  var r: EmRuntime
  r.size = sizeof(r).uint()
  r.getEnvironment = proc(r: ptr EmRuntime): EmEnv {.cdecl.} =
    subEnv

  if initf(addr r) == 0:
    env.emLog("Initialized module", name)
    return true

  else:
    return false


emInit():
  echo "initialized module loader"
