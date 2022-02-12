import
  ./emacs_api

import std/[macrocache, macros, strutils]


proc funcall*[Args](
    env: EmEnv, name: string, args: Args,
    checkErr: bool = true
  ): EmValue {.discardable.} =

  var emArgs: seq[EmValue]
  for name, value in fieldPairs(args):
    emArgs.add env.toEmacs(value)

  return env.funcall(name, emArgs, checkErr = checkErr)

type
  EmProcData* = object
    minArity*: int
    maxArity*: int
    name*: string
    docstring*: string
    impl*: EmProc
    data*: pointer

proc defun*(env: EmEnv, impl: EmProcData) =
  discard env.funcall("defalias", @[
    env.intern(impl.name),
    env.makeFunction(
      env,
      impl.minArity.uint,
      impl.maxArity.uint,
      impl.impl,
      impl.docstring.cstring,
      impl.data
    )
  ])

template defun*(
    nowEnv: EmEnv,
    procName: string,
    argLen: Slice[int],
    doc: string,
    body: untyped
  ): untyped =

  block:
    proc implProc(
        env {.inject.}: EmEnv,
        nargs {.inject.}: uint,
        args {.inject.}: ptr EmValue
      ): EmValue {.closure.} =

      body

    let impl = EmProcData(
      name: procName,
      docstring: doc,
      minArity: argLen.a,
      maxArity: argLen.b,
      impl: cast[EmProc](rawProc(implProc)),
      data: rawEnv(implProc)
    )

    nowEnv.defun(impl)

const
  emcallNamespace {.strdefine.}: string = ""

template emPassError*(env: EmEnv): untyped =
  {.line: instantiationInfo(fullPaths = true).}:
    let ex = getCurrentException()
    let trace = ex.getStackTrace()
    let msg = getCurrentExceptionMsg()
    discard env.funcallRaw(
      env.intern("error"), @[env.emVal(
        "Nim exception: $# - $#\n$#" % [$ex.name, $ex.msg, trace]
      )])

const emcallNames = CacheSeq"emcallNames"

proc emcallImpl*(bindpatt: string, impl: NimNode): NimNode =

  let
    wrapName = genSym(ident = impl.name().strVal() & "Emcall")
    wrapImpl = ident(impl.name().strVal() & "Emprox")
    nargs = ident("nargs")
    args = ident("args")
    env = ident("env")

  emcallNames.add wrapName

  var data: EmProcData
  if 0 < len(emcallNamespace):
    data.name = emcallNamespace & ":"

  data.name.add bindpatt % [impl.name().strVal()]

  data.maxArity = impl.params().len() - 1
  let implName = impl.name().toStrLit()

  var
    implRepack = newStmtList()
    recall = newCall(impl.name())
    count = 0

  let info = newLit(impl.lineInfoObj())

  if impl.params()[1][1].eqIdent("EmEnv"):
    count = -1

  for arg in impl.params()[1..^1]:
    let typ = arg[^2]
    for name in arg[0..^3]:
      var pass = ident(name.strVal())
      let argname = name.toStrLit()
      var init: NimNode = newCall("default", typ)
      if arg[^1].kind == nnkEmpty:
        if not typ.eqIdent("EmEnv"):
          inc data.minArity

      else:
        init = arg[^1]

      if typ.eqIdent("EmEnv"):
        discard

      else:
        implRepack.add quote do:
          var `pass`: `typ` = default(`typ`)
          if `count` < `nargs`:
            if expectValid(
              `env`,
              `args`[`count`],
              `pass`,
              desc = (
                "Argument '" & `argname` & "' for proc '" & `implName` & "' " &
                  "defined in " & $`info`
              )
            ):
              fromEmacs(`env`, `pass`, `args`[`count`], check = false)

          else:
            `pass` = `init`

      recall.add pass
      inc count

  let callConv =
    if impl.params()[0].kind == nnkEmpty:
      quote do:
        `recall`
        result = `env`.intern("nil")

    else:
      quote do:
        result = toEmacs(`env`, `recall`)

  result = quote do:
    `impl`

    let `wrapImpl` = proc(
      `env`: EmEnv,
      `nargs`: uint,
      `args`: ptr UncheckedArray[EmValue]
    ): EmValue {.closure.} =
      `implRepack`
      try:
        `callConv`

      except:
        emPassError(`env`)

    let `wrapName` =
      block:
        var base = `data`
        base.impl = cast[EmProc](rawProc(`wrapImpl`))
        base.data = rawEnv(`wrapImpl`)

        base

  # echo result.repr()

macro emcall*(bindname: static[string], impl: untyped): untyped =
  result = emcallImpl(bindname, impl)

macro emcalln*(body: untyped): untyped =
  result = emcallImpl("$1", body)

macro embind*(name: static[string], body: untyped): untyped =
  let env = body.params()[1][0]
  if body.params()[1][1].strVal() != "EmEnv":
    error(
      "Expected `EmEnv` as a first argument for embind target proc",
      body)

  var pass = nnkTupleConstr.newTree()
  for arg in body.params()[2..^1]:
    for name in arg[0..^3]:
      pass.add name

  result = body
  let fcall =
    if len(pass) == 0:
      newCall("funcall", env, newLit(name))

    else:
      newCall("funcall", env, newLit(name), pass)

  result.body = newCall(nnkBracketExpr.newTree(
    ident"fromEmacs",
    body.params()[0],
  ), env, fcall)

type
  EmDefun*[Args, Ret] = object
    name*: string

  EmVar*[T] = object
    name*: string

func getvar*[T](name: string): EmVar[T] = EmVar[T](name: name)

func getfun*[Args, Ret](
    name: string, args: typedesc[Args], ret: typedesc[Ret]
  ): EmDefun[Args, Ret] =

  result.name = name


proc funcall*[Args, Ret](
    env: EmEnv, fun: EmDefun[Args, Ret],
    args: Args,
    checkErr: bool = true): Ret =
  env.fromEmacs(result, env.funcall(fun.name, args, checkErr = checkErr))


proc funcall*[Args, Ret](
    env: EmEnv, fun: EmDefun[Args, Ret],
    checkErr: bool = true): Ret =
  env.fromEmacs(result, env.funcall(fun.name, @[], checkErr = checkErr))

macro bindAllEmcall*(env: EmEnv): untyped =
  result = newStmtList()
  for name in emcallNames:
    result.add newCall("defun", env, name)
