import
  ./emacs_sugar,
  ./emacs_funcall,
  ./emacs_api

export emacs_sugar, emacs_api, emacs_funcall

import std/[compilesettings]

template emInit*(body: untyped): untyped =
  proc init(runtime {.inject.}: ptr EmRuntime): cint {.
    exportc: "emacs_module_init", cdecl, dynlib.} =
    var env {.inject.}: EmEnv = runtime.getEnvironment(runtime)

    assert not isNil(env)
    bindAllEmcall(env)

    try:
      body
      discard env.funcall("provide", @[
        env.intern(querySetting(projectName))])

    except:
      env.emPassError()

    return 0

template emInit*(): untyped =
  emInit():
    discard
