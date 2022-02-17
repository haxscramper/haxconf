import ./emacs

proc returnValue(val: int, other: int = 12): int {.emcall: "bind".} =
  result = val + other

proc returnValue1(val: int): int {.emcalln.} =
  result = 123

proc storeFunction(env: EmEnv, val: EmValue): int {.emcall: "store-function".} =
  echo env.treeRepr(env.symVal(val))

proc pointMax(env: EmEnv): int {.embind: "point-max".}
proc pointMin(env: EmEnv): int {.embind: "point-min".}
proc makeMarker(env: EmEnv): EmMarker {.embind: "make-marker".}
proc markerPosition(env: EmEnv, marker: EmMarker): OrNil[int] {.
  embind: "marker-position".}

emInit():
  echo "initalized emacs"

  env.defun("test", 0..0, "doc"):
    return env.toEmacs(123)

  echo "point-max: ", env.pointMax()
  echo "from emacs: ", env.pointMin()
