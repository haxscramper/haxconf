import std/[re, os, times, math, macros, strformat, strutils]
import ./emacs

const newDayAfter = 5
const logMinDelay = 8

const testing = true
proc dlog(args: varargs[string, `$`]): void =
  when testing: echo args.join(" ")
  else: discard

proc getWeekNum(time: DateTime): int =
  ceil(
    (
      getDayOfYear(
        monthday = time.monthday,
        month = time.month,
        year = time.year) +
      getDayOfWeek(1, mJan, time.year).ord
    ) / 7
  ).toInt()


type
  NoteType = enum
    ntDaily = "daily"
    ntWeekly = "weekly"
    ntMonthly = "monthly"

proc getCurrentNote(fileDirectory: string, ntype: NoteType = ntDaily): string =
  ## Return path to current daily note
  var time = now()
  result = fileDirectory.joinPath(
    case ntype:
      of ntDaily: time.format("yyyy-MM-dd") & ".org"
      of ntWeekly: time.format("yyyy") &
        "-W" &
        ($time.getWeekNum()).align(2, '0') & ".org"
      of ntMonthly: time.format("yyyy-MM") & ".org"
  )

proc noteAppendRequired(note: string): bool =
  var lastHour = 0
  var lastMinute = 0
  for line in note.readFile().split("\n"):
    if line =~ re"^\*\* @time:(\d\d):(\d\d);":
      lastHour = matches[0].parseInt()
      lastMinute = matches[1].parseInt()

  proc getSince0000(hour, minute: int): int =
    if hour < newDayAfter:
      minute + (24 + hour) * 60
    else:
      minute + hour * 60

  let now0000 = getSince0000(now().hour, now().minute)
  let file0000 = getSince0000(lastHour, lastMinute)

  return now0000 - file0000 > logMinDelay or (
    lastHour == 0 and lastMinute == 0)

proc getTimeStampNow(): string =
  "@time:" & now().format("HH:mm") & ";"

proc thisWeekDays(): seq[string] =
  let curr = now()
  for day in WeekDay:
    let nowIdx = cast[int](curr.weekday)
    let dayIdx = cast[int](day)
    let diff = dayIdx - nowIdx
    let interval = TimeInterval(days: abs(diff))
    let res =
      if diff > 0: curr + interval
      else: curr - interval

    result.add res.format("yyyy-MM-dd dddd")

proc addNewLog(note: string): void =
  let file = note.open(fmAppend)
  file.write("\n** " & getTimeStampNow() & "\n\n\n")
  file.close()

proc dailyNode(): string =
  result.addf(
    """
#+TITLE: @date: $1; $2

* TODO Tasks [/]
  DEADLINE: <$3>
** TODO {{{replace-target}}}

* Logs

** $2

""",
    now().format("yyyy-MM-dd"),
    getTimeStampNow(),
    now().format("yyyy-MM-dd ddd") & " 23:55"
  )

proc weeklyNote(): string =
  return &"""
#+title: Weekly note N{now().getWeekNum()}

{thisWeekDays().join("\n")}
"""

proc monthlyNote(): string =
  &"""
#+title: Monthly note for {now().format("MMMM yyyy")}
"""

proc fileIsEmpty(note: string): bool =
  note.readFile().len == 0

# let fileDirectory =
#   if "file-dir".kp:
#     let dir = "file-dir".k.toStr()
#     if dir.endsWith("/"): dir[0..^2]
#     else: dir
#   else:
#     getHomeDir() &
#       ".config/hax-local/dirs/personal/notes/" &
#       (
#         case ntype:
#           of ntDaily: "daily/"
#           of ntWeekly: "weekly/"
#           of ntMonthly: "monthly/"
#       )

# if "modify-file".kp:
#   if not fileExists(note) or note.fileIsEmpty():
#     createNewNote(note, ntype)

#   if ntype == ntDaily and noteAppendRequired(note):
#     addNewLog(note)

# if "update-symlink".kp:
#   removeFile(fileDirectory & "/today.org")
#   createSymlink(src = note, dest = fileDirectory & "/today.org")

proc dailyNote(sym: NoteType) {.emcall: "open-daily-note".} =
  echo sym
  echo "has daily note"

emInit()
