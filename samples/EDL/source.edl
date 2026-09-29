## EDL source tracking: files, positions, spans and line lookup.
##
## This module belongs to the EDL compiler frontend. It knows nothing about
## the bootstrap substrate: `SourceFile` is the EDL representation of a
## compilation unit.
##
## Bootstrap dialect (see specs/decisions/ADR-0001): plain procs, objects and
## enums only. No macros, no templates, no custom pragmas -- so that the
## bootstrap-to-EDL migrator can translate this file without handling macro
## semantics.

type
  SourceFile* = ref object
    ## One EDL source file held in memory.
    path*: string
    text*: string
    lineStarts*: seq[int]

  SourcePos* = object
    ## A human-facing position. `line` and `col` are 1-based, `offset` is a
    ## 0-based index into `SourceFile.text`.
    line*: int
    col*: int
    offset*: int

  SourceSpan* = object
    ## Half-open byte range `[start, stop)` inside `file`.
    file*: SourceFile
    start*: SourcePos
    stop*: SourcePos

proc newSourceFile*(path, text: string): SourceFile =
  ## Builds a source file and precomputes line start offsets.
  result = SourceFile(path: path, text: text)
  result.lineStarts = @[0]
  var i = 0
  while i < text.len:
    if text[i] == '\n':
      result.lineStarts.add(i + 1)
    inc i

proc posAt*(f: SourceFile, offset: int): SourcePos =
  ## Converts a byte offset into a line/column position.
  if f == nil:
    return
  var target = offset
  if target < 0:
    target = 0
  if target > f.text.len:
    target = f.text.len
  var lo = 0
  var hi = f.lineStarts.len - 1
  while lo < hi:
    let mid = (lo + hi + 1) div 2
    if f.lineStarts[mid] <= target:
      lo = mid
    else:
      hi = mid - 1
  result.line = lo + 1
  result.col = target - f.lineStarts[lo] + 1
  result.offset = target

proc lineText*(f: SourceFile, line: int): string =
  ## Returns the text of `line` (1-based) without its line terminator.
  if f == nil:
    return ""
  let idx = line - 1
  if idx < 0 or idx >= f.lineStarts.len:
    return ""
  let startOff = f.lineStarts[idx]
  var stopOff = f.text.len
  if idx + 1 < f.lineStarts.len:
    stopOff = f.lineStarts[idx + 1] - 1
  if stopOff > startOff and f.text[stopOff - 1] == '\r':
    dec stopOff
  if stopOff < startOff:
    stopOff = startOff
  result = f.text[startOff ..< stopOff]

proc newSpan*(f: SourceFile, startOff, stopOff: int): SourceSpan =
  ## Builds a span between two byte offsets.
  var stop = stopOff
  if stop < startOff:
    stop = startOff
  result.file = f
  result.start = posAt(f, startOff)
  result.stop = posAt(f, stop)

proc spanFrom*(a, b: SourceSpan): SourceSpan =
  ## Smallest span covering both `a` and `b` (same file assumed).
  if a.file == nil:
    return b
  if b.file == nil:
    return a
  result.file = a.file
  result.start = a.start
  result.stop = b.stop

proc spanOf*(node: SourceSpan): SourceSpan =
  ## Identity helper kept so callers read symmetrically with `spanFrom`.
  result = node

proc isEmpty*(s: SourceSpan): bool =
  s.start.offset >= s.stop.offset

proc length*(s: SourceSpan): int =
  let n = s.stop.offset - s.start.offset
  if n < 0: 0 else: n

proc `$`*(s: SourcePos): string =
  result = $s.line & ":" & $s.col

proc `$`*(s: SourceSpan): string =
  if s.file == nil:
    return "<unknown>"
  result = s.file.path & ":" & $s.start.line & ":" & $s.start.col
