## EDL diagnostics.
##
## Error messages are a language feature (see specs/errors.md), not an
## afterthought, so every EDL diagnostic carries:
##   * a severity,
##   * a stable machine-readable code (`EDL0101`),
##   * a precise source span,
##   * and optionally an actionable `help` line.
##
## Codes are grouped by compiler stage:
##   EDL01xx lexer
##   EDL02xx parser
##   EDL03xx name resolution
##   EDL04xx type system
##   EDL09xx toolchain / driver
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

import ./source

const
  # ---- EDL01xx: lexer -----------------------------------------------------
  edlLexUnexpectedChar* = "EDL0101"
  edlLexUnterminatedString* = "EDL0102"
  edlLexUnterminatedComment* = "EDL0103"
  edlLexMalformedNumber* = "EDL0104"
  edlLexInvalidEscape* = "EDL0105"
  edlLexInvalidChar* = "EDL0106"

  # ---- EDL02xx: parser ----------------------------------------------------
  edlParseUnexpectedToken* = "EDL0201"
  edlParseExpectedToken* = "EDL0202"
  edlParseExpectedExpression* = "EDL0203"
  edlParseExpectedType* = "EDL0204"
  edlParseExpectedTopLevel* = "EDL0205"
  edlParseExpectedName* = "EDL0206"
  edlParseNotImplemented* = "EDL0207"

  # ---- EDL03xx: name resolution -------------------------------------------
  edlResolveUnknownIdent* = "EDL0301"
  edlResolveDuplicateDecl* = "EDL0302"
  edlResolveUsedBeforeDecl* = "EDL0303"
  edlResolveNotCallable* = "EDL0304"
  edlResolveNoMain* = "EDL0305"

  # ---- EDL04xx: type system ----------------------------------------------
  edlTypeUnknownType* = "EDL0401"
  edlTypeMismatch* = "EDL0402"
  edlTypeNotAssignable* = "EDL0403"
  edlTypeNotImplemented* = "EDL0404"
  edlTypeArgCount* = "EDL0405"
  edlTypeCondNotBool* = "EDL0406"
  edlTypeReturnMismatch* = "EDL0407"
  edlTypeUnsupportedOp* = "EDL0408"
  edlTypeUnknownField* = "EDL0409"
  edlTypeMissingReturn* = "EDL0410"
  edlTypeDuplicateField* = "EDL0411"
  edlTypeEmptyEnum* = "EDL0412"
  edlTypeEnumOrder* = "EDL0413"
  edlTypeLoopControl* = "EDL0414"

  # ---- EDL05xx: semantic --------------------------------------------------
  edlSemNotImplemented* = "EDL0501"

  # ---- EDL09xx: toolchain -------------------------------------------------
  edlDrvFileNotFound* = "EDL0901"
  edlDrvBootstrapNotFound* = "EDL0902"
  edlDrvEmitFailed* = "EDL0903"
  edlDrvBackendFailed* = "EDL0904"

type
  DiagSeverity* = enum
    dgNote
    dgWarning
    dgError

  Diagnostic* = ref object
    severity*: DiagSeverity
    code*: string
    message*: string
    help*: string
    span*: SourceSpan

  Diagnostics* = ref object
    items*: seq[Diagnostic]

proc newDiagnostics*(): Diagnostics =
  result = Diagnostics(items: @[])

proc severityName*(s: DiagSeverity): string =
  case s
  of dgNote: "note"
  of dgWarning: "warning"
  of dgError: "error"

proc addDiag*(d: Diagnostics, severity: DiagSeverity, code, message: string,
              span: SourceSpan, help = ""): Diagnostic =
  result = Diagnostic(severity: severity, code: code, message: message,
                      help: help, span: span)
  d.items.add(result)

proc reportError*(d: Diagnostics, code, message: string, span: SourceSpan,
                  help = ""): Diagnostic =
  result = addDiag(d, dgError, code, message, span, help)

proc reportWarning*(d: Diagnostics, code, message: string, span: SourceSpan,
                    help = ""): Diagnostic =
  result = addDiag(d, dgWarning, code, message, span, help)

proc reportNote*(d: Diagnostics, code, message: string, span: SourceSpan,
                 help = ""): Diagnostic =
  result = addDiag(d, dgNote, code, message, span, help)

proc errorCount*(d: Diagnostics): int =
  result = 0
  if d == nil:
    return
  for it in d.items:
    if it.severity == dgError:
      inc result

proc warningCount*(d: Diagnostics): int =
  result = 0
  if d == nil:
    return
  for it in d.items:
    if it.severity == dgWarning:
      inc result

proc hasErrors*(d: Diagnostics): bool =
  errorCount(d) > 0

proc isEmpty*(d: Diagnostics): bool =
  d == nil or d.items.len == 0

proc repeatChar(c: char, n: int): string =
  var i = 0
  while i < n:
    result.add(c)
    inc i

proc renderDiagnostic*(diag: Diagnostic): string =
  ## Renders one diagnostic as a multi-line, source-anchored message.
  result = $diag.span
  result.add(": " & severityName(diag.severity) & ": " & diag.message)
  if diag.code.len > 0:
    result.add(" [" & diag.code & "]")

  if diag.span.file != nil:
    let lineNo = diag.span.start.line
    let src = lineText(diag.span.file, lineNo)
    if src.len > 0:
      let gutter = $lineNo
      result.add("\n" & gutter & " | " & src)
      var col = diag.span.start.col
      if col > src.len:
        col = src.len
      if col < 1:
        col = 1
      var width = 1
      if diag.span.stop.line == diag.span.start.line and
         diag.span.stop.col > diag.span.start.col:
        width = diag.span.stop.col - diag.span.start.col
      let available = src.len - (col - 1)
      if width > available and available > 0:
        width = available
      if width < 1:
        width = 1
      result.add("\n" & repeatChar(' ', gutter.len) & " | " &
                 repeatChar(' ', col - 1) & repeatChar('^', width))

  if diag.help.len > 0:
    result.add("\n  help: " & diag.help)

proc render*(d: Diagnostics): string =
  ## Renders every diagnostic plus a summary line.
  result = ""
  if d == nil or d.items.len == 0:
    return
  for it in d.items:
    if result.len > 0:
      result.add("\n")
    result.add(renderDiagnostic(it))
  let e = errorCount(d)
  let w = warningCount(d)
  var parts: seq[string] = @[]
  if e > 0:
    parts.add($e & " error" & (if e == 1: "" else: "s"))
  if w > 0:
    parts.add($w & " warning" & (if w == 1: "" else: "s"))
  if parts.len > 0:
    result.add("\n")
    var summary = ""
    for i in 0 ..< parts.len:
      if i > 0:
        summary.add(", ")
      summary.add(parts[i])
    result.add("edl: " & summary & ".")

proc hasCode*(d: Diagnostics, code: string): bool =
  ## Used by negative tests: asserts a specific diagnostic was produced.
  result = false
  if d == nil:
    return
  for it in d.items:
    if it.code == code:
      return true
