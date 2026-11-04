## EDL lexical analysis.
##
## Turns EDL source text into a flat token sequence. The lexer never aborts on a
## malformed input: it records a diagnostic, emits a usable token, and carries on,
## so a single run reports every lexical problem in the file.
##
## Lexical rules are specified in specs/syntax.md.
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

import std/strutils

import ./source
import ./tokens
import ./diagnostics

type
  LexerState* = ref object
    file*: SourceFile
    diags*: Diagnostics
    pos*: int

proc newLexerState*(file: SourceFile, diags: Diagnostics): LexerState =
  result = LexerState(file: file, diags: diags, pos: 0)

# ---- character helpers -----------------------------------------------------

proc isDigit(c: char): bool =
  result = c >= '0' and c <= '9'

proc isHexDigit(c: char): bool =
  result = isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')

proc hexValue(c: char): int =
  if c >= '0' and c <= '9':
    result = ord(c) - ord('0')
  elif c >= 'a' and c <= 'f':
    result = ord(c) - ord('a') + 10
  else:
    result = ord(c) - ord('A') + 10

proc isIdentStart(c: char): bool =
  result = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'

proc isIdentPart(c: char): bool =
  result = isIdentStart(c) or isDigit(c)

# ---- cursor ----------------------------------------------------------------

proc atEnd(s: LexerState): bool =
  result = s.pos >= s.file.text.len

proc peek(s: LexerState): char =
  if s.pos < 0 or s.pos >= s.file.text.len:
    result = '\0'
  else:
    result = s.file.text[s.pos]

proc peekAt(s: LexerState, ahead: int): char =
  let i = s.pos + ahead
  if i < 0 or i >= s.file.text.len:
    result = '\0'
  else:
    result = s.file.text[i]

proc advance(s: LexerState): char =
  result = s.peek()
  inc s.pos

proc tok(s: LexerState, kind: TokenKind, start: int): Token =
  ## Builds a token spanning `[start, s.pos)`.
  result = Token(kind: kind, text: s.file.text[start ..< s.pos],
                 span: s.file.newSpan(start, s.pos))

proc errorAt(s: LexerState, code, message: string, start, stop: int, help: string): void =
  discard s.diags.reportError(code, message, s.file.newSpan(start, stop), help)

# ---- trivia ----------------------------------------------------------------

proc skipTrivia(s: LexerState) =
  ## Skips whitespace and comments. Block comments nest.
  var again = true
  while again:
    again = false
    while not s.atEnd():
      let c = s.peek()
      if c == ' ' or c == '\t' or c == '\r' or c == '\n':
        inc s.pos
      else:
        break
    if s.peek() == '/' and s.peekAt(1) == '/':
      while not s.atEnd() and s.peek() != '\n':
        inc s.pos
      again = true
    elif s.peek() == '/' and s.peekAt(1) == '*':
      let start = s.pos
      s.pos += 2
      var depth = 1
      while depth > 0 and not s.atEnd():
        if s.peek() == '/' and s.peekAt(1) == '*':
          inc depth
          s.pos += 2
        elif s.peek() == '*' and s.peekAt(1) == '/':
          dec depth
          s.pos += 2
        else:
          inc s.pos
      if depth > 0:
        errorAt(s, edlLexUnterminatedComment, "unterminated block comment",
                start, s.pos, "close the comment with '*/'")
      again = true

# ---- literals --------------------------------------------------------------

proc scanEscape(s: LexerState, value: var string): bool =
  ## Consumes the character after a backslash. Returns false on an invalid escape.
  if s.atEnd():
    return false
  let esc = s.advance()
  result = true
  case esc
  of 'n': value.add('\n')
  of 'r': value.add('\r')
  of 't': value.add('\t')
  of '0': value.add('\0')
  of '\\': value.add('\\')
  of '"': value.add('"')
  of '\'': value.add('\'')
  else:
    errorAt(s, edlLexInvalidEscape, "unknown escape sequence '\\" & esc & "'",
            s.pos - 2, s.pos,
            "use \\n, \\r, \\t, \\0, \\\\, \\\" or \\'")
    value.add(esc)

proc scanString(s: LexerState, start: int): Token =
  ## `s.pos` is just after the opening quote.
  var value = ""
  var terminated = false
  while not s.atEnd():
    let c = s.peek()
    if c == '"':
      inc s.pos
      terminated = true
      break
    elif c == '\n':
      break
    elif c == '\\':
      inc s.pos
      discard scanEscape(s, value)
    else:
      value.add(c)
      inc s.pos
  if not terminated:
    errorAt(s, edlLexUnterminatedString, "unterminated string literal",
            start, s.pos, "add a closing '\"' on the same line")
  result = tok(s, tkStringLit, start)
  result.strVal = value

proc scanChar(s: LexerState, start: int): Token =
  ## `s.pos` is just after the opening quote. Consumes up to the closing quote
  ## (or the end of the line) so that a multi-character literal such as 'ab' is
  ## reported as one over-long char rather than as an unterminated literal.
  var value = ""
  while not s.atEnd() and s.peek() != '\'' and s.peek() != '\n':
    if s.peek() == '\\':
      inc s.pos
      discard scanEscape(s, value)
    else:
      value.add(s.advance())
  var terminated = false
  if s.peek() == '\'':
    discard s.advance()
    terminated = true
  if not terminated:
    errorAt(s, edlLexUnterminatedString, "unterminated character literal",
            start, s.pos, "add a closing \"'\"")
  elif value.len != 1:
    errorAt(s, edlLexInvalidChar, "character literal must contain exactly one character",
            start, s.pos, "a char holds exactly one character, such as 'a' or '\\n'")
  result = tok(s, tkCharLit, start)
  result.strVal = value

proc scanDigits(s: LexerState, base: int, value: var int64, digits: var int, overflowing: var bool) =
  while not s.atEnd():
    let c = s.peek()
    if c == '_':
      inc s.pos
    elif (base == 16 and isHexDigit(c)) or (base == 10 and isDigit(c)) or
         (base == 2 and (c == '0' or c == '1')):
      let d = int64(if base == 16: hexValue(c) else: ord(c) - ord('0'))
      if value > (high(int64) - d) div int64(base):
        overflowing = true
      else:
        value = value * int64(base) + d
      inc digits
      inc s.pos
    else:
      break

proc scanNumber(s: LexerState, start: int): Token =
  var value: int64 = 0
  var digits = 0
  var overflowing = false
  var isFloat = false

  if s.peek() == '0' and (s.peekAt(1) == 'x' or s.peekAt(1) == 'X'):
    s.pos += 2
    scanDigits(s, 16, value, digits, overflowing)
    if digits == 0:
      errorAt(s, edlLexMalformedNumber, "expected hexadecimal digits after '0x'",
              start, s.pos, "")
  elif s.peek() == '0' and (s.peekAt(1) == 'b' or s.peekAt(1) == 'B'):
    s.pos += 2
    scanDigits(s, 2, value, digits, overflowing)
    if digits == 0:
      errorAt(s, edlLexMalformedNumber, "expected binary digits after '0b'",
              start, s.pos, "")
  else:
    scanDigits(s, 10, value, digits, overflowing)
    if s.peek() == '.' and isDigit(s.peekAt(1)):
      isFloat = true
      inc s.pos
      while isDigit(s.peek()) or s.peek() == '_':
        inc s.pos
    if s.peek() == 'e' or s.peek() == 'E':
      var ahead = 1
      if s.peekAt(ahead) == '+' or s.peekAt(ahead) == '-':
        inc ahead
      if isDigit(s.peekAt(ahead)):
        isFloat = true
        inc s.pos
        if s.peek() == '+' or s.peek() == '-':
          inc s.pos
        while isDigit(s.peek()):
          inc s.pos

  result = tok(s, tkIntLit, start)
  if isFloat:
    result.kind = tkFloatLit
    let raw = result.text.replace("_", "")
    try:
      result.floatVal = parseFloat(raw)
    except ValueError:
      errorAt(s, edlLexMalformedNumber, "malformed float literal '" & result.text & "'",
              start, s.pos, "")
  elif overflowing:
    errorAt(s, edlLexMalformedNumber, "integer literal is too large for i64",
            start, s.pos, "literal values must fit in 64 bits")
  else:
    result.intVal = value

# ---- identifiers -----------------------------------------------------------

proc scanIdent(s: LexerState, start: int): Token =
  inc s.pos
  while isIdentPart(s.peek()):
    inc s.pos
  let text = s.file.text[start ..< s.pos]
  result = tok(s, keywordKind(text), start)

# ---- entry point -----------------------------------------------------------

proc scanToken(s: LexerState): Token =
  let start = s.pos
  let c = s.peek()
  if isIdentStart(c):
    result = scanIdent(s, start)
  elif isDigit(c):
    result = scanNumber(s, start)
  elif c == '"':
    inc s.pos
    result = scanString(s, start)
  elif c == '\'':
    inc s.pos
    result = scanChar(s, start)
  else:
    inc s.pos
    case c
    of '(': result = tok(s, tkLParen, start)
    of ')': result = tok(s, tkRParen, start)
    of '{': result = tok(s, tkLBrace, start)
    of '}': result = tok(s, tkRBrace, start)
    of '[': result = tok(s, tkLBracket, start)
    of ']': result = tok(s, tkRBracket, start)
    of ',': result = tok(s, tkComma, start)
    of ':': result = tok(s, tkColon, start)
    of '.': result = tok(s, tkDot, start)
    of ';': result = tok(s, tkSemicolon, start)
    of '?': result = tok(s, tkQuestion, start)
    of '+': result = tok(s, tkPlus, start)
    of '*': result = tok(s, tkStar, start)
    of '/': result = tok(s, tkSlash, start)
    of '%': result = tok(s, tkPercent, start)
    of '-':
      if s.peek() == '>':
        inc s.pos
        result = tok(s, tkArrow, start)
      else:
        result = tok(s, tkMinus, start)
    of '=':
      if s.peek() == '=':
        inc s.pos
        result = tok(s, tkEq, start)
      else:
        result = tok(s, tkAssign, start)
    of '!':
      if s.peek() == '=':
        inc s.pos
        result = tok(s, tkNe, start)
      else:
        errorAt(s, edlLexUnexpectedChar, "unexpected character '!'", start, s.pos,
                "EDL has no '!' operator; write the comparison explicitly")
        result = tok(s, tkBadToken, start)
    of '<':
      if s.peek() == '=':
        inc s.pos
        result = tok(s, tkLe, start)
      else:
        result = tok(s, tkLt, start)
    of '>':
      if s.peek() == '=':
        inc s.pos
        result = tok(s, tkGe, start)
      else:
        result = tok(s, tkGt, start)
    else:
      errorAt(s, edlLexUnexpectedChar, "unexpected character '" & c & "'",
              start, s.pos, "")
      result = tok(s, tkBadToken, start)

proc tokenize*(file: SourceFile, diags: Diagnostics): seq[Token] =
  ## Lexes `file` completely. The result always ends with a `tkEof` token, and
  ## every problem found is reported in `diags`.
  var s = newLexerState(file, diags)
  result = @[]
  while true:
    skipTrivia(s)
    if s.atEnd():
      result.add(Token(kind: tkEof, text: "", span: file.newSpan(s.pos, s.pos)))
      break
    result.add(scanToken(s))
