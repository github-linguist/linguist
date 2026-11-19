## The EDL parser: tokens in, EDL abstract syntax tree out.
##
## Recursive descent with a precedence-climbing loop for binary operators. The
## parser never stops at the first error: it reports, recovers to the next
## plausible construct, and continues, so one run reports every syntax error in
## the file.
##
## Constructs that are specified but not implemented yet (generics, `unsafe`
## blocks, the `?` error-propagation operator) are *parsed and reported*, not
## rejected as syntax errors. This keeps the grammar stable while the semantics
## grow, and gives users a roadmap-shaped message instead of a confusing one.
##
## Grammar implemented here (normative text in specs/syntax.md):
##
##   module      := { topDecl }
##   topDecl     := importDecl | fnDecl | structDecl | enumDecl
##   importDecl  := 'import' dottedPath
##                | 'import' '{' name { ',' name } '}' 'from' dottedPath
##   fnDecl      := 'fn' name '(' [ params ] ')' [ '->' type ] block
##   params      := param { ',' param }
##   param       := name ':' type
##   structDecl  := 'struct' name '{' { name ':' type } '}'
##   enumDecl    := 'enum' name '{' [ name [ '=' intLit ] { ',' ... } [','] ] '}'
##   block       := '{' { stmt } '}'
##   stmt        := letDecl | varDecl | returnStmt | ifStmt | whileStmt
##                | forStmt | block | exprStmt
##   expr        := equality, then comparisons, additive, multiplicative, unary
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

import ./source
import ./tokens
import ./ast
import ./diagnostics

type
  ParserState = ref object
    file: SourceFile
    toks: seq[Token]
    pos: int
    diags: Diagnostics

  ParseResult* = object
    module*: Node       ## the nkModule node
    nodeCount*: int     ## number of nodes, used to size semantic side tables
    diags*: Diagnostics

# ---- cursor ----------------------------------------------------------------

proc cur(p: ParserState): Token =
  if p.pos < p.toks.len:
    result = p.toks[p.pos]
  elif p.toks.len > 0:
    result = p.toks[p.toks.len - 1]
  else:
    result = Token(kind: tkEof, text: "", span: p.file.newSpan(0, 0))

proc advance(p: ParserState): Token =
  result = cur(p)
  if p.pos < p.toks.len:
    inc p.pos

proc prevSpan(p: ParserState): SourceSpan =
  if p.pos > 0 and p.pos <= p.toks.len:
    result = p.toks[p.pos - 1].span
  else:
    result = cur(p).span

proc check(p: ParserState, kind: TokenKind): bool =
  result = cur(p).kind == kind

proc match(p: ParserState, kind: TokenKind): bool =
  if check(p, kind):
    discard advance(p)
    result = true

proc errorHere(p: ParserState, code, message, help: string): void =
  discard p.diags.reportError(code, message, cur(p).span, help)

proc expect(p: ParserState, kind: TokenKind, context: string): Token =
  if check(p, kind):
    result = advance(p)
  else:
    errorHere(p, edlParseExpectedToken,
              "expected " & tokenKindName(kind) & " " & context &
              ", found " & describe(cur(p)), "")
    result = Token(kind: tkBadToken, text: "", span: cur(p).span)

proc synchronize(p: ParserState) =
  ## Recovery: skips tokens until something that can start a declaration or a
  ## statement. A leading '}' is consumed, since at top level it is stray.
  if check(p, tkRBrace):
    discard advance(p)
    return
  while not check(p, tkEof):
    let k = cur(p).kind
    if k == tkKwFn or k == tkKwStruct or k == tkKwEnum or k == tkKwImport or
       k == tkKwLet or k == tkKwVar or k == tkKwReturn or k == tkKwIf or
       k == tkKwWhile or k == tkKwFor or k == tkRBrace:
      break
    discard advance(p)

# ---- types -----------------------------------------------------------------

proc parseTypeRef(p: ParserState): Node =
  let start = cur(p).span
  if not check(p, tkIdent):
    errorHere(p, edlParseExpectedType,
              "expected a type, found " & describe(cur(p)),
              "types are named, for example 'i32', 'string' or 'User'")
    result = newNode(nkTypeRef, start)
    result.name = "<error>"
    if not check(p, tkRBrace) and not check(p, tkEof) and
       not check(p, tkComma) and not check(p, tkRParen):
      discard advance(p)
    return
  var name = advance(p).text
  while match(p, tkDot):
    if check(p, tkIdent):
      name = name & "." & advance(p).text
    else:
      errorHere(p, edlParseExpectedName, "expected a name after '.'", "")
      break
  result = newNode(nkTypeRef, spanFrom(start, prevSpan(p)))
  result.name = name

# ---- expressions -----------------------------------------------------------

proc parseExpression(p: ParserState, minPrec: int): Node

proc parsePrimary(p: ParserState): Node =
  let t = cur(p)
  case t.kind
  of tkIntLit:
    discard advance(p)
    result = newNode(nkIntLit, t.span)
    result.intVal = t.intVal
  of tkFloatLit:
    discard advance(p)
    result = newNode(nkFloatLit, t.span)
    result.floatVal = t.floatVal
  of tkStringLit:
    discard advance(p)
    result = newNode(nkStringLit, t.span)
    result.strVal = t.strVal
  of tkCharLit:
    discard advance(p)
    result = newNode(nkCharLit, t.span)
    result.strVal = t.strVal
  of tkKwTrue, tkKwFalse:
    discard advance(p)
    result = newNode(nkBoolLit, t.span)
    result.boolVal = t.kind == tkKwTrue
  of tkKwNil:
    discard advance(p)
    result = newNode(nkNilLit, t.span)
  of tkIdent:
    discard advance(p)
    result = newNodeWithName(nkIdent, t.text, t.span)
  of tkLParen:
    discard advance(p)
    result = parseExpression(p, 1)
    discard expect(p, tkRParen, "to close the parenthesised expression")
  else:
    errorHere(p, edlParseExpectedExpression,
              "expected an expression, found " & describe(t), "")
    result = newNode(nkError, t.span)
    if not check(p, tkRBrace) and not check(p, tkEof):
      discard advance(p)

proc parsePostfix(p: ParserState): Node =
  result = parsePrimary(p)
  var keepGoing = true
  while keepGoing:
    keepGoing = false
    if check(p, tkLParen):
      discard advance(p)
      var args: seq[Node] = @[]
      if not check(p, tkRParen):
        args.add(parseExpression(p, 1))
        while match(p, tkComma):
          if check(p, tkRParen):
            break
          args.add(parseExpression(p, 1))
      let close = expect(p, tkRParen, "to close the argument list")
      let call = newNode(nkCall, spanFrom(result.span, close.span))
      call.addChild(result)
      for a in args:
        call.addChild(a)
      result = call
      keepGoing = true
    elif check(p, tkDot):
      discard advance(p)
      if check(p, tkIdent):
        let field = advance(p)
        let node = newNodeWithName(nkFieldAccess, field.text,
                                   spanFrom(result.span, field.span))
        node.addChild(result)
        result = node
        keepGoing = true
      else:
        errorHere(p, edlParseExpectedName,
                  "expected a field name after '.', found " & describe(cur(p)), "")
    elif check(p, tkLBracket):
      discard advance(p)
      let index = parseExpression(p, 1)
      let close = expect(p, tkRBracket, "to close the index")
      let node = newNode(nkIndex, spanFrom(result.span, close.span))
      node.addChild(result)
      node.addChild(index)
      result = node
      keepGoing = true
    elif check(p, tkQuestion):
      let q = advance(p)
      discard p.diags.reportError(edlParseNotImplemented,
        "the '?' error-propagation operator is not implemented yet", q.span,
        "Result and Option are planned; see specs/errors.md")

proc parseUnary(p: ParserState): Node =
  if check(p, tkMinus) or check(p, tkKwNot):
    let op = advance(p)
    let operand = parseUnary(p)
    result = newNode(nkUnary, spanFrom(op.span, operand.span))
    result.name = op.text
    result.addChild(operand)
  else:
    result = parsePostfix(p)

proc binaryPrec(k: TokenKind): int =
  ## Higher binds tighter. All operators are left-associative.
  case k
  of tkKwOr: 1
  of tkKwAnd: 2
  of tkEq, tkNe: 3
  of tkLt, tkLe, tkGt, tkGe: 4
  of tkPlus, tkMinus: 5
  of tkStar, tkSlash, tkPercent: 6
  else: 0

proc parseExpression(p: ParserState, minPrec: int): Node =
  var left = parseUnary(p)
  var keepGoing = true
  while keepGoing:
    let k = cur(p).kind
    let prec = binaryPrec(k)
    if prec == 0 or prec < minPrec:
      break
    let op = advance(p)
    let right = parseExpression(p, prec + 1)
    let node = newNode(nkBinary, spanFrom(left.span, right.span))
    node.name = op.text
    node.addChild(left)
    node.addChild(right)
    left = node
  result = left

proc parseExpression(p: ParserState): Node =
  result = parseExpression(p, 1)

# ---- statements ------------------------------------------------------------

proc parseBlock(p: ParserState): Node

proc parseLetOrVar(p: ParserState, kind: NodeKind): Node =
  let kw = advance(p)
  var name = "<error>"
  if check(p, tkIdent):
    name = advance(p).text
  else:
    errorHere(p, edlParseExpectedName,
              "expected a variable name after '" & kw.text & "', found " &
              describe(cur(p)), "")
  var typeAnn: Node = nil
  if match(p, tkColon):
    typeAnn = parseTypeRef(p)
  var init: Node = nil
  if match(p, tkAssign):
    init = parseExpression(p)
  else:
    errorHere(p, edlParseExpectedToken,
              "'" & kw.text & "' requires an initialiser",
              "write '" & kw.text & " " & name & " = <expression>'")
  result = newNodeWithName(kind, name, spanFrom(kw.span, prevSpan(p)))
  result.typeAnn = typeAnn
  if init != nil:
    result.addChild(init)

proc parseIfStmt(p: ParserState): Node =
  let kw = advance(p)
  let cond = parseExpression(p)
  let thenBlock = parseBlock(p)
  result = newNode(nkIf, spanFrom(kw.span, thenBlock.span))
  result.addChild(cond)
  result.body = thenBlock
  if match(p, tkKwElse):
    if check(p, tkKwIf):
      let nested = parseIfStmt(p)
      result.elseBody = nested
      result.span = spanFrom(kw.span, nested.span)
    else:
      let elseBlock = parseBlock(p)
      result.elseBody = elseBlock
      result.span = spanFrom(kw.span, elseBlock.span)

proc parseWhileStmt(p: ParserState): Node =
  let kw = advance(p)
  let cond = parseExpression(p)
  let body = parseBlock(p)
  result = newNode(nkWhile, spanFrom(kw.span, body.span))
  result.addChild(cond)
  result.body = body

proc parseForStmt(p: ParserState): Node =
  let kw = advance(p)
  var name = "<error>"
  if check(p, tkIdent):
    name = advance(p).text
  else:
    errorHere(p, edlParseExpectedName,
              "expected a loop variable after 'for', found " & describe(cur(p)), "")
  discard expect(p, tkKwIn, "after the loop variable")
  let iterable = parseExpression(p)
  let body = parseBlock(p)
  result = newNodeWithName(nkFor, name, spanFrom(kw.span, body.span))
  result.addChild(iterable)
  result.body = body

proc parseStatement(p: ParserState): Node =
  case cur(p).kind
  of tkKwLet:
    result = parseLetOrVar(p, nkLetDecl)
  of tkKwVar:
    result = parseLetOrVar(p, nkVarDecl)
  of tkKwReturn:
    let kw = advance(p)
    result = newNode(nkReturn, kw.span)
    if not check(p, tkRBrace) and not check(p, tkEof):
      result.addChild(parseExpression(p))
      result.span = spanFrom(kw.span, prevSpan(p))
  of tkKwIf:
    result = parseIfStmt(p)
  of tkKwWhile:
    result = parseWhileStmt(p)
  of tkKwFor:
    result = parseForStmt(p)
  of tkKwBreak:
    let kw = advance(p)
    result = newNode(nkBreak, kw.span)
  of tkKwContinue:
    let kw = advance(p)
    result = newNode(nkContinue, kw.span)
  of tkKwUnsafe:
    let kw = advance(p)
    discard p.diags.reportError(edlParseNotImplemented,
      "'unsafe' blocks are not implemented yet", kw.span,
      "low-level memory access is planned; see specs/memory.md")
    let body = parseBlock(p)
    result = newNode(nkBlock, spanFrom(kw.span, body.span))
    result.body = body
  of tkLBrace:
    result = parseBlock(p)
  else:
    let lhs = parseExpression(p)
    if match(p, tkAssign):
      let rhs = parseExpression(p)
      result = newNode(nkAssign, spanFrom(lhs.span, rhs.span))
      result.addChild(lhs)
      result.addChild(rhs)
    else:
      result = newNode(nkExprStmt, lhs.span)
      result.addChild(lhs)

proc parseBlock(p: ParserState): Node =
  let openTok = expect(p, tkLBrace, "to start a block")
  result = newNode(nkBlock, openTok.span)
  while not check(p, tkRBrace) and not check(p, tkEof):
    let before = p.pos
    result.addChild(parseStatement(p))
    if p.pos == before:
      # Nothing was consumed: force progress so a malformed block cannot loop.
      discard advance(p)
  let closeTok = expect(p, tkRBrace, "to close the block")
  result.span = spanFrom(openTok.span, closeTok.span)

# ---- declarations ----------------------------------------------------------

proc skipGenerics(p: ParserState) =
  ## Generics are specified but not implemented. Report, then recover to the
  ## parameter list or the body brace so the rest of the declaration still parses.
  discard p.diags.reportError(edlParseNotImplemented,
    "generic parameters are not implemented yet", cur(p).span,
    "generics are planned; see specs/generics.md")
  while not check(p, tkEof) and not check(p, tkLParen) and not check(p, tkLBrace):
    discard advance(p)

proc parseParam(p: ParserState): Node =
  if not check(p, tkIdent):
    errorHere(p, edlParseExpectedName,
              "expected a parameter name, found " & describe(cur(p)), "")
    result = newNode(nkParam, cur(p).span)
    result.name = "<error>"
    return
  let nameTok = advance(p)
  result = newNodeWithName(nkParam, nameTok.text, nameTok.span)
  if match(p, tkColon):
    result.typeAnn = parseTypeRef(p)
  else:
    errorHere(p, edlParseExpectedToken,
              "parameter '" & nameTok.text & "' needs a type",
              "write '" & nameTok.text & ": <type>'")

proc parseFnDecl(p: ParserState): Node =
  let kw = advance(p)
  var name = "<error>"
  if check(p, tkIdent):
    name = advance(p).text
  else:
    errorHere(p, edlParseExpectedName,
              "expected a function name after 'fn', found " & describe(cur(p)), "")
  if check(p, tkLt):
    skipGenerics(p)
  result = newNodeWithName(nkFnDecl, name, kw.span)
  discard expect(p, tkLParen, "after the function name")
  if not check(p, tkRParen):
    result.addChild(parseParam(p))
    while match(p, tkComma):
      if check(p, tkRParen):
        break
      result.addChild(parseParam(p))
  discard expect(p, tkRParen, "to close the parameter list")
  if match(p, tkArrow):
    result.typeAnn = parseTypeRef(p)
  let body = parseBlock(p)
  result.body = body
  result.span = spanFrom(kw.span, body.span)

proc parseStructDecl(p: ParserState): Node =
  let kw = advance(p)
  var name = "<error>"
  if check(p, tkIdent):
    name = advance(p).text
  else:
    errorHere(p, edlParseExpectedName,
              "expected a struct name after 'struct', found " & describe(cur(p)), "")
  if check(p, tkLt):
    skipGenerics(p)
  result = newNodeWithName(nkStructDecl, name, kw.span)
  discard expect(p, tkLBrace, "to start the struct body")
  while not check(p, tkRBrace) and not check(p, tkEof):
    let before = p.pos
    if check(p, tkIdent):
      let fieldName = advance(p)
      var field = newNodeWithName(nkFieldDecl, fieldName.text, fieldName.span)
      if match(p, tkColon):
        field.typeAnn = parseTypeRef(p)
      else:
        errorHere(p, edlParseExpectedToken,
                  "field '" & fieldName.text & "' needs a type",
                  "write '" & fieldName.text & ": <type>'")
      field.span = spanFrom(fieldName.span, prevSpan(p))
      result.addChild(field)
    else:
      errorHere(p, edlParseExpectedName,
                "expected a field name, found " & describe(cur(p)), "")
      synchronize(p)
    if p.pos == before:
      discard advance(p)
  discard expect(p, tkRBrace, "to close the struct body")
  result.span = spanFrom(kw.span, prevSpan(p))

proc parseEnumValue(p: ParserState): Node =
  if not check(p, tkIdent):
    errorHere(p, edlParseExpectedName,
              "expected an enum value name, found " & describe(cur(p)), "")
    result = newNode(nkEnumValue, cur(p).span)
    result.name = "<error>"
    result.intVal = -1
    if not check(p, tkRBrace) and not check(p, tkEof):
      discard advance(p)
    return
  let nameTok = advance(p)
  result = newNodeWithName(nkEnumValue, nameTok.text, nameTok.span)
  result.intVal = -1
  if match(p, tkAssign):
    if check(p, tkIntLit):
      let valueTok = advance(p)
      result.intVal = valueTok.intVal
      result.span = spanFrom(nameTok.span, valueTok.span)
    else:
      errorHere(p, edlParseExpectedExpression,
                "an explicit enum value must be an integer literal, found " &
                describe(cur(p)), "")

proc parseEnumDecl(p: ParserState): Node =
  let kw = advance(p)
  var name = "<error>"
  if check(p, tkIdent):
    name = advance(p).text
  else:
    errorHere(p, edlParseExpectedName,
              "expected an enum name after 'enum', found " & describe(cur(p)), "")
  if check(p, tkLt):
    skipGenerics(p)
  result = newNodeWithName(nkEnumDecl, name, kw.span)
  discard expect(p, tkLBrace, "to start the enum body")
  if not check(p, tkRBrace):
    result.addChild(parseEnumValue(p))
    while match(p, tkComma):
      if check(p, tkRBrace):
        break
      result.addChild(parseEnumValue(p))
  discard expect(p, tkRBrace, "to close the enum body")
  result.span = spanFrom(kw.span, prevSpan(p))

proc parseDottedPath(p: ParserState): string =
  if not check(p, tkIdent):
    errorHere(p, edlParseExpectedName,
              "expected a module name, found " & describe(cur(p)), "")
    return ""
  result = advance(p).text
  while match(p, tkDot):
    if check(p, tkIdent):
      result = result & "." & advance(p).text
    else:
      errorHere(p, edlParseExpectedName, "expected a name after '.'", "")
      break

proc parseImportDecl(p: ParserState): Node =
  let kw = advance(p)
  result = newNode(nkImportDecl, kw.span)
  if check(p, tkLBrace):
    discard advance(p)
    if not check(p, tkRBrace):
      while true:
        if check(p, tkIdent):
          let nameTok = advance(p)
          result.addChild(newNodeWithName(nkIdent, nameTok.text, nameTok.span))
        else:
          errorHere(p, edlParseExpectedName,
                    "expected an imported name, found " & describe(cur(p)), "")
          break
        if not match(p, tkComma):
          break
        if check(p, tkRBrace):
          break
    discard expect(p, tkRBrace, "to close the import list")
    if not match(p, tkKwFrom):
      errorHere(p, edlParseExpectedToken,
                "'import { ... }' requires 'from'",
                "write 'import { Name } from module.path'")
    result.path = parseDottedPath(p)
  else:
    result.path = parseDottedPath(p)
  result.span = spanFrom(kw.span, prevSpan(p))

# ---- module ----------------------------------------------------------------

proc parseTopLevel(p: ParserState): Node =
  case cur(p).kind
  of tkKwImport:
    result = parseImportDecl(p)
  of tkKwFn:
    result = parseFnDecl(p)
  of tkKwStruct:
    result = parseStructDecl(p)
  of tkKwEnum:
    result = parseEnumDecl(p)
  else:
    errorHere(p, edlParseExpectedTopLevel,
              "expected a top-level declaration ('fn', 'struct', 'enum' or " &
              "'import'), found " & describe(cur(p)),
              "statements are only allowed inside a function body")
    result = newNode(nkError, cur(p).span)
    synchronize(p)

proc parseModule*(file: SourceFile, tokens: seq[Token],
                  diags: Diagnostics): ParseResult =
  ## Parses a whole EDL file. Every syntax problem is reported in `diags`;
  ## `result.module` is usable even when errors were found, so later passes can
  ## still run and report their own diagnostics.
  let p = ParserState(file: file, toks: tokens, pos: 0, diags: diags)
  let module = newNode(nkModule, file.newSpan(0, file.text.len))
  while not check(p, tkEof):
    let before = p.pos
    let decl = parseTopLevel(p)
    if decl != nil and decl.kind != nkError:
      module.addChild(decl)
    if p.pos == before:
      # Never loop forever on malformed input.
      discard advance(p)
  result.module = module
  result.nodeCount = assignIds(module)
  result.diags = diags
