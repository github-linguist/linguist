## The EDL abstract syntax tree.
##
## This tree is EDL's own. It is deliberately *not* the substrate's
## `compiler/ast.nim`, even though both are trees over a similar problem:
## sharing the substrate's AST would import the substrate's node kinds and
## semantics into the EDL language.
##
## Nodes are flat tagged objects (a `kind` plus every possible field) rather than
## substrate case-objects, so that the structure is trivially expressible in EDL.
##
## Semantic information (resolved symbols, inferred types) is *not* stored here.
## It lives in side tables indexed by `Node.id`, keeping syntax and semantics in
## separate passes, as required by the compiler pipeline in specs/compiler.md.
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

import ./source

type
  NodeKind* = enum
    nkError
    # module structure
    nkModule
    nkImportDecl
    nkImportList
    # declarations
    nkFnDecl
    nkParam
    nkStructDecl
    nkFieldDecl
    nkEnumDecl
    nkEnumValue
    nkTypeRef
    # statements
    nkBlock
    nkLetDecl
    nkVarDecl
    nkAssign
    nkReturn
    nkIf
    nkWhile
    nkFor
    nkBreak
    nkContinue
    nkExprStmt
    # expressions
    nkIntLit
    nkFloatLit
    nkStringLit
    nkCharLit
    nkBoolLit
    nkNilLit
    nkIdent
    nkCall
    nkFieldAccess
    nkIndex
    nkBinary
    nkUnary

  Node* = ref object
    kind*: NodeKind
    id*: int              ## dense index assigned by `assignIds`, used by side tables
    span*: SourceSpan
    name*: string         ## identifier, declaration name, field name, operator text
    strVal*: string       ## string and char literal value
    intVal*: int64
    floatVal*: float64
    boolVal*: bool
    children*: seq[Node]  ## params, fields, statements, arguments, operands
    typeAnn*: Node        ## type annotation, or nil
    body*: Node           ## block body, or nil
    elseBody*: Node       ## else branch, or nil
    path*: string         ## import path for nkImportDecl

# Node shapes: which field carries what, per kind.
#
#   nkModule      children = top-level declarations
#   nkImportDecl  children = nkIdent nodes (imported names, may be empty)   path = module path
#   nkFnDecl      children = nkParam nodes        typeAnn = return type     body = nkBlock
#                 name = function name
#   nkParam       name = parameter name           typeAnn = parameter type
#   nkStructDecl  children = nkFieldDecl nodes    name = struct name
#   nkFieldDecl   name = field name               typeAnn = field type
#   nkEnumDecl    children = nkEnumValue nodes    name = enum name
#   nkEnumValue   name = value name               intVal = explicit value (-1 when omitted)
#   nkTypeRef     name = type name, possibly dotted
#   nkBlock       children = statements
#   nkLetDecl     children = [initialiser expression]   typeAnn = type annotation (may be nil)
#   nkVarDecl     children = [initialiser expression]   typeAnn = type annotation (may be nil)
#   nkAssign      children = [target, value]
#   nkReturn      children = [expression] or empty
#   nkIf          children = [condition]          body = then block    elseBody = else branch
#   nkWhile       children = [condition]          body = loop block
#   nkFor         children = [iterable]           body = loop block    name = loop variable
#   nkBreak       no fields, only valid inside a loop
#   nkContinue    no fields, only valid inside a loop
#   nkExprStmt    children = [expression]
#   nkCall        children = [callee, arguments...]
#   nkFieldAccess children = [receiver]           name = field name
#   nkIndex       children = [receiver, index]
#   nkBinary      children = [left, right]        name = operator text
#   nkUnary       children = [operand]            name = operator text
#   nkError       could not be parsed; the parser has already reported why
#
proc newNode*(kind: NodeKind, span: SourceSpan): Node =
  result = Node(kind: kind, id: -1, span: span, children: @[])

proc newNodeWithName*(kind: NodeKind, name: string, span: SourceSpan): Node =
  result = newNode(kind, span)
  result.name = name

proc addChild*(n, child: Node) =
  if n != nil and child != nil:
    n.children.add(child)

proc kindName*(k: NodeKind): string =
  case k
  of nkError: "error"
  of nkModule: "module"
  of nkImportDecl: "import"
  of nkImportList: "import-list"
  of nkFnDecl: "fn"
  of nkParam: "param"
  of nkStructDecl: "struct"
  of nkFieldDecl: "field"
  of nkEnumDecl: "enum"
  of nkEnumValue: "enum-value"
  of nkTypeRef: "type"
  of nkBlock: "block"
  of nkLetDecl: "let"
  of nkVarDecl: "var"
  of nkAssign: "assign"
  of nkReturn: "return"
  of nkIf: "if"
  of nkWhile: "while"
  of nkFor: "for"
  of nkBreak: "break"
  of nkContinue: "continue"
  of nkExprStmt: "expr"
  of nkIntLit: "int"
  of nkFloatLit: "float"
  of nkStringLit: "string"
  of nkCharLit: "char"
  of nkBoolLit: "bool"
  of nkNilLit: "nil"
  of nkIdent: "ident"
  of nkCall: "call"
  of nkFieldAccess: "field-access"
  of nkIndex: "index"
  of nkBinary: "binary"
  of nkUnary: "unary"

proc isExpr*(k: NodeKind): bool =
  result = k.ord >= nkIntLit.ord and k.ord <= nkUnary.ord

proc isStmt*(k: NodeKind): bool =
  result = (k.ord >= nkBlock.ord and k.ord <= nkExprStmt.ord)

proc isDecl*(k: NodeKind): bool =
  result = (k.ord >= nkFnDecl.ord and k.ord <= nkTypeRef.ord)

# ---- identity assignment ---------------------------------------------------

proc assignIdsRec(n: Node, next: var int) =
  if n == nil:
    return
  n.id = next
  inc next
  for c in n.children:
    assignIdsRec(c, next)
  assignIdsRec(n.typeAnn, next)
  assignIdsRec(n.body, next)
  assignIdsRec(n.elseBody, next)

proc assignIds*(n: Node): int =
  ## Assigns dense ids in depth-first order to every node reachable from `n`,
  ## and returns the total number of nodes. Side tables are sized with it.
  var next = 0
  assignIdsRec(n, next)
  result = next

# ---- debugging -------------------------------------------------------------

proc dumpTreeRec(n: Node, depth: int, dest: var string) =
  if n == nil:
    return
  var i = 0
  while i < depth:
    dest.add("  ")
    inc i
  dest.add(kindName(n.kind))
  if n.name.len > 0:
    dest.add(" " & n.name)
  case n.kind
  of nkIntLit: dest.add(" " & $n.intVal)
  of nkFloatLit: dest.add(" " & $n.floatVal)
  of nkStringLit: dest.add(" \"" & n.strVal & "\"")
  of nkBoolLit: dest.add(" " & (if n.boolVal: "true" else: "false"))
  of nkImportDecl: dest.add(" " & n.path)
  else: discard
  dest.add("\n")
  for c in n.children:
    dumpTreeRec(c, depth + 1, dest)
  dumpTreeRec(n.typeAnn, depth + 1, dest)
  dumpTreeRec(n.body, depth + 1, dest)
  dumpTreeRec(n.elseBody, depth + 1, dest)

proc dumpTree*(n: Node): string =
  ## Indented textual dump, used by tests and by `edlc emit-ast`.
  result = ""
  dumpTreeRec(n, 0, result)
