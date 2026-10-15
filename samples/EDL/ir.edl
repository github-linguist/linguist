## The EDL intermediate representation.
##
## The IR is typed, fully resolved and free of syntax: no scopes, no type
## annotations to look up, no inference left. Every name in it is already the
## final name to emit, and every node carries its `TypeId`.
##
## Instructions and expressions share one node type (`IrNode`) discriminated by
## `IrKind`, which keeps the tree trivially expressible in EDL and makes the
## lowering pass a direct structural translation.
##
## Shapes:
##   irIntLit/irFloatLit/irStringLit/irCharLit/irBoolLit   literals
##   irLocal/irParam          name
##   irEnumValue              ownerName = enum, name = value
##   irCall                   name = function, args = arguments
##   irBuiltinCall            name = builtin ("print"), args = arguments
##   irBinary                 name = operator, args = [left, right]
##   irUnary                  name = operator, args = [operand]
##   irFieldAccess            name = field, args = [receiver]
##   irLet/irVar              name, ty, args = [initialiser]
##   irAssign                 args = [target, value]
##   irReturn                 args = [value] or empty
##   irIf                     args = [condition], thenBody, elseBody
##   irWhile                  args = [condition], thenBody
##   irBlock                  thenBody (a nested lexical block)
##   irBreak/irContinue       no operands, only emitted inside a loop
##   irExprStmt               args = [expression]
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

import ./types

type
  IrKind* = enum
    irError
    # expressions
    irIntLit
    irFloatLit
    irStringLit
    irCharLit
    irBoolLit
    irLocal
    irParam
    irEnumValue
    irCall
    irBuiltinCall
    irBinary
    irUnary
    irFieldAccess
    # instructions
    irLet
    irVar
    irAssign
    irReturn
    irIf
    irWhile
    irBlock
    irBreak
    irContinue
    irExprStmt

  IrNode* = ref object
    kind*: IrKind
    ty*: TypeId
    name*: string
    ownerName*: string
    intVal*: int64
    floatVal*: float64
    strVal*: string
    boolVal*: bool
    args*: seq[IrNode]
    thenBody*: seq[IrNode]
    elseBody*: seq[IrNode]

  IrParam* = object
    name*: string
    ty*: TypeId

  IrField* = object
    name*: string
    ty*: TypeId

  IrStruct* = object
    name*: string
    fields*: seq[IrField]

  IrEnumValue* = object
    name*: string
    value*: int64

  IrEnum* = object
    name*: string
    values*: seq[IrEnumValue]

  IrFn* = object
    name*: string
    params*: seq[IrParam]
    returnType*: TypeId
    body*: seq[IrNode]

  IrModule* = ref object
    name*: string
    sourcePath*: string
    fns*: seq[IrFn]
    structs*: seq[IrStruct]
    enums*: seq[IrEnum]
    hasMain*: bool

proc newIr*(kind: IrKind, ty: TypeId): IrNode =
  result = IrNode(kind: kind, ty: ty, name: "", ownerName: "",
                  args: @[], thenBody: @[], elseBody: @[])

proc irKindName*(k: IrKind): string =
  case k
  of irError: "error"
  of irIntLit: "int"
  of irFloatLit: "float"
  of irStringLit: "string"
  of irCharLit: "char"
  of irBoolLit: "bool"
  of irLocal: "local"
  of irParam: "param"
  of irEnumValue: "enum-value"
  of irCall: "call"
  of irBuiltinCall: "builtin"
  of irBinary: "binary"
  of irUnary: "unary"
  of irFieldAccess: "field"
  of irLet: "let"
  of irVar: "var"
  of irAssign: "assign"
  of irReturn: "return"
  of irIf: "if"
  of irWhile: "while"
  of irBlock: "block"
  of irBreak: "break"
  of irContinue: "continue"
  of irExprStmt: "expr"

# ---- debugging -------------------------------------------------------------

proc dumpIrRec(e: IrNode, t: TypeTable, depth: int, dest: var string)

proc dumpIndent(depth: int, dest: var string) =
  var i = 0
  while i < depth:
    dest.add("  ")
    inc i

proc dumpChildren(nodes: seq[IrNode], t: TypeTable, depth: int,
                  dest: var string) =
  for n in nodes:
    dumpIrRec(n, t, depth, dest)

proc dumpIrRec(e: IrNode, t: TypeTable, depth: int, dest: var string) =
  if e == nil:
    return
  dumpIndent(depth, dest)
  dest.add(irKindName(e.kind))
  if e.name.len > 0:
    dest.add(" " & e.name)
  if e.ownerName.len > 0:
    dest.add(" (" & e.ownerName & ")")
  case e.kind
  of irIntLit: dest.add(" " & $e.intVal)
  of irFloatLit: dest.add(" " & $e.floatVal)
  of irStringLit: dest.add(" \"" & e.strVal & "\"")
  of irBoolLit: dest.add(" " & (if e.boolVal: "true" else: "false"))
  of irLet, irVar, irReturn: dest.add(": " & typeName(t, e.ty))
  else: discard
  dest.add("\n")
  for arg in e.args:
    dumpIrRec(arg, t, depth + 1, dest)
  dumpChildren(e.thenBody, t, depth + 1, dest)
  dumpChildren(e.elseBody, t, depth + 1, dest)

proc dumpIrModule*(m: IrModule, t: TypeTable): string =
  result = ""
  for s in m.structs:
    result.add("struct " & s.name & "\n")
    for f in s.fields:
      result.add("  " & f.name & ": " & typeName(t, f.ty) & "\n")
  for e in m.enums:
    result.add("enum " & e.name & "\n")
    for v in e.values:
      result.add("  " & v.name & " = " & $v.value & "\n")
  for fn in m.fns:
    result.add("fn " & fn.name & "(")
    for i in 0 ..< fn.params.len:
      if i > 0:
        result.add(", ")
      result.add(fn.params[i].name & ": " & typeName(t, fn.params[i].ty))
    result.add(")")
    if kindOf(t, fn.returnType) != tyVoid:
      result.add(" -> " & typeName(t, fn.returnType))
    result.add("\n")
    for s in fn.body:
      dumpIrRec(s, t, 1, result)
