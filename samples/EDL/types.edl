## The EDL type system.
##
## Types are interned in a `TypeTable` and referred to by a small integer,
## `TypeId`. Builtin kinds are registered first, in `TypeKind` order, so the id
## of a builtin is exactly its kind ordinal -- `tid(k)` relies on that invariant
## and tests assert it.
##
## Types not implemented yet (i128, u128, bytes) are *named* here so the
## compiler can tell "unknown type" from "specified but not implemented", and
## report the second one with a pointer to the roadmap instead of a bare error.
##
## Bootstrap dialect: see specs/decisions/ADR-0001.

type
  TypeKind* = enum
    tyError       ## a type error occurred; suppresses cascading diagnostics
    tyVoid
    tyBool
    tyI8
    tyI16
    tyI32
    tyI64
    tyI128
    tyU8
    tyU16
    tyU32
    tyU64
    tyU128
    tyF32
    tyF64
    tyChar
    tyString
    tyBytes
    tyIsize
    tyUsize
    tyNamed       ## a struct or enum, identified by its declaration index
    tyFn          ## reserved: function types are not implemented yet
    tyUnknown     ## not inferred yet; suppresses cascading diagnostics

  TypeId* = int

  TypeInfo* = object
    kind*: TypeKind
    name*: string            ## set for tyNamed, and for builtins
    sym*: int                ## declaration index for tyNamed, -1 otherwise
    paramTypes*: seq[TypeId] ## reserved for tyFn
    returnType*: TypeId      ## reserved for tyFn

  TypeTable* = ref object
    infos*: seq[TypeInfo]

proc kindName*(k: TypeKind): string =
  case k
  of tyError: "<error>"
  of tyVoid: "void"
  of tyBool: "bool"
  of tyI8: "i8"
  of tyI16: "i16"
  of tyI32: "i32"
  of tyI64: "i64"
  of tyI128: "i128"
  of tyU8: "u8"
  of tyU16: "u16"
  of tyU32: "u32"
  of tyU64: "u64"
  of tyU128: "u128"
  of tyF32: "f32"
  of tyF64: "f64"
  of tyChar: "char"
  of tyString: "string"
  of tyBytes: "bytes"
  of tyIsize: "isize"
  of tyUsize: "usize"
  of tyNamed: "<named>"
  of tyFn: "<fn>"
  of tyUnknown: "<unknown>"

proc tid*(k: TypeKind): TypeId =
  ## Id of a builtin kind. Valid for every `TypeKind`, by construction of
  ## `newTypeTable`.
  result = TypeId(ord(k))

proc newTypeTable*(): TypeTable =
  ## Registers every `TypeKind` as its own entry, in order.
  result = TypeTable(infos: @[])
  for k in TypeKind:
    result.infos.add(TypeInfo(kind: k, name: kindName(k), sym: -1,
                              paramTypes: @[], returnType: tid(tyVoid)))

proc kindOf*(t: TypeTable, id: TypeId): TypeKind =
  if id < 0 or id >= t.infos.len:
    result = tyError
  else:
    result = t.infos[id].kind

proc infoOf*(t: TypeTable, id: TypeId): TypeInfo =
  if id < 0 or id >= t.infos.len:
    result = TypeInfo(kind: tyError, name: "<invalid>", sym: -1)
  else:
    result = t.infos[id]

proc typeName*(t: TypeTable, id: TypeId): string =
  let info = infoOf(t, id)
  case info.kind
  of tyNamed:
    if info.name.len > 0:
      result = info.name
    else:
      result = "<named# " & $info.sym & ">"
  else:
    result = info.name

proc addNamedType*(t: TypeTable, name: string, sym: int): TypeId =
  ## Registers a struct or enum type. `sym` uniquely identifies the declaration.
  result = TypeId(t.infos.len)
  t.infos.add(TypeInfo(kind: tyNamed, name: name, sym: sym,
                       paramTypes: @[], returnType: tid(tyVoid)))

proc addFnType*(t: TypeTable, paramTypes: seq[TypeId],
                returnType: TypeId): TypeId =
  result = TypeId(t.infos.len)
  t.infos.add(TypeInfo(kind: tyFn, name: "<fn>", sym: -1,
                       paramTypes: paramTypes, returnType: returnType))

proc sameType*(t: TypeTable, a, b: TypeId): bool =
  ## Structural identity of two types.
  if a == b:
    return true
  let ka = kindOf(t, a)
  if ka != kindOf(t, b):
    return false
  if ka == tyNamed:
    let sa = infoOf(t, a).sym
    let sb = infoOf(t, b).sym
    result = sa >= 0 and sa == sb
  else:
    result = false

# ---- classification --------------------------------------------------------

proc isSignedInt*(k: TypeKind): bool =
  result = k.ord >= tyI8.ord and k.ord <= tyI128.ord

proc isUnsignedInt*(k: TypeKind): bool =
  result = k.ord >= tyU8.ord and k.ord <= tyU128.ord

proc isIntegerKind*(k: TypeKind): bool =
  result = isSignedInt(k) or isUnsignedInt(k) or k == tyIsize or k == tyUsize

proc isFloatKind*(k: TypeKind): bool =
  result = k == tyF32 or k == tyF64

proc isNumericKind*(k: TypeKind): bool =
  result = isIntegerKind(k) or isFloatKind(k)

proc isSpecifiedButUnimplemented*(k: TypeKind): bool =
  ## Named in the EDL type universe but not usable yet.
  result = k == tyI128 or k == tyU128 or k == tyBytes

proc isPrintableKind*(k: TypeKind): bool =
  ## Types the temporary `print` builtin accepts.
  result = isNumericKind(k) or k == tyBool or k == tyChar or k == tyString

proc builtinKindByName*(name: string): TypeKind =
  ## Maps a builtin type name to its kind, or `tyError` when the name is not a
  ## builtin EDL type at all (it may still be a struct or enum).
  case name
  of "void": tyVoid
  of "bool": tyBool
  of "i8": tyI8
  of "i16": tyI16
  of "i32": tyI32
  of "i64": tyI64
  of "i128": tyI128
  of "u8": tyU8
  of "u16": tyU16
  of "u32": tyU32
  of "u64": tyU64
  of "u128": tyU128
  of "f32": tyF32
  of "f64": tyF64
  of "char": tyChar
  of "string": tyString
  of "bytes": tyBytes
  of "isize": tyIsize
  of "usize": tyUsize
  else: tyError

# ---- integer literals and ranges -------------------------------------------

proc intBounds*(k: TypeKind): (int64, int64) =
  ## Inclusive value range of an integer type. Literals are lexed into an i64,
  ## so the upper bound of u64 is reported as `high(int64)`: values above it
  ## need a literal syntax that does not exist yet.
  case k
  of tyI8: (-128'i64, 127'i64)
  of tyI16: (-32768'i64, 32767'i64)
  of tyI32: (-2147483648'i64, 2147483647'i64)
  of tyI64, tyIsize: (low(int64), high(int64))
  of tyU8: (0'i64, 255'i64)
  of tyU16: (0'i64, 65535'i64)
  of tyU32: (0'i64, 4294967295'i64)
  of tyU64, tyUsize: (0'i64, high(int64))
  else: (0'i64, 0'i64)

proc litFits*(k: TypeKind, value: int64): bool =
  if not isIntegerKind(k):
    return false
  let bounds = intBounds(k)
  result = value >= bounds[0] and value <= bounds[1]

proc defaultIntType*(v: int64): TypeKind =
  ## Type of an integer literal with no expected type: the smallest signed type
  ## that holds it, falling back to u64 for values above `high(i64)`.
  if litFits(tyI32, v):
    result = tyI32
  elif litFits(tyI64, v):
    result = tyI64
  else:
    result = tyU64
