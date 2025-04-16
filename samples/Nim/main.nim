iterator fn1: int = yield 12
template fn2 = discard
macro fn3 = discard
proc fn4 = discard
proc fn5(a, b: int, c: float, d, e: string; f: char) = discard

template fn6 =
  block: ## number literals
    echo (1'u8, 0b1010, 0xdeadBEEF'big, -123'32, 12_345.002e-12'f128)
    let a = 0xdeadBEEF'wrap
    let b = -123'wrap

  block: ## char literals
    let c = '\1'
    let c = '\e'
    let c = '\n'
    let c = '\x12'

  block: ## string literals
    let a = ""
    let b = "ab\ndef"
    let c = r"a\ndef"
    let g = """
a\ndef"""
    let h = """a\ndef"""
    let i = """a\ndef""".dedent # BUG: syntax highlight wrong
    let j = """a\ndef"""&"asdf" # BUG: syntax highlight wrong
    let k = """a\ndef""" & "asdf" # ok

  block:
    ## tuples
    let a = (-123'wrap, 12'f33, )
    let b = ()
    let c = (1,)
    let d = (1)

  block:
    let a = @[1]
    let b = [1,2]

proc `'fn6b`(a: string): int = discard
proc `cast`(a: string): int = discard
proc fn6c[T; T2](a: T): T = discard
proc fn6d[T, T2](a: T): T = discard
proc fn6e[T](a: T): T = discard

runnableExamples: discard

runnableExamples("-r:off"): discard

proc fn7 =
  runnableExamples("-r:off -d:danger"):
    assert 1 + 1 == 2

proc fn8 =
  runnableExamples("-r:off -d:danger"):
    assert 1 + 1 == 2

proc fn9 =
  ##[
  hello world `foo.bar`
  * b1
  * b2
  * b3
  ]##
  runnableExamples("-r:off -d:danger"):
    assert 1 + 1 == 2

type F10 = enum a1, a2
type F11 = ref object of RootRef
  x1, x2: int
  x3: tuple[a: int]
  x4: (int, float, float32, float64)

type
  F12 = int
  F13 = ref F12

const a = 1
const
  b = 2
  c = b
let d = "ad"
var e = "ad"
