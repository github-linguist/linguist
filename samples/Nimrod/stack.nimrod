import math

type
  EStackEmpty = object of E_Base

  TStack* [A] = object
    data: seq[A]
    count: int

proc initStack*[A](initialSize = 32): TStack[A] =
  assert isPowerOfTwo(initialSize)
  result.count = 0
  newSeq(result.data,initialSize)

proc cap*[A] (s: TStack[A]): int =
  result = s.data.len

proc len*[A](stack: TStack[A]): int =
  result = stack.count

proc push*[A](s: var TStack[A], item: A) =
  if s.count == s.data.len:
    # not enough room, make container bigger
    var d: Seq[A]
    newSeq(d,s.len * 2)
    for i in 0 .. s.data.len - 1:
      shallowCopy(d[i],s.data[i])
    shallowCopy(s.data,d)
  s.data[s.count] = item
  inc(s.count)

proc pop*[A](s: var TStack[A]): A {.raises: [EStackEmpty].}=
  if s.count == 0:
    raise newException(EStackEmpty,"the stack is empty")
  dec(s.count)
  result = s.data[s.count]

proc top*[A](s: TStack[A]): A =
  result = s.data[s.count - 1]

proc isEmpty*[A](s: var TStack[A]): bool =
  return s.count == 0

#Tests
when isMainModule:
  var stk: TStack[char] = initStack[char](4)
  stk.push('a')
  stk.push('b')
  stk.push('c')
  stk.push('d')

  assert(stk.count == 4)
  assert(stk.data.len == 4)
  stk.push('e')
  assert(stk.cap == 8)
  assert(stk.top == 'e')


  discard stk.pop
  discard stk.pop
  discard stk.pop
  discard stk.pop
  assert(stk.isEmpty == false)
  discard stk.pop
  assert(stk.isEmpty == true)

  try:
    discard stk.pop
  except:
    let
      e = getCurrentException()
      msg = getCurrentExceptionMsg()
    echo "Exception: [[", repr(e), "]] msg: ", msg
