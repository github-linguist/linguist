import strutils

proc jumpBackward(pos: var int, program: string) =
  var level = 1
  while pos > 0 and level != 0:
    dec pos
    case program[pos]
    of '[':
      dec level
    of ']':
      inc level
    else:
      discard 1
  dec pos

proc jumpForward(pos: var int, program: string) =
  var level = 1
  while pos < program.len and level != 0:
    inc pos
    case program[pos]
    of ']':
      inc level
    of '[':
      dec level
    else:
      discard 1

proc bf(program: string) =
  var tape: array[0..20, int]
  var pointer = 0
  var pos = 0
  var indent = 0

  while pos < program.len:
    var token = program[pos]
    case token
    of '+':
      inc tape[pointer]
    of '-':
      dec tape[pointer]
    of ',':
      tape[pointer] = int(stdin.readChar())
    of '.':
      stdout.write(chr(tape[pointer]))
    of '[':
      if tape[pointer] == 0:
        jumpForward(pos, program)
    of ']':
      if tape[pointer] != 0:
        jumpBackward(pos, program)
    of '>':
      inc pointer
    of '<':
      dec pointer
    else:
      discard 1
    inc pos

var addition = ",>++++++[<-------->-],[<+>-]<."
var hello_world = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

bf(addition)
# bf(hello_world)
