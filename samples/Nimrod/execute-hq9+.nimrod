var program = "9hHqQ+"
var i = 0

proc bottle(n: int): string =
  case n
  of 0:
    result = "No more bottles"
  of 1:
    result = "1 bottle"
  else:
    result = $n & " bottles"

proc ninetyNineBottles =
  for n in countdown(99, 1):
    echo bottle(n), " bottle of beer on the wall"
    echo bottle(n), " bottle of beer"
    echo "Take one down, pass it around"
    echo bottle(n - 1), " of beer on the wall"

for token in items(program):
  case token
  of 'h', 'H':
    echo("Hello, world!")
  of 'q', 'Q':
    echo(program)
  of '9':
    ninetyNineBottles()
  of '+':
    inc(i)
  else:
    echo("Unknown command: ", token)
