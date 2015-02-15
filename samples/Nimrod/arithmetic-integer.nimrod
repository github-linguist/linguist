import parseopt,strutils

var
  opt: TOptParser = initOptParser()
  str = opt.cmdLineRest.split
  a: int = 0
  b: int = 0

try:
  a = parseInt(str[0])
  b = parseInt(str[1])
except EinvalidValue:
  quit("Invalid params. Two integers are expected.")


echo ("a      : " & $a)
echo ("b      : " & $b)
echo ("a + b  : " & $(a+b))
echo ("a - b  : " & $(a-b))
echo ("a * b  : " & $(a*b))
echo ("a div b: " & $(a div b))
echo ("a mod b: " & $(a mod b))
