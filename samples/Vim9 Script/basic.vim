vim9script

var myNumber: number = 42
const PI: number = 3.14159
final myList: list<string> = ['vim', 'neovim', 'vim9']

def HelloWorld(): void
  echo "Hello, Vim9!"
enddef

def Add(a: number, b: number): number
  return a + b
enddef

def ProcessList(items: list<string>): dict<number>
  var result: dict<number> = {}
  for item in items
    result[item] = len(item)
  endfor
  return result
enddef

var double = (n: number): number => n * 2
var squared = (n: number): number => n * n

def CheckValue(val: number): string
  if val > 0
    return "positive"
  elseif val < 0
    return "negative"
  else
    return "zero"
  endif
enddef

var name = "Vim9"
echo $"Welcome to {name}!"

export def PublicFunction(): void
  echo "This function is exported"
enddef

def SafeDivide(a: number, b: number): number
  try
    return a / b
  catch
    echoerr "Division error"
    return 0
  endtry
enddef

var isEnabled: bool = true
var isDisabled: bool = false

HelloWorld()
var sum = Add(10, 20)
echo $"Sum: {sum}"
