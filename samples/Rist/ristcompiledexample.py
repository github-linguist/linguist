import os
import sys

from time import sleep as _sleep
from typing import Any, Callable


def clear() -> None:
  os.system("cls" if os.name == "nt" else "clear")

def wait(seconds: int):
  _sleep(seconds)

def waitAndDo(
  seconds: int,
  func: Callable,
  *fargs: Any,
  **fkwargs
) -> Any:
  wait(seconds)
  call = func
  return func(*fargs, **fkwargs)

print("1. Clear Screen\n2. Exit\n")
inp: str = input("Please enter what you wanna do: ")
try:
  inp: int = int(inp)
except:
  raise TypeError("Please enter a number")

if (inp < 1) or (inp > 2):
  raise TypeError("Only 1 and 2 are allowed")

inp -= 1
if not inp:
  # it was 1
  print("Clearing in 3 seconds...")
  waitAndDo(3, clear)
else:
  # it was 2
  print("Exiting in 3 seconds...")
  waitAndDo(3, sys.exit, 0) 
