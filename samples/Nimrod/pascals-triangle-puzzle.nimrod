import math, strutils

var B_X, B_Y, B_Z : int = 0

type
   Block_Value = object
      Known   : int
      X, Y, Z : int

let
   X: Block_Value = Block_Value(Known:0, X:1, Y:0, Z:0)
   Y: Block_Value = Block_Value(Known:0, X:0, Y:1, Z:0)
   Z: Block_Value = Block_Value(Known:0, X:0, Y:0, Z:1)

proc Add (L : var Block_Value, R : Block_Value) =
   # Symbolically adds one block to another
   L.Known = L.Known + R.Known
   L.X = L.X + R.X - R.Z    # Z is excluded as n(Y - X - Z) = 0
   L.Y = L.Y + R.Y + R.Z

proc Add (L: var Block_Value, R: int) =
   # Symbolically adds a value to the block
   L.Known = L.Known + R

proc Image (N : Block_Value): string =
   # The block value, when X,Y,Z are known
   result = $(N.Known + N.X * B_X + N.Y * B_Y + N.Z * B_Z)

proc Solve_2x2 (A11: int, A12:int, B1:int, A21:int, A22:int, B2: int) =
   # Don't care about things, supposing an integer solution exists
   if A22 == 0:
      B_X = toInt(B2 / A21)
      B_Y = toInt((B1 - (A11*B_X)) / A12)
   else:
      B_X = toInt((B1*A22 - B2*A12) / (A11*A22 - A21*A12))
      B_Y = toInt((B1 - A11*B_X) / A12)
   B_Z = B_Y - B_X

var B : array [1..5, array[1..5, Block_Value]]   # The lower triangle contains blocks

# The bottom blocks
Add(B[5][1],X)
Add(B[5][2],11)
Add(B[5][3],Y)
Add(B[5][4],4)
Add(B[5][5],Z)

# Upward run
for Row in countdown(4,1):
   for Column in 1 .. Row:
      Add (B[Row][Column], B[Row + 1][Column])
      Add (B[Row][Column], B[Row + 1][Column + 1])

# Now have known blocks 40=[3][1], 151=[1][1] and Y=X+Z to determine X,Y,Z
Solve_2x2( B[1][1].X,
           B[1][1].Y,
           151 - B[1][1].Known,
           B[3][1].X,
           B[3][1].Y,
           40 - B[3][1].Known)

#Print the results
for Row in 1..5:
   writeln(stdout,"")
   for Column in 1..Row:
      write(stdout, Image(B[Row][Column]), " ")
