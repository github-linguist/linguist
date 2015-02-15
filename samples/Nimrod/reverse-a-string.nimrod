var
  str1 = "Reverse This!"

proc reverse(s: string): string =
  result = ""
  for i in countdown(high(str1), 0):
    result.add str1[i]

echo "Original string: ", str1, "\nReversed: ", reverse(str1)
echo "Using inbuilt array reversal: ", reverse(str1)
