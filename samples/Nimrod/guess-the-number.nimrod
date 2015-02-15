import strutils, math

randomize()
var chosen = 1 + random(10)
echo "I have thought of a number. Try to guess it!"

var guess = parseInt(readLine(stdin))

while guess != chosen:
  echo "Your guess was wrong. Try again!"
  guess = parseInt(readLine(stdin))

echo "Well guessed!"
