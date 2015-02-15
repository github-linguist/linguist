import strutils

proc `$` (x: seq[string]): string =
   result = x.join("")

echo(split("She was a soul stripper. She took my heart!", {'a','e','i'}).join(""))

# using the above proc to overload the toString operator `$` as an alternative to using join()
echo($split("She was a soul stripper. She took my heart!", {'a','e','i'}))
