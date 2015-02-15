property outputText: ""
repeat with i from 1 to 100
  if i mod 15 = 0 then
    set outputText to outputText & "FizzBuzz"
  else if i mod 3 = 0 then
    set outputText to outputText & "Fizz"
  else if i mod 5 = 0 then
    set outputText to outputText & "Buzz"
  else
    set outputText to outputText & i
  end if
  set outputText to outputText & linefeed
end repeat
outputText
