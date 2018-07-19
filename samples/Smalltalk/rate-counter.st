|times|
times := Bag new.
1 to: 10 do: [:n| times add:
   (Time millisecondsToRun: [3000 factorial])].
Transcript show: times average asInteger.
