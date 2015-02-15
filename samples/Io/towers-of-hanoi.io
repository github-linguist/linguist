hanoi := method(n, from, to, via,
  if (n == 1) then (
    writeln("Move from ", from, " to ", to)
  ) else (
    hanoi(n - 1, from, via, to  )
    hanoi(1    , from, to , via )
    hanoi(n - 1, via , to , from)
  )
)
