declare
  MyMap = unit('hello':13 'world':31 '!':71)
in
  {ForAll {Record.toListInd MyMap} Show}  %% pairs
  {ForAll {Record.arity     MyMap} Show}  %% keys
  {ForAll {Record.toList    MyMap} Show}  %% values
