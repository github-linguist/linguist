   A=: 1
   B=: 2
   C=: 3
   F=: verb define
      A=:4
      B=.5
      D=.6
      A+B+C+D
   )
   F ''
18
   A
4
   B
2
   D
|value error
