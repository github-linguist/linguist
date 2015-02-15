declare
  StdIn = {New class $ from Open.file Open.text end init(name:stdin)}

  fun {ReadInt}
     {String.toInt {StdIn getS($)}}
  end

  A = {ReadInt}
  B = {ReadInt}
in
  {ForAll
   ["A+B = "#A+B
    "A-B = "#A-B
    "A*B = "#A*B
    "A/B = "#A div B  %% truncates towards 0
    "remainder "#A mod B  %% has the same sign as A
    "A^B = "#{Pow A B}
   ]
   System.showInfo}
