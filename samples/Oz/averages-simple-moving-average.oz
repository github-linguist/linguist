declare

  fun {CreateSMA Period}
     Xs = {NewCell nil}
  in
     fun {$ X}
        Xs := {List.take X|@Xs Period}

        {FoldL @Xs Number.'+' 0.0}
        /
        {Int.toFloat {Min Period {Length @Xs}}}
     end
  end

in

  for Period in [3 5] do
     SMA = {CreateSMA Period}
  in
     {System.showInfo "\nSTART PERIOD "#Period}
     for I in 1..5 do
        {System.showInfo "  Number = "#I#" , SMA = "#{SMA {Int.toFloat I}}}
     end
     for I in 5..1;~1 do
        {System.showInfo "  Number = "#I#" , SMA = "#{SMA {Int.toFloat I}}}
     end
  end
