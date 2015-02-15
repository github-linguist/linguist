declare
  A0 = {List.toTuple unit "_###_##_#_#_#_#__#__"}

  MaxGenerations = 9

  Rules = unit('___':&_
               '__#':&_
               '_#_':&_
               '_##':&#
               '#__':&_
               '#_#':&#
               '##_':&#
               '###':&_)

  fun {Evolve A}
     {Record.mapInd A
      fun {$ I V}
         Left = {CondSelect A I-1 &_}
         Right = {CondSelect A I+1 &_}
         Env = {String.toAtom [Left V Right]}
      in
         Rules.Env
      end
     }
  end

  fun lazy {Iterate X F}
     X|{Iterate {F X} F}
  end
in
  for
     I in 0..MaxGenerations
     A in {Iterate A0 Evolve}
  do
     {System.showInfo "Gen. "#I#": "#{Record.toList A}}
  end
