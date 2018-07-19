declare
  %% e.g. "21" -> "1211"
  fun {LookAndSayString S}
     for DigitGroup in {Group S} append:Add do
        {Add {Int.toString {Length DigitGroup}}}
        {Add [DigitGroup.1]}
     end
  end

  %% lazy sequence of integers starting with N
  fun {LookAndSay N}
     fun lazy {Loop S}
        {String.toInt S}|{Loop {LookAndSayString S}}
     end
  in
     {Loop {Int.toString N}}
  end

  %% like Haskell's "group"
  fun {Group Xs}
     case Xs of nil then nil
     [] X|Xr then
	Ys Zs
        {List.takeDropWhile Xr fun {$ W} W==X end ?Ys ?Zs}
     in
        (X|Ys) | {Group Zs}
     end
  end
in
  {ForAll {List.take {LookAndSay 1} 10} Show}
