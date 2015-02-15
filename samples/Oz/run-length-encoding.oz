declare
  fun {RLEncode Xs}
     for G in {Group Xs} collect:C do
	{C {Length G}#G.1}
     end
  end

  fun {RLDecode Xs}
     for C#Y in Xs append:Ap do
	{Ap {Replicate Y C}}
     end
  end

  %% Helpers
  %% e.g. "1122" -> ["11" "22"]
  fun {Group Xs}
     case Xs of nil then nil
     [] X|Xr then
	Ys Zs
        {List.takeDropWhile Xr fun {$ W} W==X end ?Ys ?Zs}
     in
        (X|Ys) | {Group Zs}
     end
  end
  %% e.g. 3,4 -> [3 3 3 3]
  fun {Replicate X N}
     case N of 0 then nil
     else X|{Replicate X N-1}
     end
  end

  Data = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
  Enc = {RLEncode Data}
in
  {System.showInfo Data}
  {Show Enc}
  {System.showInfo {RLDecode Enc}}
