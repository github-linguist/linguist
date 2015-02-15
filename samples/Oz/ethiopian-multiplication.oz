declare
  fun {Halve X}   X div 2             end
  fun {Double X}  X * 2               end
  fun {Even X}    {Abs X mod 2} == 0  end  %% standard function: Int.isEven

  fun {EthiopicMult X Y}
     X >= 0 = true %% assert: X must not be negative

     Rows = for
               L in X; L>0;  {Halve L}  %% C-like iterator: "Init; While; Next"
               R in Y; true; {Double R}
               collect:Collect
	    do
	       {Collect L#R}
	    end

     OddRows = {Filter Rows LeftIsOdd}
     RightColumn = {Map OddRows SelectRight}
  in
     {Sum RightColumn}
  end

  %% Helpers
  fun {LeftIsOdd L#_}   {Not {Even L}}          end
  fun {SelectRight _#R} R                       end
  fun {Sum Xs}          {FoldL Xs Number.'+' 0} end
in
  {Show {EthiopicMult 17 34}}
