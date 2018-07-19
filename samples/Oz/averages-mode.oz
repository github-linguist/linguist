declare
  fun {Mode Xs}
     Freq = {Dictionary.new}
     for X in Xs do
        Freq.X := {CondSelect Freq X 0} + 1
     end
     MaxCount = {FoldL {Dictionary.items Freq} Max 0}
  in
     for Value#Count in {Dictionary.entries Freq} collect:C do
        if Count == MaxCount then
  	 {C Value}
        end
     end
  end
in
  {Show {Mode [1 2 3 3 2 1 1]}}
  {Show {Mode [1 2 3 3 2 1]}}
