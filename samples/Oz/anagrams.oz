declare
  %% Helper function
  fun {ReadLines Filename}
     File = {New class $ from Open.file Open.text end init(name:Filename)}
  in
     for collect:C break:B do
	case {File getS($)} of false then {File close} {B}
	[] Line then {C Line}
        end
     end
  end

  %% Groups anagrams by using a mutable dictionary
  %% with sorted words as keys
  WordDict = {Dictionary.new}
  for Word in {ReadLines "unixdict.txt"}  do
     Keyword = {String.toAtom {Sort Word Value.'<'}}
  in
     WordDict.Keyword := Word|{CondSelect WordDict Keyword nil}
  end
  Sets = {Dictionary.items WordDict}

  %% Filter such that only the largest sets remain
  MaxSetSize = {FoldL {Map Sets Length} Max 0}
  LargestSets = {Filter Sets fun {$ S} {Length S} == MaxSetSize end}
in
  %% Display result (make sure strings are shown as string, not as number lists)
  {Inspector.object configureEntry(widgetShowStrings true)}
  {Inspect LargestSets}
