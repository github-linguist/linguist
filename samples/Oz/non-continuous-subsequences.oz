declare
  fun {NCSubseq SeqList}
     Seq = {FS.value.make SeqList}
     proc {Script Result}
        %% the result is a subset of Seq
        {FS.subset Result Seq}

        %% at least one element of Seq is missing
        local Gap in
           {FS.include Gap Seq}
           {FS.exclude Gap Result}
           %% and this element is between the smallest
           %% and the largest elements of the subsequence
           Gap >: {FS.int.min Result}
           Gap <: {FS.int.max Result}
        end

        %% enumerate all such sets
        {FS.distribute naive [Result]}
     end
  in
     {Map {SearchAll Script} FS.reflect.lowerBoundList}
  end
in
  {Inspect {NCSubseq [1 2 3 4]}}
