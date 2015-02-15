declare
  fun {Comb M N}
     proc {CombScript Comb}
        %% Comb is a subset of [0..N-1]
        Comb = {FS.var.upperBound {List.number 0 N-1 1}}
        %% Comb has cardinality M
        {FS.card Comb M}
        %% enumerate all possibilities
        {FS.distribute naive [Comb]}
     end
  in
     %% Collect all solutions and convert to lists
     {Map {SearchAll CombScript} FS.reflect.upperBoundList}
  end
in
  {Inspect {Comb 3 5}}
