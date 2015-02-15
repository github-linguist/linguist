SelectionSort := function(v)
   local i, j, k, n, m;
   n := Size(v);
   for i in [1 .. n] do
      k := i;
      m := v[i];
      for j in [i + 1 .. n] do
         if v[j] < m then
            k := j;
            m := v[j];
         fi;
      od;
      v[k] := v[i];
      v[i] := m;
   od;
end;

v := List([1 .. 100], n -> Random([1 .. 100]));
SelectionSort(v);
v;
