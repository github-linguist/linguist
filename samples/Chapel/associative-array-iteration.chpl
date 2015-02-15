var A = [ "H2O" => "water", "NaCl" => "salt", "O2" => "oxygen" ];

for k in A.domain do
    writeln("have key: ", k);

for v in A do
    writeln("have value: ", v);

for (k,v) in zip(A.domain, A) do
    writeln("have element: ", k, " -> ", v);
