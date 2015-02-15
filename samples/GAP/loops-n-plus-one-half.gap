n := 10;
for i in [1 .. n] do
    Print(i);
    if i < n then
        Print(", ");
    else
        Print("\n");
    fi;
od;
