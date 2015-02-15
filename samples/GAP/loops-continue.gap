for i in [1 .. 11] do
    if RemInt(i, 5) = 0 then
        Print(i, "\n");
        continue;
    fi;
    Print(i, ", ");
od;

# 1, 2, 3, 4, 5
# 6, 7, 8, 9, 10
