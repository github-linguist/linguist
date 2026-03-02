BubbleSort := proc(L)
    local i, j, tmp, n;
    n := nops(L);
    for i from 1 to n do
        for j from 1 to n-i do
            if L[j] > L[j+1] then
                tmp := L[j];
                L[j] := L[j+1];
                L[j+1] := tmp;
            end if;
        end do;
    end do;
    return L;
end proc;

BubbleSort([5,1,4,2,8]);
