with(ListTools):

SumList := proc(L)
    local s, x;
    s := 0;
    for x in L do
        s := s + x;
    end do;
    return s;
end proc;

SumList([1,5,10,20]);
