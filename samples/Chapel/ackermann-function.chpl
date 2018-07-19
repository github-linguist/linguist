proc A(m:int, n:int):int {
        if m == 0 then
                return n + 1;
        else if n == 0 then
                return A(m - 1, 1);
        else
                return A(m - 1, A(m, n - 1));
}
