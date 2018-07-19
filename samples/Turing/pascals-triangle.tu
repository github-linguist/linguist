procedure pascal (n : int)
    for i : 0 .. n
        var c : int
        c := 1
        for k : 0 .. i
            put intstr(c) + " " ..
            c := c * (i - k) div (k + 1)
        end for
        put ""
    end for
end pascal

pascal(5)
