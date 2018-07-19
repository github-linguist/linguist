function montepi(n)
    s = 0
    for i = 1:n
        s += rand()^2 + rand()^2 < 1
    end
    return 4*s/n
end
