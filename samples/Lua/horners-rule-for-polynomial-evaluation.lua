function horners_rule( coeff, x )
    local res = 0
    for i = #coeff, 1, -1 do
        res = res * x + coeff[i]
    end
    return res
end

x = 3
coefficients = { -19, 7, -4, 6 }
print( horners_rule( coefficients, x ) )
