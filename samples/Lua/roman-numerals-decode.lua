function ToNumeral( roman )
    local Num = { ["M"] = 1000, ["D"] = 500, ["C"] = 100, ["L"] = 50, ["X"] = 10, ["V"] = 5, ["I"] = 1 }
    local numeral = 0

    local i = 1
    local strlen = string.len(roman)
    while i < strlen do
        local z1, z2 = Num[ string.sub(roman,i,i) ], Num[ string.sub(roman,i+1,i+1) ]
        if z1 < z2 then
            numeral = numeral + ( z2 - z1 )
            i = i + 2
        else
            numeral = numeral + z1
            i = i + 1
        end
    end

    if i <= strlen then numeral = numeral + Num[ string.sub(roman,i,i) ] end

    return numeral
end

print( ToNumeral( "MCMXC" ) )
print( ToNumeral( "MMVIII" ) )
print( ToNumeral( "MDCLXVI" ) )
