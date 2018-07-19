function MatMul( m1, m2 )
    if #m1[1] ~= #m2 then       -- inner matrix-dimensions must agree
        return nil
    end

    local res = {}

    for i = 1, #m1 do
        res[i] = {}
        for j = 1, #m2[1] do
            res[i][j] = 0
            for k = 1, #m2 do
                res[i][j] = res[i][j] + m1[i][k] * m2[k][j]
            end
        end
    end

    return res
end

-- Test for MatMul
mat1 = { { 1, 2, 3 }, { 4, 5, 6 } }
mat2 = { { 1, 2 }, { 3, 4 }, { 5, 6 } }
erg = MatMul( mat1, mat2 )
for i = 1, #erg do
    for j = 1, #erg[1] do
        io.write( erg[i][j] )
        io.write("  ")
    end
    io.write("\n")
end
