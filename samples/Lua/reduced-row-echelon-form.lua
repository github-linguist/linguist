function ToReducedRowEchelonForm ( M )
    local lead = 1
    local n_rows, n_cols = #M, #M[1]

    for r = 1, n_rows do
        if n_cols <= lead then break end

        local i = r
        while M[i][lead] == 0 do
            i = i + 1
            if n_rows == i then
                i = r
                lead = lead + 1
                if n_cols == lead then break end
            end
        end
        M[i], M[r] = M[r], M[i]

        local m = M[r][lead]
        for k = 1, n_cols do
            M[r][k] = M[r][k] / m
        end
        for i = 1, n_rows do
            if i ~= r then
                local m = M[i][lead]
                for k = 1, n_cols do
                    M[i][k] = M[i][k] - m * M[r][k]
                end
            end
        end
        lead = lead + 1
    end
end

M = { { 1, 2, -1, -4 },
      { 2, 3, -1, -11 },
      { -2, 0, -3, 22 } }

res = ToReducedRowEchelonForm( M )

for i = 1, #M do
    for j = 1, #M[1] do
        io.write( M[i][j], "  " )
    end
    io.write( "\n" )
end
