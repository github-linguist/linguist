function identity_matrix (size)
        local m = {}
        for i = 1, size do
                m[i] = {}
                for j = 1, size do
                        m[i][j] = i == j and 1 or 0
                end
        end
        return m
end

function print_matrix (m)
        for i = 1, #m do
                print(table.concat(m[i], " "))
        end
end

print_matrix(identity_matrix(5))
