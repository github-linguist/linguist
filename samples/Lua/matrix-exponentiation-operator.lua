Matrix = {}

function Matrix.new( dim_y, dim_x )
    assert( dim_y and dim_x )

    local matrix = {}
    local metatab = {}
    setmetatable( matrix, metatab )
    metatab.__add = Matrix.Add
    metatab.__mul = Matrix.Mul
    metatab.__pow = Matrix.Pow

    matrix.dim_y = dim_y
    matrix.dim_x = dim_x

    matrix.data = {}
    for i = 1, dim_y do
        matrix.data[i] = {}
    end
    return matrix
end

function Matrix.Show( m )
    for i = 1, m.dim_y do
        for j = 1, m.dim_x do
            io.write( tostring( m.data[i][j] ), " " )
        end
        io.write( "\n" )
    end
end

function Matrix.Add( m, n )
    assert( m.dim_x == n.dim_x and m.dim_y == n.dim_y )

    local r = Matrix.new( m.dim_y, m.dim_x )
    for i = 1, m.dim_y do
        for j = 1, m.dim_x do
            r.data[i][j] = m.data[i][j] + n.data[i][j]
        end
    end
    return r
end

function Matrix.Mul( m, n )
    assert( m.dim_x == n.dim_y )

    local r = Matrix.new( m.dim_y, n.dim_x )
    for i = 1, m.dim_y do
        for j = 1, n.dim_x do
            r.data[i][j] = 0
            for k = 1, m.dim_x do
                r.data[i][j] = r.data[i][j] + m.data[i][k] * n.data[k][j]
            end
        end
    end
    return r
end

function Matrix.Pow( m, p )
    assert( m.dim_x == m.dim_y )

    local r = Matrix.new( m.dim_y, m.dim_x )

    if p == 0 then
        for i = 1, m.dim_y do
            for j = 1, m.dim_x do
                if i == j then
                    r.data[i][j] = 1
                else
                    r.data[i][j] = 0
                end
            end
        end
    elseif p == 1 then
        for i = 1, m.dim_y do
            for j = 1, m.dim_x do
                r.data[i][j] = m.data[i][j]
            end
        end
    else
        r = m
        for i = 2, p do
            r = r * m
        end
    end

    return r
end


m = Matrix.new( 2, 2 )
m.data = { { 1, 2 }, { 3, 4 } }

n = m^4;

Matrix.Show( n )
