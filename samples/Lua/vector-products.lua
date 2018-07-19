Vector = {}
function Vector.new( _x, _y, _z )
    return { x=_x, y=_y, z=_z }
end

function Vector.dot( A, B )
    return A.x*B.x + A.y*B.y + A.z*B.z
end

function Vector.cross( A, B )
    return { x = A.y*B.z - A.z*B.y,
             y = A.z*B.x - A.x*B.z,
             z = A.x*B.y - A.y*B.x }
end

function Vector.scalar_triple( A, B, C )
    return Vector.dot( A, Vector.cross( B, C ) )
end

function Vector.vector_triple( A, B, C )
    return Vector.cross( A, Vector.cross( B, C ) )
end


A = Vector.new( 3, 4, 5 )
B = Vector.new( 4, 3, 5 )
C = Vector.new( -5, -12, -13 )

print( Vector.dot( A, B ) )

r = Vector.cross(A, B )
print( r.x, r.y, r.z )

print( Vector.scalar_triple( A, B, C ) )

r = Vector.vector_triple( A, B, C )
print( r.x, r.y, r.z )
