i = 1

-- in the event that the number inadvertently wraps around,
-- stop looping - this is unlikely with Lua's default underlying
-- number type (double), but on platform without double
-- the C type used for numbers can be changed
while i > 0 do
    print( i )
    i = i + 1
end
