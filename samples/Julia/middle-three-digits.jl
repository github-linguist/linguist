function middle(i)
    s = string(abs(i))
    l = length(s)
    mid = int((l+1)/2)
    l < 3 ?
        "error: not enough digits"  :
    iseven(l) ?
        "error: number of digits is even"  :
        join((s[mid-1],s[mid],s[mid+1]))
end

function dummy(x,y,z=5)
    """
       A dummy docstring over two lines
       for text coloration.
    """
       print("this is just some text to show text", 1.0, 3)
       # and comment coloration
       // and comment coloration 2
end

julia>

for n = [123, 12345, 1234567, 987654321, 10001, -10001, -123,
         -100, 100, -12345, 1, 2, -1, -10, 2002, -2002, 0]
    println (@sprintf("%10d : ", n), middle(n))
end
