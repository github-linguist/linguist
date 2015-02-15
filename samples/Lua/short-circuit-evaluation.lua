function a(i)
    print "Function a(i) called."
    return i
end

function b(i)
    print "Function b(i) called."
    return i
end

i = true
x = a(i) and b(i);  print ""
y = a(i) or  b(i);  print ""

i = false
x = a(i) and b(i);  print ""
y = a(i) or  b(i)
