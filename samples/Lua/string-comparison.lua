function compare(a, b)
    print(("%s is of type %s and %s is of type %s"):format(
        a, type(a),
        b, type(b)
    ))
    if a <  b then print(('%s is strictly less than %s'):format(a, b)) end
    if a <= b then print(('%s is less than or equal to %s'):format(a, b)) end
    if a >  b then print(('%s is strictly greater than %s'):format(a, b)) end
    if a >= b then print(('%s is greater than or equal to %s'):format(a, b)) end
    if a == b then print(('%s is equal to %s'):format(a, b)) end
    if a ~= b then print(('%s is not equal to %s'):format(a, b)) end
    print ""
end

compare('YUP', 'YUP')
compare('BALL', 'BELL')
compare('24', '123')
compare(24, 123)
compare(5.0, 5)
