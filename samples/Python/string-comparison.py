def compare(a, b):
    print("\n%r is of type %r and %r is of type %r"
          % (a, type(a), b, type(b)))
    if a <  b:      print('%r is strictly less than  %r' % (a, b))
    if a <= b:      print('%r is less than or equal to %r' % (a, b))
    if a >  b:      print('%r is strictly greater than  %r' % (a, b))
    if a >= b:      print('%r is greater than or equal to %r' % (a, b))
    if a == b:      print('%r is equal to %r' % (a, b))
    if a != b:      print('%r is not equal to %r' % (a, b))
    if a is b:      print('%r has object identity with %r' % (a, b))
    if a is not b:  print('%r has negated object identity with %r' % (a, b))

compare('YUP', 'YUP')
compare('BALL', 'BELL')
compare('24', '123')
compare(24, 123)
compare(5.0, 5)
