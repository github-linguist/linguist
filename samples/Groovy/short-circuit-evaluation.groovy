def f = { println '  AHA!'; it instanceof String }
def g = { printf ('%5d ', it); it > 50 }

println 'bitwise'
assert g(100) & f('sss')
assert g(2) | f('sss')
assert ! (g(1) & f('sss'))
assert g(200) | f('sss')

println '''
logical'''
assert g(100) && f('sss')
assert g(2) || f('sss')
assert ! (g(1) && f('sss'))
assert g(200) || f('sss')
