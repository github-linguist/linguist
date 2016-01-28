import "lib/pratttest"
local a = goexp 1 + 3 * 4 ^ 5 ^ 6/(2 - 4 + -a) + -b(c)

terralib.tree.printraw(a)
