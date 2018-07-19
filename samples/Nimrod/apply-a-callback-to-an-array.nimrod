var arr = [1,2,3,4]
arr.map proc(some: var int) = echo(some, " squared = ", some*some)
