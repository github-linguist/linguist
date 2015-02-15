import math

proc isperfect(n: int): bool =
    let max: int = (sqrt(n.toFloat)).toInt
    var sum: int = 1
    for i in 2..max:
        if n mod i == 0:
            let q: int = n div i
            sum += (i + q)
    return (n == sum)

for i in 2..10_000:
    if isperfect(i):
        echo(i)
