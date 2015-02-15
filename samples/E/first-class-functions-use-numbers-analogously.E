def x := 2.0
def xi := 0.5
def y := 4.0
def yi := 0.25
def z := x + y
def zi := 1.0 / (x + y)
def forward := [x,  y,  z ]
def reverse := [xi, yi, zi]

def multiplier(a, b) {
    return fn x { a * b * x }
}

def s := 0.5
for i => a in forward {
    def b := reverse[i]
    println(`s = $s, a = $a, b = $b, multiplier($a, $b)($s) = ${multiplier(a, b)(s)}`)
}
