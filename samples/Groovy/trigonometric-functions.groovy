def radians = Math.PI/4
def degrees = 45

def d2r = { it*Math.PI/180 }
def r2d = { it*180/Math.PI }

println "sin(\u03C0/4) = ${Math.sin(radians)}  == sin(45\u00B0) = ${Math.sin(d2r(degrees))}"
println "cos(\u03C0/4) = ${Math.cos(radians)}  == cos(45\u00B0) = ${Math.cos(d2r(degrees))}"
println "tan(\u03C0/4) = ${Math.tan(radians)}  == tan(45\u00B0) = ${Math.tan(d2r(degrees))}"
println "asin(\u221A2/2) = ${Math.asin(2**(-0.5))} == asin(\u221A2/2)\u00B0 = ${r2d(Math.asin(2**(-0.5)))}\u00B0"
println "acos(\u221A2/2) = ${Math.acos(2**(-0.5))} == acos(\u221A2/2)\u00B0 = ${r2d(Math.acos(2**(-0.5)))}\u00B0"
println "atan(1) = ${Math.atan(1)} == atan(1)\u00B0 = ${r2d(Math.atan(1))}\u00B0"
