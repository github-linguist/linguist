def pi := (-1.0).acos()

def radians := pi / 4.0
def degrees := 45.0

def d2r := (pi/180).multiply
def r2d := (180/pi).multiply

println(`$\
${radians.sin()} ${d2r(degrees).sin()}
${radians.cos()} ${d2r(degrees).cos()}
${radians.tan()} ${d2r(degrees).tan()}
${def asin := radians.sin().asin()} ${r2d(asin)}
${def acos := radians.cos().acos()} ${r2d(acos)}
${def atan := radians.tan().atan()} ${r2d(atan)}
`)
