/** Guard accepting only objects with an 'eat' method */
def Eatable {
    to coerce(specimen, ejector) {
        if (Ref.isNear(specimen) && specimen.__respondsTo("eat", 0)) {
            return specimen
        } else {
            throw.eject(ejector, `inedible: $specimen`)
        }
    }
}

def makeFoodBox() {
    return [].diverge(Eatable) # A guard-constrained list
}
