# from
# http://wiki.erights.org/wiki/Walnut/Ordinary_Programming/Objects_and_Functions
def makeCar(var name) {
    var x := 0
    var y := 0
    def car {
        to moveTo(newX,newY) {
            x := newX
            y := newY
        }
        to getX() {return x}
        to getY() {return y}
        to setName(newName) {name := newName}
        to getName() {return name}
    } 
    return car
}
# Now use the makeCar function to make a car, which we will move and print
def sportsCar := makeCar("Ferrari")
sportsCar.moveTo(10,20)
println(`The car ${sportsCar.getName()} is at X location ${sportsCar.getX()}`)
