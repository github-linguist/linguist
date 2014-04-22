# from
# http://wiki.erights.org/wiki/Walnut/Ordinary_Programming/Objects_and_Functions
def makeVehicle(self) {
    def vehicle {
        to milesTillEmpty() {
            return self.milesPerGallon() * self.getFuelRemaining()
        }
    }
    return vehicle
}

def makeCar() {
    var fuelRemaining := 20
    def car extends makeVehicle(car) {
        to milesPerGallon() {return 19}
        to getFuelRemaining() {return fuelRemaining}
    }
    return car
}

def makeJet() {
    var fuelRemaining := 2000
    def jet extends makeVehicle(jet) {
        to milesPerGallon() {return 2}
        to getFuelRemaining() {return fuelRemaining}
    }
    return jet
}

def car := makeCar()
println(`The car can go ${car.milesTillEmpty()} miles.`)
