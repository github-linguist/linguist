def asCompassPoint(angle) {
    def cardinalDirections = ["north", "east", "south", "west"]

    int index = Math.floor(angle / 11.25 + 0.5)
    int cardinalIndex = (index / 8)

    def c1 = cardinalDirections[cardinalIndex % 4]
    def c2 = cardinalDirections[(cardinalIndex + 1) % 4]
    def c3 = (cardinalIndex == 0 || cardinalIndex == 2) ? "$c1$c2" : "$c2$c1"

    def point = [
        "$c1", "$c1 by $c2", "$c1-$c3", "$c3 by $c1", "$c3", "$c3 by $c2", "$c2-$c3", "$c2 by $c1"
    ][index % 8]
    point.substring(0, 1).toUpperCase() + point.substring(1)
}
Number.metaClass.asCompassPoint =  { asCompassPoint(delegate) }

[0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75,
 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5,
 354.37, 354.38].eachWithIndex { angle, index ->
    println "${(index % 32) + 1}".padRight(3) + "${angle.asCompassPoint().padLeft(20)}    $angle\u00b0"
}
