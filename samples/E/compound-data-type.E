def makePoint(x, y) {
    def point {
        to getX() { return x }
        to getY() { return y }
    }
    return point
}
