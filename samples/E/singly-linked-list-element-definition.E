interface LinkedList guards LinkedListStamp {}
def empty implements LinkedListStamp {
    to null() { return true }
}
def makeLink(value :int, var next :LinkedList) {
    def link implements LinkedListStamp {
        to null() { return false }
        to value() { return value }
        to next() { return next }
        to setNext(new) { next := new }
    }
    return link
}
