def makeElement(var value, var next, var prev) {
    def element {
        to setValue(v) { value := v }
        to getValue() { return value }

        to setNext(n) { next := n }
        to getNext() { return next }

        to setPrev(p) { prev := p }
        to getPrev() { return prev }
    }

    return element
}
