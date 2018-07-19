def insertAfter(head :LinkedList ? (!head.null()),
                new  :LinkedList ? (new.next().null())) {
    new.setNext(head.next())
    head.setNext(new)
}

def a := makeLink(1, empty)
def b := makeLink(2, empty)
def c := makeLink(3, empty)

insertAfter(a, b)
insertAfter(a, c)

var x := a
while (!x.null()) {
    println(x.value())
    x := x.next()
}
