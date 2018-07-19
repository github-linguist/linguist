Node := Object clone do(
    next := nil
    obj := nil
)

Stack := Object clone do(
    node := nil

    pop := method(
        obj := node obj
        node = node next
        obj
    )

    push := method(obj,
        nn := Node clone
        nn obj = obj
        nn next = self node
        self node = nn
    )
)
