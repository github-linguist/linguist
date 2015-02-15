def insert(anchor, new):
    new.next = anchor.next
    new.prev = anchor
    anchor.next.prev = new
    anchor.next = new
