oo::class create AbstractQueue {
    method enqueue item {
        error "not implemented"
    }
    method dequeue {} {
        error "not implemented"
    }
    self unexport create new
}
