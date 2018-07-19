package queue

// int queue
// the zero object is a valid queue ready to be used.
// items are pushed at tail, popped at head.
// tail = -1 means queue is full
type Queue struct {
    b []string
    head, tail int
}

func (q *Queue) Push(x string) {
    switch {
    // buffer full. reallocate.
    case q.tail < 0:
        next := len(q.b)
        bigger := make([]string, 2*next)
        copy(bigger[copy(bigger, q.b[q.head:]):], q.b[:q.head])
        bigger[next] = x
        q.b, q.head, q.tail = bigger, 0, next+1
    // zero object. make initial allocation.
    case len(q.b) == 0:
        q.b, q.head, q.tail = make([]string, 4), 0 ,1
        q.b[0] = x
    // normal case
    default:
        q.b[q.tail] = x
        q.tail++
        if q.tail == len(q.b) {
            q.tail = 0
        }
        if q.tail == q.head {
            q.tail = -1
        }
    }
}

func (q *Queue) Pop() (string, bool) {
    if q.head == q.tail {
        return "", false
    }
    r := q.b[q.head]
    if q.tail == -1 {
        q.tail = q.head
    }
    q.head++
    if q.head == len(q.b) {
        q.head = 0
    }
    return r, true
}

func (q *Queue) Empty() bool {
    return q.head == q.tail
}
