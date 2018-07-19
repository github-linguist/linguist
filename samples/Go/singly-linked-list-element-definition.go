type Ele struct {
    Data interface{}
    Next *Ele
}

func (e *Ele) Append(data interface{}) *Ele {
    if e.Next == nil {
        e.Next = &Ele{data, nil}
    } else {
        tmp := &Ele{data, e.Next}
        e.Next = tmp
    }
    return e.Next
}

func (e *Ele) String() string {
    return fmt.Sprintf("Ele: %v", e.Data)
}
