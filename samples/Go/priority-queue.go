package main

import (
    "fmt"
    "container/heap"
)

type Task struct {
    priority int
    name     string
}

type TaskPQ []Task

func (self TaskPQ) Len() int { return len(self) }
func (self TaskPQ) Less(i, j int) bool {
    return self[i].priority < self[j].priority
}
func (self TaskPQ) Swap(i, j int) { self[i], self[j] = self[j], self[i] }
func (self *TaskPQ) Push(x interface{}) { *self = append(*self, x.(Task)) }
func (self *TaskPQ) Pop() (popped interface{}) {
    popped = (*self)[len(*self)-1]
    *self = (*self)[:len(*self)-1]
    return
}

func main() {
    pq := &TaskPQ{{3, "Clear drains"},
        {4, "Feed cat"},
        {5, "Make tea"},
        {1, "Solve RC tasks"}}

    // heapify
    heap.Init(pq)

    // enqueue
    heap.Push(pq, Task{2, "Tax return"})

    for pq.Len() != 0 {
        // dequeue
        fmt.Println(heap.Pop(pq))
    }
}
