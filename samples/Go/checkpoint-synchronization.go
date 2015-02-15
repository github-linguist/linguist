package main

import (
    "log"
    "math/rand"
    "os"
    "sync"
    "time"
)

const nMech = 5
const detailsPerMech = 4

var l = log.New(os.Stdout, "", 0)

func main() {
    assemble := make(chan int)
    var complete sync.WaitGroup

    go solicit(assemble, &complete, nMech*detailsPerMech)

    for i := 1; i <= nMech; i++ {
        complete.Add(detailsPerMech)
        for j := 0; j < detailsPerMech; j++ {
            assemble <- 0
        }
        // Go checkpoint feature
        complete.Wait()
        // checkpoint reached
        l.Println("mechanism", i, "completed")
    }
}

func solicit(a chan int, c *sync.WaitGroup, nDetails int) {
    rand.Seed(time.Now().UnixNano())
    var id int // worker id, for output
    for nDetails > 0 {
        // some random time to find a worker
        time.Sleep(time.Duration(5e8 + rand.Int63n(5e8)))
        id++
        // contract to assemble a certain number of details
        contract := rand.Intn(5) + 1
        if contract > nDetails {
            contract = nDetails
        }
        dword := "details"
        if contract == 1 {
            dword = "detail"
        }
        l.Println("worker", id, "contracted to assemble", contract, dword)
        go worker(a, c, contract, id)
        nDetails -= contract
    }
}

func worker(a chan int, c *sync.WaitGroup, contract, id int) {
    // some random time it takes for this worker to assemble a detail
    assemblyTime := time.Duration(5e8 + rand.Int63n(5e8))
    l.Println("worker", id, "enters shop")
    for i := 0; i < contract; i++ {
        <-a
        l.Println("worker", id, "assembling")
        time.Sleep(assemblyTime)
        l.Println("worker", id, "completed detail")
        c.Done()
    }
    l.Println("worker", id, "leaves shop")
}
