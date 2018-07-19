package main

import (
    "errors"
    "fmt"
    "strings"
    "sync"
)

var hdText = `Humpty Dumpty sat on a wall.
Humpty Dumpty had a great fall.
All the king's horses and all the king's men,
Couldn't put Humpty together again.`

var mgText = `Old Mother Goose,
When she wanted to wander,
Would ride through the air,
On a very fine gander.
Jack's mother came in,
And caught the goose soon,
And mounting its back,
Flew up to the moon.`

func main() {
    reservePrinter := startMonitor(newPrinter(5), nil)
    mainPrinter := startMonitor(newPrinter(5), reservePrinter)
    var busy sync.WaitGroup
    busy.Add(2)
    go writer(mainPrinter, "hd", hdText, &busy)
    go writer(mainPrinter, "mg", mgText, &busy)
    busy.Wait()
}

// printer is a type representing an abstraction of a physical printer.
// It is a type defintion for a function that takes a string to print
// and returns an error value, (hopefully usually nil, meaning no error.)
type printer func(string) error

// newPrinter is a constructor.  The parameter is a quantity of ink.  It
// returns a printer object encapsulating the ink quantity.
// Note that this is not creating the monitor, only the object serving as
// a physical printer by writing to standard output.
func newPrinter(ink int) printer {
    return func(line string) error {
        if ink == 0 {
            return eOutOfInk
        }
        for _, c := range line {
            fmt.Printf("%c", c)
        }
        fmt.Println()
        ink--
        return nil
    }
}

var eOutOfInk = errors.New("out of ink")

// For the language task, rSync is a type used to approximate the Ada
// rendezvous mechanism that includes the caller waiting for completion
// of the callee.  For this use case, we signal completion with an error
// value as a response.  Exceptions are not idiomatic in Go and there is
// no attempt here to model the Ada exception mechanism.  Instead, it is
// idomatic in Go to return error values.  Sending an error value on a
// channel works well here to signal completion.  Go unbuffered channels
// provide synchronous rendezvous, but call and response takes two channels,
// which are bundled together here in a struct.  The channel types are chosen
// to mirror the parameter and return types of "type printer" defined above.
// The channel types here, string and error are both "reference types"
// in Go terminology.  That is, they are small things containing pointers
// to the actual data.  Sending one on a channel does not involve copying,
// or much less marshalling string data.
type rSync struct {
    call     chan string
    response chan error
}

// "rendezvous Print" requested by use case task.
// For the language task though, it is implemented here as a method on
// rSync that sends its argument on rSync.call and returns the result
// received from rSync.response.  Each channel operation is synchronous.
// The two operations back to back approximate the Ada rendezvous.
func (r *rSync) print(data string) error {
    r.call <- data      // blocks until data is accepted on channel
    return <-r.response // blocks until response is received
}

// monitor is run as a goroutine.  It encapsulates the printer passed to it.
// Print requests are received through the rSync object "entry," named entry
// here to correspond to the Ada concept of an entry point.
func monitor(hardPrint printer, entry, reserve *rSync) {
    for {
        // The monitor goroutine will block here waiting for a "call"
        // to its "entry point."
        data := <-entry.call
        // Assuming the call came from a goroutine calling rSync.print,
        // that goroutine is now blocked, waiting for this one to send
        // a response.

        // attempt output
        switch err := hardPrint(data); {

        // consider return value from attempt
        case err == nil:
            entry.response <- nil // no problems

        case err == eOutOfInk && reserve != nil:
            // Requeue to "entry point" of reserve printer monitor.
            // Caller stays blocked, and now this goroutine blocks until
            // it gets a response from the reserve printer monitor.
            // It then transparently relays the response to the caller.
            entry.response <- reserve.print(data)

        default:
            entry.response <- err // return failure
        }
        // The response is away.  Loop, and so immediately block again.
    }
}

// startMonitor can be seen as an rSync constructor.  It also
// of course, starts the monitor for which the rSync serves as entry point.
// Further to the langauge task, note that the channels created here are
// unbuffered.  There is no buffer or message box to hold channel data.
// A sender will block waiting for a receiver to accept data synchronously.
func startMonitor(p printer, reservePrinter *rSync) *rSync {
    entry := &rSync{make(chan string), make(chan error)}
    go monitor(p, entry, reservePrinter)
    return entry
}

// Two writer tasks are started as goroutines by main.  They run concurrently
// and compete for printers as resources.  Note the call to "rendezvous Print"
// as requested in the use case task and compare the syntax,
//    Here:           printMonitor.print(line);
//    Ada solution:   Main.Print ("string literal");
func writer(printMonitor *rSync, id, text string, busy *sync.WaitGroup) {
    for _, line := range strings.Split(text, "\n") {
        if err := printMonitor.print(line); err != nil {
            fmt.Printf("**** writer task %q terminated: %v ****\n", id, err)
            break
        }
    }
    busy.Done()
}
