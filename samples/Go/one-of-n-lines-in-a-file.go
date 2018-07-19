package main

import (
    "bufio"
    "fmt"
    "io"
    "math/rand"
    "time"
)

// choseLineRandomly implements the method described in the task.
// input is a an io.Reader, which could be an os.File, for example.
// Or, to implement a simulation, it could be anything else that implements
// io.Reader.  The method as described suggests saving and returning
// lines, but the rest of the task requires line numbers.  This function
// thus returns both.
func choseLineRandomly(r io.Reader) (s string, ln int, err error) {
    br := bufio.NewReader(r)
    s, err = br.ReadString('\n')
    if err != nil {
        return
    }
    ln = 1
    lnLast := 1.
    var sLast string
    for {
        // note bufio.ReadString used here.  This effectively defines a
        // line of the file as zero or more bytes followed by a newline.
        sLast, err = br.ReadString('\n')
        if err == io.EOF {
            return s, ln, nil // normal return
        }
        if err != nil {
            break
        }
        lnLast++
        if rand.Float64() < 1/lnLast {
            s = sLast
            ln = int(lnLast)
        }
    }
    return // error return
}

// oneOfN function required for task item 1.  Specified to take a number
// n, the number of lines in a file, but the method (above) specified to
// to be used does not need n, but rather the file itself.  This function
// thus takes both, ignoring n and passing the file to choseLineRandomly.
func oneOfN(n int, file io.Reader) int {
    _, ln, err := choseLineRandomly(file)
    if err != nil {
        panic(err)
    }
    return ln
}

// simulated file reader for task item 2
type simReader int

func (r *simReader) Read(b []byte) (int, error) {
    if *r <= 0 {
        return 0, io.EOF
    }
    b[0] = '\n'
    *r--
    return 1, nil
}

func main() {
    // task item 2 simulation consists of accumulating frequency statistic
    // on 1,000,000 calls of oneOfN on simulated file.
    n := 10
    freq := make([]int, n)
    rand.Seed(time.Now().UnixNano())
    for times := 0; times < 1e6; times++ {
        sr := simReader(n)
        freq[oneOfN(n, &sr)-1]++
    }

    // task item 3.  show frequencies.
    fmt.Println(freq)
}
