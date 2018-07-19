package main

// Files required to build supporting package raster are found in:
// * Bitmap
// * Read a PPM file
// * Write a PPM file

import (
    "fmt"
    "os/exec"
    "raster"
)

func main() {
    // (A file with this name is output by the Go solution to the task
    // "Bitmap/PPM conversion through a pipe," but of course any handy
    // jpeg should work.)
    c := exec.Command("djpeg", "pipeout.jpg")
    pipe, err := c.StdoutPipe()
    if err != nil {
        fmt.Println(err)
        return
    }
    err = c.Start()
    if err != nil {
        fmt.Println(err)
        return
    }
    b, err := raster.ReadPpmFrom(pipe)
    if err != nil {
        fmt.Println(err)
        return
    }
    err = b.WritePpmFile("pipein.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
