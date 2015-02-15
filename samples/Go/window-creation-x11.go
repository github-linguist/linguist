package main

import (
    "code.google.com/p/x-go-binding/xgb"
    "fmt"
)

func main() {
    c, err := xgb.Dial("")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer c.Close()

    strBytes := []byte("Hello XGB!")
    rectangles := []xgb.Rectangle{{40, 40, 20, 20}}

    // get the first screen
    s := c.DefaultScreen()

    // root window
    win := s.Root

    // create black (foreground) graphic context
    fg := c.NewId()
    mask := uint32(xgb.GCForeground | xgb.GCGraphicsExposures)
    values := []uint32{s.BlackPixel, 0}
    c.CreateGC(fg, win, mask, values)

    // create white (background) graphic context
    bg := c.NewId()
    mask = uint32(xgb.GCBackground | xgb.GCGraphicsExposures)
    values[0] = s.WhitePixel // (values[1] still 0)
    c.CreateGC(bg, win, mask, values)

    //  create the window
    win = c.NewId()
    mask = xgb.CWBackPixel | xgb.CWEventMask
    // values[0] still s.WhitePixel
    values[1] = xgb.EventMaskExposure | xgb.EventMaskKeyPress
    c.CreateWindow(0, win, s.Root, 0, 0, 150, 150, 10,
        xgb.WindowClassInputOutput, s.RootVisual, mask, values)
    c.MapWindow(win)

    for {
        event, err := c.WaitForEvent()
        if err != nil {
            fmt.Println(err)
            return
        }
        switch event.(type) {
        case xgb.ExposeEvent:
            c.PolyRectangle(win, fg, rectangles)
            c.ImageText8(win, bg, 20, 20, strBytes)
        case xgb.KeyPressEvent:
            return
        }
    }
}
