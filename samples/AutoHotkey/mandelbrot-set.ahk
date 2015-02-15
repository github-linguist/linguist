Max_Iteration := 256
Width := Height := 400

File := "MandelBrot." Width ".bmp"
Progress, b2 w400 fs9, Creating Colours ...
Gosub, CreateColours
Gosub, CreateBitmap
Progress, Off
Gui, -Caption
Gui, Margin, 0, 0
Gui, Add, Picture,, %File%
Gui, Show,, MandelBrot
Return

GuiClose:
GuiEscape:
ExitApp



;---------------------------------------------------------------------------
CreateBitmap: ; create and save a 32bit bitmap file
;---------------------------------------------------------------------------
    ; define header details
    HeaderBMP  := 14
    HeaderDIB  := 40
    DataOffset := HeaderBMP + HeaderDIB
    ImageSize  := Width * Height * 4 ; 32bit
    FileSize   := DataOffset + ImageSize
    Resolution := 3780 ; from mspaint

    ; create bitmap header
    VarSetCapacity(IMAGE, FileSize, 0)
    NumPut(Asc("B")   , IMAGE, 0x00, "Char")
    NumPut(Asc("M")   , IMAGE, 0x01, "Char")
    NumPut(FileSize   , IMAGE, 0x02, "UInt")
    NumPut(DataOffset , IMAGE, 0x0A, "UInt")
    NumPut(HeaderDIB  , IMAGE, 0x0E, "UInt")
    NumPut(Width      , IMAGE, 0x12, "UInt")
    NumPut(Height     , IMAGE, 0x16, "UInt")
    NumPut(1          , IMAGE, 0x1A, "Short") ; Planes
    NumPut(32         , IMAGE, 0x1C, "Short") ; Bits per Pixel
    NumPut(ImageSize  , IMAGE, 0x22, "UInt")
    NumPut(Resolution , IMAGE, 0x26, "UInt")
    NumPut(Resolution , IMAGE, 0x2A, "UInt")

    ; fill in Data
    Gosub, CreatePixels

    ; save Bitmap to file
    FileDelete, %File%
    Handle := DllCall("CreateFile", "Str", File, "UInt", 0x40000000
            , "UInt", 0, "UInt", 0, "UInt", 2, "UInt", 0, "UInt", 0)
    DllCall("WriteFile", "UInt", Handle, "UInt", &IMAGE, "UInt"
            , FileSize, "UInt *", Bytes, "UInt", 0)
    DllCall("CloseHandle", "UInt", Handle)

Return



;---------------------------------------------------------------------------
CreatePixels: ; create pixels for [-2 < x < 1] [-1.5 < y < 1.5]
;---------------------------------------------------------------------------
    Loop, % Height // 2 + 1 {
        yi := A_Index - 1
        y0 := -1.5 + yi / Height * 3 ; range -1.5 .. +1.5
        Progress, % 200*yi // Height, % "Current line: " 2*yi " / " Height
        Loop, %Width% {
            xi := A_Index - 1
            x0 := -2 + xi / Width * 3 ; range -2 .. +1
            Gosub, Mandelbrot
            p1 := DataOffset + 4 * (Width * yi + xi)
            NumPut(Colour, IMAGE, p1, "UInt")
            p2 := DataOffset + 4 * (Width * (Height-yi) + xi)
            NumPut(Colour, IMAGE, p2, "UInt")
        }
    }
Return



;---------------------------------------------------------------------------
Mandelbrot: ; calculate a colour for each pixel
;---------------------------------------------------------------------------
    x := y := Iteration := 0
    While, (x*x + y*y <= 4) And (Iteration < Max_Iteration) {
        xtemp := x*x - y*y + x0
        y := 2*x*y + y0
        x := xtemp
        Iteration++
    }
    Colour := Iteration = Max_Iteration ? 0 : Colour_%Iteration%

Return



;---------------------------------------------------------------------------
CreateColours: ; borrowed from PureBasic example
;---------------------------------------------------------------------------
    Loop, 64 {
        i4 := (i3 := (i2 := (i1 := A_Index - 1) + 64) + 64) + 64
        Colour_%i1% := RGB(4*i1 + 128, 4*i1, 0)
        Colour_%i2% := RGB(64, 255, 4*i1)
        Colour_%i3% := RGB(64, 255 - 4*i1, 255)
        Colour_%i4% := RGB(64, 0, 255 - 4*i1)
    }
Return



;---------------------------------------------------------------------------
RGB(r, g, b) { ; return 24bit color value
;---------------------------------------------------------------------------
    Return, (r&0xFF)<<16 | g<<8 | b
}
