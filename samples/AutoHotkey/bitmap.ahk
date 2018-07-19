test:
blue := color(0,0,255)  ; rgb
cyan := color(0,255,255)
blue_square := Bitmap(10, 10, blue)
cyanppm := Bitmap(10, 10, cyan)
x := blue_square[4,4] ; get pixel(4,4)
msgbox % "blue: 4,4,R,G,B, RGB: " x.R ", " x.G ", " x.B ", " x.rgb()
blue_square[4,4] := cyan ; set pixel(4,4)
x := blue_square[4,4] ; get pixel(4,4)
blue_square.write("blue.ppm")
return

Bitmap(width = 1, height = 1, background = 0)
{
global black
black := color(0,0,0)
if !background
background := black

    static BitmapType
    if !BitmapType
        BitmapType
        := Object("fill", "Bitmap_Fill"
	         ,"write", "Bitmap_write_ppm3")

	img := Object("width", width
                ,"height", height
                , "base"    , BitmapType)

	img._SetCapacity(height) ; an array of rows
  img.fill(background)
Return img
}


Bitmap_Fill(bitmap, color)
{
r := color.r
g := color.g
b := color.b
  loop % bitmap.height
  {
    height := A_Index
    loop % bitmap.width
    {
      width := A_Index
      bitmap[height, width] := color(r, g, b)
    }
  }
 return bitmap
}

Bitmap_write_ppm3(bitmap, filename)
{
file := FileOpen(filename, 0x11) ; utf-8, write
file.seek(0,0)
file.write("P3`n"
. bitmap.width . " " . bitmap.height . "`n"
. "255`n")
  loop % bitmap.height
  {
    height := A_Index
    loop % bitmap.width
    {
      width := A_Index
      color := bitmap[height, width]
      file.Write(color.R . " ")
      file.Write(color.G . " ")
      file.Write(color.B . " ")
    }
    file.write("`n")
  }
  file.close()
 return 0
}

Color(r, g, b)
{
    static ColorType
    if !ColorType
        ColorType
        := Object("rgb"   , "Color_rgb")

    return Object("r" , r, "g", g, "b", b
                , "base"    , ColorType)

 ; return Object("r" , r, "g", g, "b", b, "rgb", "Color_rgb")
}

Color_rgb(clr)
{
return clr.R << 16 | clr.G << 8 | clr.B
}
