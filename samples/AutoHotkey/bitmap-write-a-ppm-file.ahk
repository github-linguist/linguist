cyan := color(0,255,255) ; r,g,b
cyanppm := Bitmap(10, 10, cyan) ; width, height, background-color
Bitmap_write_ppm3(cyanppm, "cyan.ppm")
run, cyan.ppm
return

#include bitmap_storage.ahk  ; see basic bitmap storage task

Bitmap_write_ppm3(bitmap, filename)
{
file := FileOpen(filename, 0x11) ; utf-8, write
file.seek(0,0) ; overwrite BOM created with fileopen()
file.write("P3`n"  ;  `n = \n in ahk
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
