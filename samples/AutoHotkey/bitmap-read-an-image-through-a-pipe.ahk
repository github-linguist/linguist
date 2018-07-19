ppm := Run("cmd.exe /c convert lena50.jpg ppm:-")
                       ; pipe in from imagemagick
img := ppm_read("", ppm) ;
x := img[4,4] ; get pixel(4,4)
y := img[24,24] ; get pixel(24,24)
msgbox % x.rgb() " " y.rgb()
img.write("lena50copy.ppm")
return

ppm_read(filename, ppmo=0) ; only ppm6 files supported
{
if !ppmo  ; if image not already in memory, read from filename
  fileread, ppmo, % filename

  index := 1
  pos := 1

  loop, parse, ppmo, `n, `r
  {
    if (substr(A_LoopField, 1, 1) == "#")
      continue
loop,
{
 if !pos := regexmatch(ppmo, "\d+", pixel, pos)
break
    bitmap%A_Index% := pixel
    if (index == 4)
      Break
    pos := regexmatch(ppmo, "\s", x, pos)
    index ++
}
  }

  type := bitmap1
  width := bitmap2
  height := bitmap3
  maxcolor := bitmap4
  bitmap := Bitmap(width, height, color(0,0,0))
  index := 1
  i := 1
  j := 1
 bits := pos
loop % width * height
  {
      bitmap[i, j, "r"]  := numget(ppmo, 3 * A_Index + bits, "uchar")
      bitmap[i, j, "g"]  := numget(ppmo, 3 * A_Index + bits + 1, "uchar")
      bitmap[i, j, "b"]  := numget(ppmo, 3 * A_Index + bits + 2, "uchar")

      if (j == width)
{
	j := 1
	i += 1
}
      else
	j++
}
 return bitmap
  }
#include bitmap_storage.ahk ; from http://rosettacode.org/wiki/Basic_bitmap_storage/AutoHotkey
#include run.ahk ; http://www.autohotkey.com/forum/viewtopic.php?t=16823
