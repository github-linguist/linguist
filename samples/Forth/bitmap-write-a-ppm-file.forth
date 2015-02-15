: write-ppm { bmp fid -- }
  s" P6"             fid write-line throw
  bmp bdim swap
  0 <# bl hold #s #> fid write-file throw
  0 <#         #s #> fid write-line throw
  s" 255"            fid write-line throw
  bmp bdata  bmp bdim * pixels
  bounds do
    i 3              fid write-file throw
  pixel +loop ;

s" red.ppm" w/o create-file throw
test over write-ppm
close-file throw
