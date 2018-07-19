package require Tk

proc output_ppm {image filename} {
    $image write $filename -format ppm
}

set img [newImage 150 150]
fill $img red
setPixel $img green 40 40
output_ppm $img filename.ppm

# check the file format:
set fh [open filename.ppm]
puts [gets $fh] ;# ==> P6
puts [gets $fh] ;# ==> 150 150
puts [gets $fh] ;# ==> 255
binary scan [read $fh 3] c3 pixel
foreach colour $pixel {puts [expr {$colour & 0xff}]} ;# ==> 255 \n 0 \n 0 \n
close $fh
