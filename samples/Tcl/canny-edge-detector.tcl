package require crimp
package require crimp::pgm

proc readPGM {filename} {
    set f [open $filename rb]
    set data [read $f]
    close $f
    return [crimp read pgm $data]
}
proc writePGM {filename image} {
    crimp write 2file pgm-raw $filename $image
}

proc cannyFilterFile {{inputFile "lena.pgm"} {outputFile "lena_canny.pgm"}} {
    writePGM $outputFile [crimp filter canny sobel [readPGM $inputFile]]
}
cannyFilterFile {*}$argv
