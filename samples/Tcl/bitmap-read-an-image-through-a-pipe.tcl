package require Tk

proc magickalReadImage {bufferImage fileName} {
    set f [open |[list convert [file normalize $fileName] ppm:-] "rb"]
    try {
        $bufferImage put [read $f] -format ppm
    } finally {
        close $f
    }
}
