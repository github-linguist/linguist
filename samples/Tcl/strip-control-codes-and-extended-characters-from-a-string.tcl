proc stripAsciiCC str {
    regsub -all {[\u0000-\u001f\u007f]+} $str ""
}
proc stripCC str {
    regsub -all {[^\u0020-\u007e]+} $str ""
}
