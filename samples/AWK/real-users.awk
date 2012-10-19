BEGIN {
    FS = ":"
}
$3 > 999 && $7 !~ /\/bin\/(sh|false)/ {
    print $1
}
