USE: io.directories.search

"." t [
    dup ".factor" tail? [ print ] [ drop ] if
] each-file
