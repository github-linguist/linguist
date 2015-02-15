#! /usr/bin/env factor
USING: kernel calendar calendar.format io io.encodings.utf8 io.files
sequences command-line namespaces ;

command-line get [
    "notes.txt" utf8 file-contents print
] [
    " " join "\t" prepend
    "notes.txt" utf8 [
        now timestamp>ymdhms print
        print flush
    ] with-file-appender
] if-empty
