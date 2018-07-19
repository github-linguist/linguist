if { [file exists "input.txt"] } {
    puts "input.txt exists"
}

if { [file exists [file nativename "/input.txt"]] } {
    puts "/input.txt exists"
}

if { [file isdirectory "docs"] } {
    puts "docs exists and is a directory"
}

if { [file isdirectory [file nativename "/docs"]] } {
    puts "/docs exists and is a directory"
}
