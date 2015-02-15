package require Tcl 8.5

proc wrapParagraph {n text} {
    regsub -all {\s+} [string trim $text] " " text
    set RE "^(.{1,$n})(?:\\s+(.*))?$"
    for {set result ""} {[regexp $RE $text -> line text]} {} {
	append result $line "\n"
    }
    return [string trimright $result "\n"]
}

set txt \
"In olden times when wishing still helped one, there lived a king
whose daughters were all beautiful, but the youngest was so beautiful
that the sun itself, which has seen so much, was astonished whenever
it shone in her face.  Close by the king's castle lay a great dark
forest, and under an old lime-tree in the forest was a well, and when
the day was very warm, the king's child went out into the forest and
sat down by the side of the cool fountain, and when she was bored she
took a golden ball, and threw it up on high and caught it, and this
ball was her favorite plaything."

puts "[string repeat - 80]"
puts [wrapParagraph 80 $txt]
puts "[string repeat - 72]"
puts [wrapParagraph 72 $txt]
