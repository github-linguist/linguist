package require fileutil
proc walkin {path cmd} {
    set normalized [::fileutil::fullnormalize $path]
    set myname [lindex [info level 0] 0]
    set children [glob -nocomplain -directory $path -types hidden *]
    lappend children {*}[glob -nocomplain -directory $path *]
    foreach child $children[set children {}] {
        if {[file tail $child] in {. ..}} {
            continue
        }
        if {[file isdirectory $child]} {
            if {[file type $child] eq "link"} {
                set normalizedchild [fileutil::fullnormalize $child]
                if {[string first $normalized/ $normalizedchild] == 0} {
                    #symlink to a directory in $path.  Avoid cyclic traversal.
                    #Don't descend.
                } else {
                    $myname $child $cmd
                }
            }
        }
        {*}$cmd $child
    }
}

walkin /home/usr {apply {fname {
    set tail [file tail $fname]
    if {[string match *.mp3 $tail]} {
        puts $fname
    }
}}}
