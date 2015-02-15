proc isEmptyDir {dir} {
    # Get list of _all_ files in directory
    set filenames [glob -nocomplain -tails -directory $dir * .*]
    # Check whether list is empty (after filtering specials)
    expr {![llength [lsearch -all -not -regexp $filenames {^\.\.?$}]]}
}
