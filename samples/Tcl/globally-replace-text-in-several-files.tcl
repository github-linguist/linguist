package require Tcl 8.5
package require fileutil

# Parameters to the replacement
set from "Goodbye London!"
set to "Hello New York!"
# Which files to replace
set fileList [list a.txt b.txt c.txt]

# Make a command fragment that performs the replacement on a supplied string
set replacementCmd [list string map [list $from $to]]
# Apply the replacement to the contents of each file
foreach filename $fileList {
    fileutil::updateInPlace $filename $replacementCmd
}
