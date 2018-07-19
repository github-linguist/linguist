<?php
/* Assignment of scalar variables */
$a = 1;
$b =& $a; // $b and $a are now linked together
$b = 2; //both $b and $a now equal 2
$c = $b;
$c = 7; //$c is not a reference; no change to $a or $b
unset($a); //won't unset $b, just $a.

/* Passing by Reference in and out of functions */
function &pass_out() {
    global $filestr;  //$exactly equivalent to: $filestr =& $_GLOBALS['filestr'];

    $filestr = get_file_contents("./bigfile.txt");
    return $_GLOBALS['filestr'];
}
function pass_in(&$in_filestr) {
    echo "File Content Length: ". strlen($in_filestr);

    /* Changing $in_filestr also changes the global $filestr and $tmp */
    $in_filestr .= "EDIT";
    echo "File Content Length is now longer: ". strlen($in_filestr);
}

$tmp = &pass_out(); // now $tmp and the global variable $filestr are linked
pass_in($tmp); // changes $tmp and prints the length

?>
