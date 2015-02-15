<?php
function mycmp($s1, $s2)
{
    if ($d = strlen($s2) - strlen($s1))
        return $d;
    return strcasecmp($s1, $s2);
}

$strings = array("Here", "are", "some", "sample", "strings", "to", "be", "sorted");
usort($strings, "mycmp");
?>
