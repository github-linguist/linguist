<?php
function stripchars($s, $chars) {
    return str_replace(str_split($chars), "", $s);
}

echo stripchars("She was a soul stripper. She took my heart!", "aei"), "\n";
?>
