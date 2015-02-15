<?php
$filename = 'input.txt';

$mtime = filemtime($filename); // seconds since the epoch

touch($filename,
      time(), // set mtime to current time
      fileatime($filename)); // keep atime unchanged
?>
