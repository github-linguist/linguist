<?php
$time = new DateTime('March 7 2009 7:30pm EST');
$time->modify('+12 hours');
echo $time->format('c');
?>
