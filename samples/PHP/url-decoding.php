<?php
$encoded = "http%3A%2F%2Ffoo%20bar%2F";
$unencoded = rawurldecode($encoded);
echo "The unencoded string is $unencoded !\n";
?>
