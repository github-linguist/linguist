<?php
$varname = rtrim(fgets(STDIN)); # type in "foo" on standard input
$$varname = 42;
echo "$foo\n"; # prints "42"
?>
