<?php

if (version_compare(PHP_VERSION, '5.3.0', '<' ))
{
	echo("You are using PHP Version " . PHP_VERSION . ". Please upgrade to Version 5.3.0\n");
	exit();
}
$bloop = -3;
if (isset($bloop) && function_exists('abs'))
{
	echo(abs($bloop));
}
echo(count($GLOBALS) . " variables in global scope.\n");
echo(array_sum($GLOBALS) . " is the total of variables in global scope.\n");

?>
