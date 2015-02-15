<?php

/**
 * @author Elad Yosifon
 */

$string = '      this is a string     ';
echo '^'.trim($string) .'$'.PHP_EOL;
echo '^'.ltrim($string).'$'.PHP_EOL;
echo '^'.rtrim($string).'$'.PHP_EOL;
