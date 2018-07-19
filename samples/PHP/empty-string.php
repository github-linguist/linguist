<?php

$str = ''; // assign an empty string to a variable

// check that a string is empty
if (empty($str)) { ... }

// check that a string is not empty
if (! empty($str)) { ... }

// we could also use the following
if ($str == '') { ... }
if ($str != '') { ... }

if (strlen($str) == 0) { ... }
if (strlen($str) != 0) { ... }
