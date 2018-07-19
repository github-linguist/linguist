<?php
// Check to see whether it is defined
if (!isset($var))
    echo "var is undefined at first check\n";

// Give it a value
$var = "Chocolate";

// Check to see whether it is defined after we gave it the
// value "Chocolate"
if (!isset($var))
    echo "var is undefined at second check\n";

// Give the variable an undefined value.
unset($var);

// Check to see whether it is defined after we've explicitly
// given it an undefined value.
if (!isset($var))
    echo "var is undefined at third check\n";

// Give the variable a value of 42
$var = 42;

// Check to see whether the it is defined after we've given it
// the value 42.
if (!isset($var))
    echo "var is undefined at fourth check\n";

// Because most of the output is conditional, this serves as
// a clear indicator that the program has run to completion.
echo "Done\n";
?>
