#!/usr/bin/perl -w
use strict;

# Declare the variable. It is initialized to the value "undef"
our $var;

# Check to see whether it is defined
print "var contains an undefined value at first check\n" unless defined $var;

# Give it a value
$var = "Chocolate";

# Check to see whether it is defined after we gave it the
# value "Chocolate"
print "var contains an undefined value at second check\n" unless defined $var;

# Give the variable the value "undef".
$var = undef;
# or, equivalently:
undef($var);

# Check to see whether it is defined after we've explicitly
# given it an undefined value.
print "var contains an undefined value at third check\n" unless defined $var;

# Give the variable a value of 42
$var = 42;

# Check to see whether the it is defined after we've given it
# the value 42.
print "var contains an undefined value at fourth check\n" unless defined $var;

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
print "Done\n";
