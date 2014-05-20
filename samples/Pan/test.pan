object template pantest;

# Very simple pan test file
"/long/decimal" = 123;
"/long/octal" = 0755;
"/long/hexadecimal" = 0xFF;

"/double/simple" = 0.01;
"/double/pi" = 3.14159;
"/double/exponent" = 1e-8;
"/double/scientific" = 1.3E10;

"/string/single" = 'Faster, but escapes like \t, \n and \x3d don''t work, but '' should work.';
"/string/double" = "Slower, but escapes like \t, \n and \x3d do work";

variable TEST = 2;

"/x2" = to_string(TEST);
"/x2" ?= 'Default value';

"/x3" = 1 + 2 + value("/long/decimal");

"/x4" = undef;

"/x5" = null;

variable e ?= error("Test error message");

# include gmond config for services-monitoring
include { 'site/ganglia/gmond/services-monitoring' };

"/software/packages"=pkg_repl("httpd","2.2.3-43.sl5.3",PKG_ARCH_DEFAULT);
"/software/packages"=pkg_repl("php");

# Example function
function show_things_view_for_stuff = {
    thing = ARGV[0];
    foreach( i; mything; STUFF ) {
        if ( thing == mything ) {
            return( true );
        } else {
            return SELF;
        };
    };
    false;
};

variable HERE = <<EOF;
; This example demonstrates an in-line heredoc style config file
[main]
awesome = true
EOF

variable small = false;#This should be highlighted normally again.
