'c';                      # character
'hello';                  # these two strings are the same
"hello";
'Hi $name. How are you?'; # result: "Hi $name. How are you?"
"Hi $name. How are you?"; # result: "Hi Bob. How are you?"
'\n';                     # 2-character string with a backslash and "n"
"\n";                     # newline character
`ls`;                     # runs a command in the shell and returns the output as a string
q/hello/;                 # same as 'hello', but allows custom delimiters, eg: q(hi) and q!hi!
qq/hello/;                # same as "hello", but allows custom delimiters, eg: qq{$hi} and qq#hi#
qw/one two three/;        # same as ('one', 'two', 'three'); constructs a list of the words
qx/ls/;                   # quoted execution, same as `ls`
qr/regex/;                # creates a regular expression
<<END;                    # Here-Document
Hi, whatever goes here gets put into the string,
including newlines and $variables,
until the label we put above
END
<<'END';                  # Here-Document like single-quoted
Same as above, but no interpolation of $variables.
END
