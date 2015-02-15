'c';                      # character
'hello';                  # these two strings are the same
"hello";
'Hi $name. How are you?'; # result: "Hi $name. How are you?"
"Hi $name. How are you?"; # result: "Hi Bob. How are you?"
'\n';                     # 2-character string with a backslash and "n"
"\n";                     # newline character
`ls`;                     # runs a command in the shell and returns the output as a string
<<END                     # Here-Document
Hi, whatever goes here gets put into the string,
including newlines and $variables,
until the label we put above
END;
<<'END'                   # Here-Document like single-quoted
Same as above, but no interpolation of $variables.
END;
