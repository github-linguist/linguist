{...}     ;# group in one word, without substituting content; nests
"..."     ;# group in one word, with substituting content
[...]     ;# evaluate content as script, then substitute with its result; nests
$foo      ;# substitute with content of variable foo
$bar(foo) ;# substitute with content of element 'foo' of array 'bar'
\a        ;# audible alert (bell)
\b        ;# backspace
\f        ;# form feed
\n        ;# newline
\r        ;# carriage return
\t        ;# Tab
\v        ;# vertical tab
\\        ;# backslash
\ooo      ;# the Unicode with octal value 'ooo'
\xhh      ;# the character with hexadecimal value 'hh'
\uhhhh    ;# the Unicode with hexadecimal value 'hhhh'
#         ;# if first character of a word expected to be a command, begin comment
          ;# (extends till end of line)
{*}       ;# if first characters of a word, interpret as list of words to substitute,
          ;# not single word (introduced with Tcl 8.5)
