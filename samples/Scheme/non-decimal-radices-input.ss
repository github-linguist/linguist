> (string->number "abcf123" 16) ; hex
180154659
> (string->number "123459" 10) ; decimal, the "10" is optional
123459
> (string->number "7651" 8) ; octal
4009
> (string->number "101011001" 2) ; binary
345
