+[            Start with n=1 to kick off the loop
[>>++<<       Set up {n 0 2} for divmod magic
[->+>-        Then
[>+>>]>       do
[+[-<+>]>+>>] the
<<<<<<]       magic
>>>+          Increment n % 2 so that 0s don't break things
>]            Move into n / 2 and divmod that unless it's 0
-<            Set up sentinel ‑1 then move into the first binary digit
[++++++++ ++++++++ ++++++++ Add 47 to get it to ASCII
 ++++++++ ++++++++ +++++++. and print it
[<]<]         Get to a 0; the cell to the left is the next binary digit
>>[<+>-]      Tape is {0 n}; make it {n 0}
>[>+]         Get to the ‑1
<[[-]<]       Zero the tape for the next iteration
++++++++++.   Print a newline
[-]<+]        Zero it then increment n and go again
