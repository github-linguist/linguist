v  The string to reverse. The row to copy to.
   |                      | The actual copying happens here.
   |                      | |    Increment column to write to.
   |                      | |    |  Store column #.
   v                      v v    v  v
> "reverse me" 3 10p >10g 4 p 10g1+ 10pv
               ^ ^   |:                <
First column --| |   @  ^
to write to.     |   ^  Get the address
   All calls to 10   |  to copy the next
 involve saving or   |  character to.
       reading the   End when stack is empty or
   column to write   explicit zero is reached.
               to.
