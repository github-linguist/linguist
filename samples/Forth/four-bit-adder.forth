: "NOT" invert 1 and ;
: "XOR" over over "NOT" and >r swap "NOT" and r> or ;
: halfadder over over and >r "XOR" r> ;
: fulladder halfadder >r swap halfadder r> or ;

: 4bitadder                            ( a3 a2 a1 a0 b3 b2 b1 b0 -- r3 r2 r1 r0 c)
  4 roll 0  fulladder swap >r >r
  3 roll r> fulladder swap >r >r
  2 roll r> fulladder swap >r fulladder r> r> r> 3 roll
;

: .add4 4bitadder 0 .r 4 0 do i 3 - abs roll 0 .r loop cr ;
