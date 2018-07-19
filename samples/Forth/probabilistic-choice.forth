include random.fs

\ common factors of desired probabilities (1/5 .. 1/11)
2 2 * 2 * 3 * 3 * 5 * 7 * 11 * constant denom   \ 27720

\ represent each probability as the numerator with 27720 as the denominator
: ,numerators ( max min -- )
  do denom i / , loop ;

\  final item is 27720 - sum(probs)
: ,remainder ( denom addr len -- )
  cells bounds do  i @ -  1 cells +loop , ;

create probs 12 5 ,numerators  denom probs 7 ,remainder
create bins 8 cells allot

: choose ( -- 0..7 )
  denom random
  8 0 do
    probs i cells + @ -
    dup 0< if drop i unloop exit then
  loop
  abort" can't get here" ;

: trials ( n -- )
  0 do  1  bins choose cells +  +!  loop ;

: str-table
  create ( c-str ... n -- ) 0 do , loop
  does> ( n -- str len ) swap cells + @ count ;

here ," heth"   here ," zayin" here ," waw"  here ," he"
here ," daleth" here ," gimel" here ," beth" here ," aleph"
8 str-table names

: .header
  cr ." Name" #tab emit ." Prob" #tab emit ." Actual" #tab emit ." Error" ;
: .result ( n -- )
  cr dup names type #tab emit
  dup cells probs + @ s>f denom s>f f/ fdup f. #tab emit
  dup cells bins  + @ s>f 1e6       f/ fdup f. #tab emit
  f- fabs fs. ;

: .results   .header 8 0 do i .result loop ;
