include lib/interprt.4th
include lib/istype.4th
include lib/argopen.4th

\  ---------------------
\  Variables
\  ---------------------

81 string sudokugrid
9 array sudoku_row
9 array sudoku_col
9 array sudoku_box

\  -------------
\  4tH interface
\  -------------

: >grid                                ( n2 a1 n1 -- n3)
  rot dup >r 9 chars * sudokugrid + dup >r swap
  0 do                                 ( a1 a2)
    over i chars + c@ dup is-digit     ( a1 a2 c f)
    if [char] 0 - over c! char+ else drop then
  loop                                 ( a1 a2)
  nip r> - 9 /  r> +                   ( n3)
;

0
s" 090004007" >grid
s" 000007900" >grid
s" 800000000" >grid
s" 405800000" >grid
s" 300000002" >grid
s" 000009706" >grid
s" 000000004" >grid
s" 003500000" >grid
s" 200600080" >grid
drop

\  ---------------------
\  Logic
\  ---------------------
\  Basically :
\     Grid is parsed. All numbers are put into sets, which are
\     implemented as bitmaps (sudoku_row, sudoku_col, sudoku_box)
\     which represent sets of numbers in each row, column, box.
\     only one specific instance of a number can exist in a
\     particular set.

\     SOLVER is recursively called
\     SOLVER looks for the next best guess using FINDNEXTSPACE
\     tries this trail down... if fails, backtracks... and tries
\     again.


\ Grid Related

: xy 9 * + ;                           \  x y -- offset ;
: getrow 9 / ;
: getcol 9 mod ;
: getbox dup getrow 3 / 3 * swap getcol 3 / + ;

\ Puts and gets numbers from/to grid only
: setnumber sudokugrid + c! ;          \ n position --
: getnumber sudokugrid + c@ ;

: cleargrid sudokugrid 81 bounds do 0 i c! loop ;

\ --------------
\ Set related: sets are sudoku_row, sudoku_col, sudoku_box

\ ie x y --   ;  adds x into bitmap y
: addbits_row cells sudoku_row + dup @ rot 1 swap lshift or swap ! ;
: addbits_col cells sudoku_col + dup @ rot 1 swap lshift or swap ! ;
: addbits_box cells sudoku_box + dup @ rot 1 swap lshift or swap ! ;

\ ie x y --  ; remove number x from bitmap y
: removebits_row cells sudoku_row + dup @ rot 1 swap lshift invert and swap ! ;
: removebits_col cells sudoku_col + dup @ rot 1 swap lshift invert and swap ! ;
: removebits_box cells sudoku_box + dup @ rot 1 swap lshift invert and swap ! ;

\ clears all bitsmaps to 0
: clearbitmaps 9 0 do i cells
                     0 over sudoku_row + !
                     0 over sudoku_col + !
                     0 swap sudoku_box + !
           loop ;

\ Adds number to grid and sets
: addnumber                            \ number position --
    2dup setnumber
    2dup getrow addbits_row
    2dup getcol addbits_col
         getbox addbits_box
;

\ Remove number from grid, and sets
: removenumber                         \ position --
    dup getnumber swap
    2dup getrow removebits_row
    2dup getcol removebits_col
    2dup getbox removebits_box
    nip 0 swap setnumber
;

\ gets bitmap at position, ie
\ position -- bitmap

: getrow_bits getrow cells sudoku_row + @ ;
: getcol_bits getcol cells sudoku_col + @ ;
: getbox_bits getbox cells sudoku_box + @ ;

\ position -- composite bitmap  (or'ed)
: getbits
    dup getrow_bits
    over getcol_bits
    rot getbox_bits or or
;

\ algorithm from c.l.f circa 1995 ? Will Baden
: countbits    ( number -- bits )
        [HEX] DUP  55555555 AND  SWAP  1 RSHIFT  55555555 AND  +
              DUP  33333333 AND  SWAP  2 RSHIFT  33333333 AND  +
              DUP  0F0F0F0F AND  SWAP  4 RSHIFT  0F0F0F0F AND  +
        [DECIMAL] 255 MOD
;

\ Try tests a number in a said position of grid
\ Returns true if it's possible, else false.
: try                                  \ number position -- true/false
      getbits 1 rot lshift and 0=
;

\ --------------
: parsegrid                            \ Parses Grid to fill sets.. Run before solver.
   sudokugrid                          \ to ensure all numbers are parsed into sets/bitmaps
   81 0 do
     dup i + c@
       dup if
         dup i try if
           i addnumber
         else
           unloop drop drop FALSE exit
         then
       else
         drop
       then
   loop
   drop
   TRUE
;

\ Morespaces? manually checks for spaces ...
\ Obviously this can be optimised to a count var, done initially
\ Any additions/subtractions made to the grid could decrement
\ a 'spaces' variable.

: morespaces?
     0  sudokugrid 81 bounds do i c@  0= if 1+ then loop ;

: findnextmove                         \  -- n ; n = index next item, if -1 finished.

   -1  10                              \  index  prev_possibilities  --
                                       \  err... yeah... local variables, kind of...

   81 0 do
      i sudokugrid + c@ 0= IF
             i getbits countbits 9 swap -

             \ get bitmap and see how many possibilities
             \ stack diagram:
             \ index prev_possibilities  new_possiblities --

             2dup > if
                                       \ if new_possibilities < prev_possibilities...
                 nip nip i swap
                                       \ new_index new_possibilies --

             else                      \ else prev_possibilities < new possibilities, so:

                 drop                  \ new_index new_possibilies --

             then
      THEN
   loop
   drop
;

\ findnextmove returns index of best next guess OR returns -1
\ if no more guesses. You then have to check to see if there are
\ spaces left on the board unoccupied. If this is the case, you
\ need to back up the recursion and try again.

: solver
     findnextmove
         dup 0< if
             morespaces? if
                drop false exit
             else
                drop true exit
             then
         then

     10 1 do
        i over try if
           i over addnumber
           recurse  if
                drop unloop TRUE EXIT
           else
                dup removenumber
           then
        then
     loop

     drop FALSE
;

\ SOLVER

: startsolving
   clearbitmaps                        \ reparse bitmaps and reparse grid
   parsegrid                           \ just in case..
   solver
   AND
;

\  ---------------------
\  Display Grid
\  ---------------------

\ Prints grid nicely

: .sudokugrid
  CR CR
  sudokugrid
  81 0 do
    dup i + c@ .
    i 1+
      dup 3 mod 0= if
         dup 9 mod 0= if
            CR
            dup 27 mod 0= if
              dup 81 < if ." ------+-------+------" CR then
            then
         else
           ." | "
         then
      then
    drop
  loop
  drop
  CR
;

\  ---------------------
\  Higher Level Words
\  ---------------------

: checkifoccupied                      ( offset -- t/f)
    sudokugrid + c@
;

: add                                  ( n x y --)
    xy 2dup
      dup checkifoccupied if
        dup removenumber
      then
    try if
      addnumber
      .sudokugrid
    else
      CR ." Not a valid move. " CR
      2drop
    then
;

: rm
    xy removenumber
    .sudokugrid
;

: clearit
    cleargrid
    clearbitmaps
    .sudokugrid
;

: solveit
  CR
  startsolving
  if
    ." Solution found!" CR .sudokugrid
  else
    ." No solution found!" CR CR
  then
;

: showit .sudokugrid ;

\ Print help menu
: help
  CR
  ." Type clearit     ; to clear grid " CR
  ."      1-9 x y add ; to add 1-9 to grid at x y (0 based) " CR
  ."      x y rm      ; to remove number at x y " CR
  ."      showit      ; redisplay grid " CR
  ."      solveit     ; to solve " CR
  ."      help        ; for help " CR
  CR
;

\  ---------------------
\  Execution starts here
\  ---------------------

: godoit
    clearbitmaps
    parsegrid if
      CR ." Grid valid!"
    else
      CR ." Warning: grid invalid!"
    then
    .sudokugrid
    help
;

\  -------------
\  4tH interface
\  -------------

: read-sudoku
  input 1 arg-open 0
  begin dup 9 < while refill while 0 parse >grid repeat
  drop close
;

: bye quit ;

create wordlist                        \ dictionary
  ," clearit" ' clearit ,
  ," add"     ' add ,
  ," rm"      ' rm ,
  ," showit"  ' showit ,
  ," solveit" ' solveit ,
  ," quit"    ' bye ,
  ," exit"    ' bye ,
  ," bye"     ' bye ,
  ," q"       ' bye ,
  ," help"    ' help ,
  NULL ,

wordlist to dictionary
:noname ." Unknown command '" type ." '" cr ; is NotFound
                                       \ sudoku interpreter
: sudoku
  argn 1 > if read-sudoku then
  godoit
  begin
    ." OK" cr
    refill drop ['] interpret
    catch if ." Error" cr then
  again
;

sudoku
