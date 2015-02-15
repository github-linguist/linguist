  0 enum baker                         \ enumeration of all tenants
    enum cooper
    enum fletcher
    enum miller
constant smith

create names                           \ names of all the tenants
  ," Baker"
  ," Cooper"
  ," Fletcher"
  ," Miller"
  ," Smith"                            \ get name, type it
does> swap cells + @c count type ."  lives in " ;

        5 constant #floor              \ number of floors
#floor 1- constant top                 \ top floor
        0 constant bottom              \ we're counting the floors from 0

: num@ c@ [char] 0 - ;                 ( a -- n)
: floor chars over + num@ ;            ( a n1 -- a n2)
                                       \ is it a valid permutation?
: perm?                                ( n -- a f)
  #floor base ! 0 swap s>d <# #floor 0 ?do # loop #>
  over >r bounds do 1 i num@ lshift + loop
  31 = r> swap decimal                 \ create binary mask and check
;
                                       \ test a solution
: solution?                            ( a -- a f)
  baker floor top <>                   \ baker on top floor?
  if cooper floor bottom <>            \ cooper on the bottom floor?
     if fletcher floor dup bottom <> swap top <> and
        if cooper floor swap miller floor rot >
           if smith floor swap fletcher floor rot - abs 1 <>
              if cooper floor swap fletcher floor rot - abs 1 <>
                 if true exit then     \ we found a solution!
              then
           then
        then
     then
  then false                           \ nice try, no cigar..
;
                                       ( a --)
: .solution #floor 0 do i names i chars over + c@ 1+ emit cr loop drop ;
                                       \ main routine
: dinesman                             ( --)
  2932 194 do
    i perm? if solution? if .solution leave else drop then else drop then
  loop
;                                      \ show the solution

dinesman
