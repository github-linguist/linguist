include lib/choose.4th
                                       \ target string
s" METHINKS IT IS LIKE A WEASEL" sconstant target

27 constant /charset                   \ size of characterset
29 constant /target                    \ size of target string
32 constant #copies                    \ number of offspring

/target string charset                 \ characterset
/target string this-generation         \ current generation and offspring
/target #copies [*] string new-generation

:this new-generation does> swap /target chars * + ;
                                       \ generate a mutation
: mutation charset /charset choose chars + c@ ;
                                       \ print the current candidate
: .candidate                           ( n1 n2 -- n1 f)
  ." Generation " over 2 .r ." : " this-generation count type cr /target -1 [+] =
;                                      \ test a candidate on
                                       \ THE NUMBER of correct genes
: test-candidate                       ( a -- a n)
  dup target 0 >r >r                   ( a1 a2)
  begin                                ( a1 a2)
    r@                                 ( a1 a2 n)
  while                                ( a1 a2)
    over c@ over c@ =                  ( a1 a2 n)
    r> r> rot if 1+ then >r 1- >r      ( a1 a2)
    char+ swap char+ swap              ( a1+1 a2+1)
  repeat                               ( a1+1 a2+1)
  drop drop r> drop r>                 ( a n)
;
                                       \ find the best candidate
: get-candidate                        ( -- n)
  #copies 0 >r >r                      ( --)
  begin                                ( --)
    r@                                 ( n)
  while                                ( --)
    r@ 1- new-generation               ( a)
    test-candidate r'@ over <          ( a n f)
    if swap count this-generation place r> 1- swap r> drop >r >r
    else drop drop r> 1- >r then       ( --)
  repeat                               ( --)
  r> drop r>                           ( n)
;
                                       \ generate a new candidate
: make-candidate                       ( a --)
  dup charset count rot place          ( a1)
  this-generation target >r            ( a1 a2 a3)
  begin                                ( a1 a2 a3)
    r@                                 ( a1 a2 a3 n)
  while                                ( a1 a2 a3)
    over c@ over c@ =                  ( a1 a2 a3 f)
    swap >r >r over r>                 ( a1 a2 a1 f)
    if over c@ else mutation then      ( a1 a2 a1 c)
    swap c! r> r> 1- >r                ( a1 a2 a3)
    char+ rot char+ rot char+ rot      ( a1+1 a2+1 a3+1)
  repeat                               ( a1+1 a2+1 a3+1)
  drop drop drop r> drop               ( --)
;
                                       \ make a whole new generation
: make-generation #copies 0 do i new-generation make-candidate loop ;
                                       \ weasel program
: weasel
  s"  ABCDEFGHIJKLMNOPQRSTUVWXYZ " 2dup
  charset place                        \ initialize the characterset
  this-generation place 0              \ initialize the first generation
  begin                                \ start the program
    1+ make-generation                 \ make a new generation
    get-candidate .candidate           \ select the best candidate
  until drop                           \ stop when we've found perfection
;

weasel
