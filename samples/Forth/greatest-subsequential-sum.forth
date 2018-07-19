2variable best
variable best-sum

: sum ( array len -- sum )
  0 -rot cells over + swap do i @ + cell +loop ;

: max-sub ( array len -- sub len )
  over 0 best 2!  0 best-sum !
  dup 1 do                  \ foreach length
    2dup i - cells over + swap do   \ foreach start
      i j sum
      dup best-sum @ > if
        best-sum !
        i j best 2!
      else drop then
    cell +loop
  loop
  2drop best 2@ ;

: .array  ." [" dup 0 ?do over i cells + @ . loop ." ] = " sum . ;

create test -1 , -2 , 3 , 5 , 6 , -2 , -1 , 4 , -4 , 2 , -1 ,

test 11 max-sub .array    \ [3 5 6 -2 -1 4 ] = 15
