create counts 26 cells allot

: freq ( filename -- )
  counts 26 cells erase
  slurp-file bounds do
    i c@ 32 or 'a -
    dup 0 26 within if
      cells counts +
      1 swap +!
    else drop then
  loop
  26 0 do
    cr [char] ' emit  'a i + emit  ." ': "
    counts i cells + @ .
  loop ;

s" example.txt" freq
