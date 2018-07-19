: multiplication-table
  cr 2 spaces  13 2 do i 4 u.r loop
  cr
  13 2 do
    cr i 2 u.r
    13 2 do
      i j < if 4 spaces else i j * 4 u.r then
    loop
  loop ;
