create a  char a , char b , char c ,
create b  char A , char B , char C ,
create c  char 1 , char 2 , char 3 ,

: main
  3 0 do cr
    a i cells + @ emit
    b i cells + @ emit
    c i cells + @ emit
  loop
  cr
  a b c
  3 0 do cr
    3 0 do
      rot dup @ emit cell+
    loop
  loop
  drop drop drop
;
