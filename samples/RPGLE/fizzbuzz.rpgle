**free

dcl-s num int(10);

for num = 1 to 25;
  if (%rem(num:3) = 0 and %rem(num:5) = 0);
    dsply ('num - ' + %char(num) + ' FIZZBUZZ');
  elseif (%rem(num:3) = 0);
    dsply ('num - ' + %char(num) + ' FIZZ');
  elseif (%rem(num:5) = 0);
    dsply ('num - ' + %char(num) + ' BUZZ');
  endif;
endfor;

*INLR = *ON;
return;