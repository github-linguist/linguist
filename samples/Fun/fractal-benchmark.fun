fun mandelbrot(ci, y)
  var cr = y - 0.5;
  var zi = 0;
  var zr = 0;

  for i = 1 to 1000 loop
    var zr2  = zr * zr;
    var zi2  = zi * zi;
    if zi2 + zr2 > 16 then
      return i;
    end if;
    var temp = zr * zi;
    zr = zr2 - zi2 + cr;
    zi = temp + temp + ci;
  end loop;
  return 0;
end fun;

fun main()
  for y = -39 to 38 loop
    var s = ''; //?. '';
    for x = -39 to 38 do
      if mandelbrot(x/40, y/40) = 0 then
        s &= '*'; //? '*';
      else
        s &= ' '; //? ' ';
      end if;
    end do;
    ?. s; //
  end loop;
  //?. '';
end fun;

use 'lib-time.fun';
var t = tick();
?. t.show();
main();
?. t.show();
