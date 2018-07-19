program main;

  class Triangle;
    rand bit [7:0] a,b,c,d,e,f,g,h,X,Y,Z;

    function new();
      randomize;
      $display("     [%0d]",                151);
      $display("    [%0d][%0d]",            a, b);
      $display("   [%0d][%0d][%0d]",        40,c,d);
      $display("  [%0d][%0d][%0d][%0d]",    e,f,g,h);
      $display(" [%0d][%0d][%0d][%0d][%0d]",X,11,Y,4,Z);
    endfunction

    constraint structure {
       151 == a + b;

         a == 40 + c;
         b == c + d;

        40 == e + f;
         c == f + g;
         d == g + h;

         e == X + 11;
         f == 11 + Y;
         g == Y + 4;
         h == 4 + Z;
    };

    constraint extra {
         Y == X + Z;
    };

  endclass

  Triangle answer = new;
endprogram
