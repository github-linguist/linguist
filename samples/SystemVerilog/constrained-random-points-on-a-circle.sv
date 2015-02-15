program main;

  bit [39:0] bitmap [40];

  class Point;
    rand bit signed [4:0] x;
    rand bit signed [4:0] y;

    constraint on_circle_edge {
      (10*10) <= (x*x + y*y);
      (x*x + y*y) <= (15*15);
    };

    function void do_point();
      randomize;
      bitmap[x+20][y+20] = 1;
    endfunction
  endclass

  initial begin
    Point p = new;
    repeat (100) p.do_point;
    foreach (bitmap[row]) $display( "%b", bitmap[row]);
  end

endprogram
