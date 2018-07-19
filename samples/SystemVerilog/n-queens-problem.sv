program N_queens;

  parameter SIZE_LOG2 = 3;
  parameter SIZE = 1 << SIZE_LOG2;

  `define ABS_DIFF(a,b) (a>b?a-b:b-a)

  class board;
    rand bit [SIZE_LOG2-1:0] row[SIZE];

    constraint rook_moves {
      foreach (row[i]) foreach (row[j]) if (i < j) {
        row[i] != row[j];
      }
    }

    constraint diagonal_moves {
      foreach (row[i]) foreach (row[j]) if (i < j) {
        `ABS_DIFF(row[i], row[j]) != `ABS_DIFF(i,j);
      }
    }

    function void next;
      randomize;
      foreach (row[i]) begin
        automatic bit [SIZE-1:0] x = 1 << row[i];
        $display( "  %b", x );
      end
      $display("--");
    endfunction

  endclass

  board b = new;
  initial repeat(1) b.next;

endprogram
