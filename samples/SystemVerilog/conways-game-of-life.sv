module gol;

  parameter NUM_ROWS = 20;
  parameter NUM_COLS = 32;

  bit [NUM_COLS:1] cell[1:NUM_ROWS];
  bit clk;

  initial begin
    cell[10][10:8] = 3'b111;
    cell[11][10:8] = 3'b100;
    cell[12][10:8] = 3'b010;
    repeat(8) #5 clk = ~clk;
  end

  always @(posedge clk) begin
    foreach (cell[y,x]) begin
      automatic int count = $countones({ cell[y-1][x-1+:3], cell[y][x-1], cell[y][x+1], cell[y+1][x-1+:3] });
      if (count == 3) cell[y][x] <= 1'b1;
      else if (count != 2) cell[y][x] <= 1'b0;
    end
  end

  always @(negedge clk) begin
    $display("--");
    foreach (cell[y]) $display( "  %b", cell[y] );
  end

endmodule
