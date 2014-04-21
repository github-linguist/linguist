module fifo (
    input           clk_50,
    input           clk_2,
    input           reset_n,
    output [7:0]    data_out,
    output          empty
);
