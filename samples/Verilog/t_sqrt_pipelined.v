////////////////////////////////////////////////////////////////////////////////
// Original Author: Schuyler Eldridge
// Contact Point: Schuyler Eldridge (schuyler.eldridge@gmail.com)
// t_sqrt_pipelined.v
// Created: 4.2.2012
// Modified: 4.5.2012
//
// Testbench for generic sqrt operation
// 
// Copyright (C) 2012 Schuyler Eldridge, Boston University
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
////////////////////////////////////////////////////////////////////////////////
`timescale 1ns / 1ps
module t_sqrt_pipelined();

  parameter 
    INPUT_BITS  = 4;
  localparam
    OUTPUT_BITS  = INPUT_BITS / 2 + INPUT_BITS % 2;
  
  reg [INPUT_BITS-1:0] radicand;
  reg                  clk, start, reset_n;

  wire [OUTPUT_BITS-1:0] root;
  wire                   data_valid;
//  wire [7:0] root_good;

  sqrt_pipelined 
    #(
      .INPUT_BITS(INPUT_BITS)
      )
    sqrt_pipelined 
      (
       .clk(clk),
       .reset_n(reset_n),
       .start(start),
       .radicand(radicand), 
       .data_valid(data_valid),
       .root(root)
       );

  initial begin
    radicand     = 16'bx; clk = 1'bx; start = 1'bx; reset_n = 1'bx;;
    #10 reset_n  = 0; clk = 0;
    #50 reset_n  = 1; radicand = 0;
//    #40 radicand  = 81; start = 1;
//    #10 radicand  = 16'bx; start = 0;
    #10000 $finish;
  end

  always
    #5 clk = ~clk;

  always begin
    #10 radicand  = radicand + 1; start = 1;
    #10 start     = 0;
  end
  

//  always begin
//    #80 start  = 1;
//    #10 start  = 0;
//  end

endmodule

