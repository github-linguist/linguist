////////////////////////////////////////////////////////////////////////////////
// Original Author: Schuyler Eldridge
// Contact Point: Schuyler Eldridge (schuyler.eldridge@gmail.com)
// div_pipelined.v
// Created: 4.3.2012
// Modified: 4.5.2012
//
// Testbench for div_pipelined.v
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
module t_div_pipelined();

  reg clk, start, reset_n;
  reg [7:0] dividend, divisor;
  wire      data_valid, div_by_zero;
  wire [7:0] quotient, quotient_correct;

  parameter
    BITS  = 8;

  div_pipelined
    #(
      .BITS(BITS)
      )
  div_pipelined
    (
     .clk(clk),
     .reset_n(reset_n),
     .dividend(dividend),
     .divisor(divisor),
     .quotient(quotient),
     .div_by_zero(div_by_zero),
     //     .quotient_correct(quotient_correct),
     .start(start),
     .data_valid(data_valid)
     );

  initial begin
    #10 reset_n  = 0;
    #50 reset_n  = 1;
    #1
    clk          = 0;
    dividend     = -1;
    divisor      = 127;
    #1000 $finish;
  end

//  always
//    #20 dividend  = dividend + 1;

  always begin
    #10 divisor  = divisor - 1; start = 1;
    #10 start    = 0;
  end

  always
    #5 clk  = ~clk;

  
endmodule
  