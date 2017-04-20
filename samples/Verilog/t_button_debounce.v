////////////////////////////////////////////////////////////////////////////////
// Original Author: Schuyler Eldridge
// Contact Point: Schuyler Eldridge (schuyler.eldridge@gmail.com)
// button_debounce.v
// Created:  4.5.2012
// Modified: 4.5.2012
//
// Testbench for button_debounce.v.
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
module t_button_debounce();

  parameter
    CLK_FREQUENCY  = 66000000,
    DEBOUNCE_HZ    = 2;

  reg clk, reset_n, button;
  wire debounce;
  
  button_debounce
    #(
      .CLK_FREQUENCY(CLK_FREQUENCY),
      .DEBOUNCE_HZ(DEBOUNCE_HZ)
      )
  button_debounce
    (
     .clk(clk),
     .reset_n(reset_n),
     .button(button),
     .debounce(debounce)
     );

  initial begin
    clk          = 1'bx; reset_n = 1'bx; button = 1'bx;
    #10 reset_n  = 1;
    #10 reset_n  = 0; clk = 0;
    #10 reset_n  = 1;
    #10 button   = 0;
  end

  always
    #5 clk  = ~clk;

  always begin
    #100 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
    #0.1 button  = ~button;
  end
  
endmodule
