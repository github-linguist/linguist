////////////////////////////////////////////////////////////////////////////////
// Original Author: Schuyler Eldridge
// Contact Point: Schuyler Eldridge (schuyler.eldridge@gmail.com)
// sign_extender.v
// Created: 5.16.2012
// Modified: 5.16.2012
//
// Generic sign extension module
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
`timescale 1ns/1ps
module sign_extender
  #(
    parameter
    INPUT_WIDTH = 8,
    OUTPUT_WIDTH = 16
    )
  (
   input [INPUT_WIDTH-1:0] original,
   output reg [OUTPUT_WIDTH-1:0] sign_extended_original
   );

  wire [OUTPUT_WIDTH-INPUT_WIDTH-1:0] sign_extend;

  generate
    genvar                           i;
    for (i = 0; i < OUTPUT_WIDTH-INPUT_WIDTH; i = i + 1) begin : gen_sign_extend
      assign sign_extend[i]  = (original[INPUT_WIDTH-1]) ? 1'b1 : 1'b0;
    end
  endgenerate

  always @ * begin
    sign_extended_original  = {sign_extend,original};
  end

endmodule
