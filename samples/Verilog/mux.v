`timescale 1ns / 1ps
// Copyright (C) 2008 Schuyler Eldridge, Boston University
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
module mux(opA,opB,sum,dsp_sel,out);
	input [3:0] opA,opB;
	input [4:0] sum;
	input [1:0] dsp_sel;
	output [3:0] out;
	
	reg cout;
	
	always @ (sum)
		begin
			if (sum[4] == 1)
				cout <= 4'b0001;
			else
				cout <= 4'b0000;
		end
	
	reg out;
	
	always @(dsp_sel,sum,cout,opB,opA)
		begin
			if (dsp_sel == 2'b00)
				out <= sum[3:0];
			else if (dsp_sel == 2'b01)
				out <= cout;
			else if (dsp_sel == 2'b10)
				out <= opB;
			else if (dsp_sel == 2'b11)
				out <= opA;
		end

endmodule
