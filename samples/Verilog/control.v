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
module control(clk,en,dsp_sel,an);
        input clk, en;
        output [1:0]dsp_sel;
        output [3:0]an;
        wire a,b,c,d,e,f,g,h,i,j,k,l;
        
        assign an[3] = a;
        assign an[2] = b;
        assign an[1] = c;
        assign an[0] = d;
        
        assign dsp_sel[1] = e;
        
        assign dsp_sel[0] = i;
        
        
                FDRSE #(
                .INIT(1'b0) // Initial value of register (1'b0 or 1'b1)
                ) DFF3(
                .Q(a), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(d), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF2(
                .Q(b), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(a), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF1(
                .Q(c), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(b), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF0(
                .Q(d), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(c), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                
                
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF7(
                .Q(e), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(h), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF6(
                .Q(f), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(e), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );              
                FDRSE #(
                .INIT(1'b0) // Initial value of register (1'b0 or 1'b1)
                ) DFF5(
                .Q(g), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(f), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b0) // Initial value of register (1'b0 or 1'b1)
                ) DFF4(
                .Q(h), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(g), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                
                
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF11(
                .Q(i), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(l), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b0) // Initial value of register (1'b0 or 1'b1)
                ) DFF10(
                .Q(j), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(i), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );              
                FDRSE #(
                .INIT(1'b1) // Initial value of register (1'b0 or 1'b1)
                ) DFF9(
                .Q(k), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(j), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
                FDRSE #(
                .INIT(1'b0) // Initial value of register (1'b0 or 1'b1)
                ) DFF8(
                .Q(l), // Data output
                .C(clk), // Clock input
                .CE(en), // Clock enable input
                .D(k), // Data input
                .R(1'b0), // Synchronous reset input
                .S(1'b0) // Synchronous set input
                );
endmodule
