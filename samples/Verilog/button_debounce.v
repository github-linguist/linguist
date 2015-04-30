////////////////////////////////////////////////////////////////////////////////
// Original Author: Schuyler Eldridge
// Contact Point: Schuyler Eldridge (schuyler.eldridge@gmail.com)
// button_debounce.v
// Created: 10/10/2009
// Modified: 3/20/2012
//
// Counter based debounce circuit originally written for EC551 (back
// in the day) and then modified (i.e. chagned entirely) into 3 always
// block format. This debouncer generates a signal that goes high for
// 1 clock cycle after the clock sees an asserted value on the button
// line. This action is then disabled until the counter hits a
// specified count value that is determined by the clock frequency and
// desired debounce frequency. An alternative implementation would not
// use a counter, but would use the shift register approach, looking
// for repeated matches (say 5) on the button line.
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
module button_debounce
  (
   input      clk,     // clock
   input      reset_n, // asynchronous reset 
   input      button,  // bouncy button
   output reg debounce // debounced 1-cycle signal
   );
  
  parameter
    CLK_FREQUENCY  = 66000000,
    DEBOUNCE_HZ    = 2;
  // These parameters are specified such that you can choose any power
  // of 2 frequency for a debouncer between 1 Hz and
  // CLK_FREQUENCY. Note, that this will throw errors if you choose a
  // non power of 2 frequency (i.e. count_value evaluates to some
  // number / 3 which isn't interpreted as a logical right shift). I'm
  // assuming this will not work for DEBOUNCE_HZ values less than 1,
  // however, I'm uncertain of the value of a debouncer for fractional
  // hertz button presses.
  localparam
    COUNT_VALUE  = CLK_FREQUENCY / DEBOUNCE_HZ,
    WAIT         = 0,
    FIRE         = 1,
    COUNT        = 2;

  reg [1:0]   state, next_state;
  reg [25:0]  count;
  
  always @ (posedge clk or negedge reset_n)
    state <= (!reset_n) ? WAIT : next_state;

  always @ (posedge clk or negedge reset_n) begin
    if (!reset_n) begin
      debounce <= 0;
      count    <= 0;
    end
    else begin
      debounce <= 0;
      count    <= 0;
      case (state)
        WAIT: begin
        end
        FIRE: begin
          debounce <= 1;
        end
        COUNT: begin
          count <= count + 1;
        end
      endcase 
    end
  end

  always @ * begin
    case (state)
      WAIT:    next_state = (button)                  ? FIRE : state;
      FIRE:    next_state = COUNT;
      COUNT:   next_state = (count > COUNT_VALUE - 1) ? WAIT : state;
      default: next_state = WAIT;
    endcase
  end

endmodule
