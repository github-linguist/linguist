/*
 *  PS2 Mouse Interface
 *  Copyright (C) 2010  Donna Polehn <dpolehn@verizon.net>
 *
 *  This file is part of the Zet processor. This processor is free
 *  hardware; you can redistribute it and/or modify it under the terms of
 *  the GNU General Public License as published by the Free Software
 *  Foundation; either version 3, or (at your option) any later version.
 *
 *  Zet is distrubuted in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 *  License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Zet; see the file COPYING. If not, see
 *  <http://www.gnu.org/licenses/>.
 */

module ps2_mouse (
    input clk,                // Clock Input
    input reset,              // Reset Input
    inout ps2_clk,            // PS2 Clock, Bidirectional
    inout ps2_dat,            // PS2 Data, Bidirectional

    input  [7:0] the_command,        // Command to send to mouse
    input        send_command,       // Signal to send
    output       command_was_sent,   // Signal command finished sending
    output       error_communication_timed_out,

    output [7:0] received_data,        // Received data
    output       received_data_en,     // If 1 - new data has been received
    output       start_receiving_data,
    output       wait_for_incoming_data
  );

  // --------------------------------------------------------------------
  // Internal wires and registers Declarations
  // --------------------------------------------------------------------
  wire            ps2_clk_posedge;        // Internal Wires
  wire            ps2_clk_negedge;

  reg    [7:0]    idle_counter;            // Internal Registers
  reg             ps2_clk_reg;
  reg             ps2_data_reg;
  reg             last_ps2_clk;

  reg    [2:0]    ns_ps2_transceiver;        // State Machine Registers
  reg    [2:0]    s_ps2_transceiver;

  // --------------------------------------------------------------------
  // Constant Declarations
  // --------------------------------------------------------------------
  localparam  PS2_STATE_0_IDLE            = 3'h0,        // states
              PS2_STATE_1_DATA_IN         = 3'h1,
              PS2_STATE_2_COMMAND_OUT     = 3'h2,
              PS2_STATE_3_END_TRANSFER    = 3'h3,
              PS2_STATE_4_END_DELAYED     = 3'h4;

  // --------------------------------------------------------------------
  // Finite State Machine(s)
  // --------------------------------------------------------------------
  always @(posedge clk) begin
    if(reset == 1'b1) s_ps2_transceiver <= PS2_STATE_0_IDLE;
    else              s_ps2_transceiver <= ns_ps2_transceiver;
  end

  always @(*) begin
    ns_ps2_transceiver = PS2_STATE_0_IDLE;        // Defaults

    case (s_ps2_transceiver)
    PS2_STATE_0_IDLE:
        begin
            if((idle_counter == 8'hFF) && (send_command == 1'b1))
                ns_ps2_transceiver = PS2_STATE_2_COMMAND_OUT;
            else if ((ps2_data_reg == 1'b0) && (ps2_clk_posedge == 1'b1))
                ns_ps2_transceiver = PS2_STATE_1_DATA_IN;
            else ns_ps2_transceiver = PS2_STATE_0_IDLE;
        end
    PS2_STATE_1_DATA_IN:
        begin
            // if((received_data_en == 1'b1)  && (ps2_clk_posedge == 1'b1))
            if((received_data_en == 1'b1))   ns_ps2_transceiver = PS2_STATE_0_IDLE;
            else                             ns_ps2_transceiver = PS2_STATE_1_DATA_IN;
        end
    PS2_STATE_2_COMMAND_OUT:
        begin
            if((command_was_sent == 1'b1) || (error_communication_timed_out == 1'b1))
                ns_ps2_transceiver = PS2_STATE_3_END_TRANSFER;
            else ns_ps2_transceiver = PS2_STATE_2_COMMAND_OUT;
        end
    PS2_STATE_3_END_TRANSFER:
        begin
            if(send_command == 1'b0) ns_ps2_transceiver = PS2_STATE_0_IDLE;
            else if((ps2_data_reg == 1'b0) && (ps2_clk_posedge == 1'b1))
                ns_ps2_transceiver = PS2_STATE_4_END_DELAYED;
            else ns_ps2_transceiver = PS2_STATE_3_END_TRANSFER;
        end
    PS2_STATE_4_END_DELAYED:
        begin
            if(received_data_en == 1'b1) begin
                if(send_command == 1'b0) ns_ps2_transceiver = PS2_STATE_0_IDLE;
                else                     ns_ps2_transceiver = PS2_STATE_3_END_TRANSFER;
            end
            else ns_ps2_transceiver = PS2_STATE_4_END_DELAYED;
        end

    default:
            ns_ps2_transceiver = PS2_STATE_0_IDLE;
    endcase
  end

  // --------------------------------------------------------------------
  // Sequential logic
  // --------------------------------------------------------------------
  always @(posedge clk) begin
    if(reset == 1'b1)     begin
        last_ps2_clk    <= 1'b1;
        ps2_clk_reg     <= 1'b1;
        ps2_data_reg    <= 1'b1;
    end
    else begin
        last_ps2_clk    <= ps2_clk_reg;
        ps2_clk_reg     <= ps2_clk;
        ps2_data_reg    <= ps2_dat;
    end
  end

  always @(posedge clk) begin
    if(reset == 1'b1) idle_counter <= 6'h00;
    else if((s_ps2_transceiver == PS2_STATE_0_IDLE) && (idle_counter != 8'hFF))
        idle_counter <= idle_counter + 6'h01;
    else if (s_ps2_transceiver != PS2_STATE_0_IDLE)
        idle_counter <= 6'h00;
  end

  // --------------------------------------------------------------------
  // Combinational logic
  // --------------------------------------------------------------------
  assign ps2_clk_posedge = ((ps2_clk_reg == 1'b1) && (last_ps2_clk == 1'b0)) ? 1'b1 : 1'b0;
  assign ps2_clk_negedge = ((ps2_clk_reg == 1'b0) && (last_ps2_clk == 1'b1)) ? 1'b1 : 1'b0;

  assign start_receiving_data      = (s_ps2_transceiver == PS2_STATE_1_DATA_IN);
  assign wait_for_incoming_data    = (s_ps2_transceiver == PS2_STATE_3_END_TRANSFER);

  // --------------------------------------------------------------------
  // Internal Modules
  // --------------------------------------------------------------------
  ps2_mouse_cmdout mouse_cmdout (
    .clk                           (clk),            // Inputs
    .reset                         (reset),
    .the_command                   (the_command),
    .send_command                  (send_command),
    .ps2_clk_posedge               (ps2_clk_posedge),
    .ps2_clk_negedge               (ps2_clk_negedge),
    .ps2_clk                       (ps2_clk),        // Bidirectionals
    .ps2_dat                       (ps2_dat),
    .command_was_sent              (command_was_sent),    // Outputs
    .error_communication_timed_out (error_communication_timed_out)
  );

  ps2_mouse_datain mouse_datain (
    .clk                    (clk),        // Inputs
    .reset                  (reset),
    .wait_for_incoming_data (wait_for_incoming_data),
    .start_receiving_data   (start_receiving_data),
    .ps2_clk_posedge        (ps2_clk_posedge),
    .ps2_clk_negedge        (ps2_clk_negedge),
    .ps2_data               (ps2_data_reg),
    .received_data          (received_data),   // Outputs
    .received_data_en       (received_data_en)
  );

endmodule

