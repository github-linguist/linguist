/* Author: Maisee Brown
 * Project: Harry Porter Relay Computer 
 * File: output_struct_package.sv
 * License: MIT http://opensource.org/licenses/MIT
 */
 
 package output_struct_package;
 
	typedef struct packed {
	logic LdA, LdB, LdC, LdD, LdM1, LdM2, LdX, LdY, LdJ1, LdJ2, LdInst, LdPC, LdInc, LdXY, LdCond,
			SelA, SelB, SelC, SelD, SelM1, SelM2, SelX, SelY, SelM, SelXY, SelJ, SelPC, 
			SelInc, MemRead, MemWrite, Halt, carry, sign, zero;
	logic [2:0] alu_function_code;
	logic [23:0] fsm_state;
	logic [7:0] instruction_reg;
	logic [7:0] dataBus;
	logic [15:0] addressBus;
	logic [7:0] A, B, C, D, M1, M2, X, Y, J1, J2, M, XY, J, PC, INC;} output_struct;
	
	function automatic void print_output_struct(output_struct to_print);
		
		stateAndOutput_t convert_to_enum = (stateAndOutput_t)'to_print.fsm_state
		$display (" CONTROL SIGNALS: ");
		$display (" LdA LdB LdC LdD LdM1 LdM2 LdX LdY LdJ1 LdJ2 LdInst LdPC LdInc LdXY LdCond");
		$display (" %b   %b   %b   %b   %b    %b    %b   %b   %b    %b    %b      %b    %b    %b     %b    %b ",
		to_print.LdA, to_print.LdB, to_print.LdC, to_print.LdD, to_print.LdM1, to_print.LdM2, to_print.LdX, to_print.LdY, 
		to_print.LdJ1, to_print.LdJ2, to_print.LdInst, to_print.LdPC, to_print.LdInc, to_print.LdXY, to_print.LdCond);
		$display (" SelA SelB SelC SelD SelM1 SelM2 SelX SelY SelJ1 SelJ2 SelInst SelPC SelInc SelM SelXY SelCond");
		$display (" %b    %b    %b    %b    %b     %b     %b    %b    %b     %b     %b       %b     %b     %b      %b    %b     %b ",
		to_print.SelA, to_print.SelB, to_print.SelC, to_print.SelD, to_print.SelM1, to_print.SelM2, to_print.SelX, to_print.SelY, 
		to_print.SelJ1, to_print.SelJ2, to_print.SelInst, to_print.SelPC, to_print.SelInc, to_print.SelM, to_print.SelXY, to_print.SelCond);
		$display(" MemRead MemWrite carry sign zero ALUcode fsm_state Halt")
		$display(" %b       %b        %b     %b     %b     %b           %b          %b", to_print.MemRead, to_print.MemWrite, to_print.carry
				to_print.sign, to_print.zero, to_print.alu_function_code, to_print.convert_to_enum.name, to_print.Halt);
		$display(" BUSES: (in hex)");
		$display(" Address Bus       Data Bus");
		$display(" %h  %h ", to_print.addressBus, to_print.dataBus);
		$display(" REGISTERS: (in hex)");
		$display(" A      B       C      D      M1      M2      X      Y      J1      J2");
		$display(" %h     %h     %h     %h     %h     %h     %h     %h     %h     %h     %h", to_print.A, to_print.B, to_print.C,
				to_print.D, to_print.M1, to_print.M2, to_print.X, to_print.Y, to_print.J1, to_print.J2);
		$display(" M        XY        J        PC        INC        INST");
		$display(" %h    %h    %h    %h    %h    %h", to_print.M, to_print.XY, to_print.J, to_print.J, to_print.PC, to_print.INC, to_print.instruction_reg);
	
	
	endfunction
 
 endpackage
