/* Author: Maisee Brown
 * Project: Harry Porter Relay Computer 
 * File: alu_behave.sv
 * License: MIT http://opensource.org/licenses/MIT
 */
 
 module Alu_Behave (  input logic [7:0] b, 
                      input logic [7:0] c,
                      input logic [2:0] function_code,
                      interface bus,
                      output logic sign,
                      output logic carry,
                      output logic zero);
                    

	enum logic[2:0] {	ADD = 3'b000,
						INC = 3'b001,
						AND = 3'b010,
						OR =  3'b011,
						XOR = 3'b100,
						NOT = 3'b101,
						SHL = 3'b110,
						NOP = 3'b111 } ALU_FUNCTIONS;
	logic [7:0] result;
	
		assign bus.dataBusPins = (function_code === 'z || function_code === 'x) ? 'z : result;
  
  always @(function_code)
  begin
    unique case(function_code)
      ADD: result = b + c;
      INC: result = b + 1;
      AND: result = b & c;
      OR:  result = b | c;
      XOR: result = b ^ c;
      NOT: result = ~b;
      SHL: result = {b[6:0],b[7]};
      NOP: result = '0; 
	  default: ; // do nothing
	  
    endcase
  
  set_flags(b, c, result, carry, zero, sign);
  end
  
  function automatic void set_flags(input logic [7:0] b, c, result, ref logic carry, zero, sign);
    carry = 0;
    zero = 0;
    sign = 0;
    // set carry bit
    if(function_code == ADD)
      // carry out in add when operands have same sign and result has a different sign
     if(b[7] == c[7] && result[7] != b[7]) 
        carry = 1'b1;
      else
        carry = 1'b0;
        // carry out in inc when highest possible possitive number increments to -1 
    else if(function_code == INC)
    begin
      if(b[7] == 0 && result[7] == 1)
        carry = 1'b1;
    else
        carry = 1'b0;
    end
    if(result == 0)
      zero = 1'b1;
    else
      zero = 1'b0;
      
    sign = result[7];
  
  endfunction

 endmodule
