`ifndef INCLUDED
  `include "hdl/output_struct_package.sv"
  `define INCLUDED
`endif

import output_struct_package::*;
import xtlm_pkg::*; // For trans-language TLM channels.
class scoreboard;

    xtlm_fifo #(int) monitorChannel;
    function new ();
   begin
        monitorChannel = new ("top.outputpipe");
    end
    endfunction

    task run(ref output_struct received[5000]);
	logic [14:0] count = 0;
	#5000
        while (1)
        begin
			output_struct one_transaction;
			monitorChannel.get(one_transaction);
			if(one_transaction != '0)
			begin
				received[count] = one_transaction;
				count = count + 1;
			end
			else
				break;
			$display( " %p ", one_transaction);
        end
        $display("\nHVL: Complete message received.\n");
        $finish();
    endtask
    
endclass

class stimulus_gen ;

    xtlm_fifo #(int) driverChannel;
    function new();
        driverChannel = new ("top.inputpipe");
    endfunction

    task run(input logic [14:0][7:0] memory);
        logic [8:0] memory_tosend;
		logic [14:0] count; 
		for (count = 0 ; count <= 15'b111111111111111; count = count + 1)		
        begin
			if(count == '1)
				memory_tosend = {memory[count], 1'b0};
			else 
				memory_tosend = {memory[count],1'b1};
			driverChannel.put(memory_tosend);
        end
        driverChannel.flush_pipe;
		$display("Memory has been sent");
    endtask

endclass


module tbTop;

	output_struct received[5000];
	logic [14:0][7:0] memory = '0;
    
	scoreboard scb;
    stimulus_gen stim_gen;
	

    task run();
        fork
        begin
           scb.run(received);
        end
        join_none
        fork
        begin
           stim_gen.run(memory);
        end
        join_none
    endtask

    initial 
    begin
		memory[0] = 8'b01000010; // Load 2 into A
		memory[1] = 8'b01100011; // Load 3 into B
		memory[2] = 8'b00000010; // Mov A to C
		memory[3] = 8'b10000000; // ALU ADD into A
		memory[4] = 8'b10101110; // Halt execution
        scb = new();
        stim_gen = new();
        run();
    end

endmodule
 


