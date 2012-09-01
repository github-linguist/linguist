/* Author: Maisee Brown
 * Project: Harry Porter Relay Computer 
 * File: program_control_unit_behave.sv
 * License: MIT http://opensource.org/licenses/MIT
 */
 
 module programControlUnit(buses buses, controlSignals control_signals, programControlInterface PCsigs);
	wire [15:0] address;
 	logic [15:0] addressBus; 
 	logic [15:0] high_impedence = 'z;
	wire trigger;
	assign trigger = (control_signals.SelPCpin === 1 | control_signals.SelINCpin === 1);
	assign buses.addressBusPins = trigger ? addressBus : 'z;

	always_comb
	begin
		if(control_signals.LdJ1pin)
				PCsigs.J1 = buses.dataBusPins;
		else if(control_signals.LdJ2pin)
				PCsigs.J2 = buses.dataBusPins;
		else if(control_signals.LdInstpin)
		  begin
				PCsigs.Inst = buses.dataBusPins;
			end
		else if(control_signals.LdINCpin)
				PCsigs.Inc = buses.addressBusPins;
		else if(control_signals.LdPCpin)
		  begin
		    PCsigs.PC = buses.addressBusPins;
			end
		else if(control_signals.SelPCpin)
		  begin
				addressBus = PCsigs.PCpins;
				$display("addressBus Variable = %h PCpins = %h", addressBus, PCsigs.PCpins, buses.addressBusPins);
			end
		else if(control_signals.SelINCpin)
				addressBus = PCsigs.Incpins;
	end
 
 endmodule
