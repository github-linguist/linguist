// http://hdlsnippets.com/parameterized_priority_encoder
module priority_encoder #(parameter INPUT_WIDTH=8,OUTPUT_WIDTH=3)
(
 input  logic [INPUT_WIDTH-1:0]  input_data,
 output logic [OUTPUT_WIDTH-1:0] output_data
);
 
int ii;
 
always_comb
begin
  output_data = 'b0;
	for(ii=0;ii<INPUT_WIDTH;ii++)
		if (input_data[ii])
			output_data = ii[OUTPUT_WIDTH-1:0];
end

endmodule
