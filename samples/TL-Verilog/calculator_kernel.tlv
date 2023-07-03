\m4_TLV_version 1d: tl-x.org
\SV
m4+definitions(['
   m4_include_url(['https://raw.githubusercontent.com/stevehoover/makerchip_examples/ceebf870862e222ebc6eaf92d5a7fb85a525d069/1st-claas_template_with_macros.tlv'])
'])
m4_makerchip_module_with_random_kernel_tb(my, ['assign passed = cyc_cnt > 20;'])
m4+definitions([''])  // A hack to reset line alignment to address the fact that the above macro is multi-line.
\TLV
   // A default direct hookup from |in to |out.
   m4+tlv_wrapper(|in, @0, |out, @0, /trans)
   |in
      @0
         $data[31:0] = >>1$output;
      @1
         // Calculator logic


         // Extract input fields from input data
         $val1[31:0] = >>1$output;
         $val2[31:0] = /trans$data[31:0];
         $op[2:0] = /trans$data[34:32];

         //counter
         $counter = $reset?0:(>>1$counter+1);
         $valid = $reset || $counter;

      ?$valid
         @1
            $sum[31:0] = $val1[31:0] + $val2[31:0];
            $diff[31:0] = $val1[31:0] - $val2[31:0];
            $mult[31:0] = $val1[31:0] * $val2[31:0];
            $quot[31:0] = $val1[31:0] / $val2[31:0];
             //@2
            $mem[31:0] =
               $reset ? 0:
               ($op[2:0]==3'b101)
                  ? >>1$mem[31:0] : >>1$output;


            $output[31:0] =
               $reset ? 0:
               ($op[2:0]==3'b000)
                  ? $sum[31:0] :
               ($op[2:0]==3'b001)
                  ? $diff[31:0] :
               ($op[2:0]==3'b010)
                  ? $mult[31:0] :
               ($op[2:0]==3'b011)
                  ? $quot[31:0] :
               ($op[2:0]==3'b100)
                  ? >>1$mem[31:0] : $val1[31:0];


   |out
      @0
         // Hook up inputs to outputs to implement a no-op kernel.
         // Delete this to add your kernel.
         $ANY = /top|in<>0$ANY;

         $ready = *out_ready;
         *out_avail = $avail;
         *out_data = $data;
         //`BOGUS_USE($op $rand $ready)
         `BOGUS_USE($ready)

         // Extract output data to the output field
         /trans@0$data = |in@1$output;

   m4+rename_flow(/top, |in, @0, |out, @0, /trans)
\SV
   endmodule