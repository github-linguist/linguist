package TbTL;

import TL::*;

interface Lamp;
   method Bool changed;
   method Action show_offs;
   method Action show_ons;
   method Action reset;
endinterface

module mkLamp#(String name, Bool lamp)(Lamp);
   Reg#(Bool) prev <- mkReg(False);

   method changed = (prev != lamp);

   method Action show_offs;
      if (prev && !lamp)
      $write (name + " off, ");
   endmethod

   method Action show_ons;
      if (!prev && lamp)
      $write (name + " on, ");
   endmethod

   method Action reset;
      prev <= lamp;
   endmethod
endmodule


(* synthesize *)
module mkTest();
   let dut <- sysTL;

   Reg#(Bit#(16)) ctr <- mkReg(0);

   Reg#(Bool) carN <- mkReg(False);
   Reg#(Bool) carS <- mkReg(False);
   Reg#(Bool) carE <- mkReg(False);
   Reg#(Bool) carW <- mkReg(False);

   Lamp lamps[12];

   lamps[0] <- mkLamp("0:  NS  red  ", dut.lampRedNS);
   lamps[1] <- mkLamp("1:  NS  amber", dut.lampAmberNS);
   lamps[2] <- mkLamp("2:  NS  green", dut.lampGreenNS);
   lamps[3] <- mkLamp("3:  E   red  ", dut.lampRedE);
   lamps[4] <- mkLamp("4:  E   amber", dut.lampAmberE);
   lamps[5] <- mkLamp("5:  E   green", dut.lampGreenE);
   lamps[6] <- mkLamp("6:  W   red  ", dut.lampRedW);
   lamps[7] <- mkLamp("7:  W   amber", dut.lampAmberW);
   lamps[8] <- mkLamp("8:  W   green", dut.lampGreenW);

   lamps[9]  <- mkLamp("9:  Ped red  ", dut.lampRedPed);
   lamps[10] <- mkLamp("10: Ped amber", dut.lampAmberPed);
   lamps[11] <- mkLamp("11: Ped green", dut.lampGreenPed);

   rule start (ctr == 0);
      $dumpvars;
   endrule

   rule detect_cars;
      dut.set_car_state_N(carN);
      dut.set_car_state_S(carS);
      dut.set_car_state_E(carE);
      dut.set_car_state_W(carW);
   endrule

   rule go;
      ctr <= ctr + 1;
      if (ctr == 5000) carN <= True;
      if (ctr == 6500) carN <= False;
      if (ctr == 12_000) dut.ped_button_push;
   endrule

   rule stop (ctr > 32768);
      $display("TESTS FINISHED");
      $finish(0);
   endrule

   function do_offs(l) = l.show_offs;
      function do_ons(l) = l.show_ons;
      function do_reset(l) = l.reset;

      function do_it(f);
         action
         for (Integer i=0; i<12; i=i+1)
            f(lamps[i]);
         endaction
      endfunction

      function any_changes();
         Bool b = False;
         for (Integer i=0; i<12; i=i+1)
             b = b || lamps[i].changed;
         return b;
      endfunction

      rule show (any_changes());
      do_it(do_offs);
      do_it(do_ons);
      do_it(do_reset);
      $display("(at time %d)", $time);
   endrule
endmodule

endpackage
