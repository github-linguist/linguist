package TL;

interface TL;
   method Action ped_button_push();

         (* always_enabled *)
         method Action set_car_state_N(Bool x);
         (* always_enabled *)
         method Action set_car_state_S(Bool x);
         (* always_enabled *)
         method Action set_car_state_E(Bool x);
         (* always_enabled *)
         method Action set_car_state_W(Bool x);

   method Bool lampRedNS();
   method Bool lampAmberNS();
   method Bool lampGreenNS();

   method Bool lampRedE();
   method Bool lampAmberE();
   method Bool lampGreenE();

   method Bool lampRedW();
   method Bool lampAmberW();
   method Bool lampGreenW();

   method Bool lampRedPed();
   method Bool lampAmberPed();
   method Bool lampGreenPed();
endinterface: TL

typedef enum {
         AllRed,
         GreenNS, AmberNS,
         GreenE, AmberE,
         GreenW, AmberW,
         GreenPed, AmberPed} TLstates deriving (Eq, Bits);

typedef UInt#(5) Time32;
typedef UInt#(20) CtrSize;

(* synthesize *)
module sysTL(TL);
   Time32 allRedDelay = 2;
   Time32 amberDelay = 4;
   Time32 nsGreenDelay = 20;
   Time32 ewGreenDelay = 10;
   Time32 pedGreenDelay = 10;
   Time32 pedAmberDelay = 6;

   CtrSize clocks_per_sec = 100;

   Reg#(TLstates) state <- mkReg(AllRed);
   Reg#(TLstates) next_green <- mkReg(GreenNS);
   Reg#(Time32) secs <- mkReg(0);
   Reg#(Bool) ped_button_pushed <- mkReg(False);
         Reg#(Bool) car_present_N <- mkReg(True);
   Reg#(Bool) car_present_S <- mkReg(True);
   Reg#(Bool) car_present_E <- mkReg(True);
   Reg#(Bool) car_present_W <- mkReg(True);
         Bool car_present_NS = car_present_N || car_present_S;
   Reg#(CtrSize) cycle_ctr <- mkReg(0);

   rule dec_cycle_ctr (cycle_ctr != 0);
      cycle_ctr <= cycle_ctr - 1;
   endrule

   Rules low_priority_rule = (rules
                        rule inc_sec (cycle_ctr == 0);
                                 secs <= secs + 1;
                                 cycle_ctr <= clocks_per_sec;
                        endrule endrules);

   function Action next_state(TLstates ns);
         action
                                 state <= ns;
                                 secs <= 0;
      endaction
   endfunction: next_state

   function TLstates green_seq(TLstates x);
      case (x)
                                 GreenNS: return (GreenE);
                                 GreenE:  return (GreenW);
                                 GreenW:  return (GreenNS);
      endcase
   endfunction

   function Bool car_present(TLstates x);
      case (x)
                                 GreenNS: return (car_present_NS);
                                 GreenE:  return (car_present_E);
                                 GreenW:  return (car_present_W);
      endcase
   endfunction

   function Rules make_from_green_rule(TLstates green_state, Time32 delay, Bool car_is_present, TLstates ns);
                        return (rules
                                 rule from_green (state == green_state && (secs >= delay || !car_is_present));
                                                next_state(ns);
                                 endrule endrules);
   endfunction: make_from_green_rule

   function Rules make_from_amber_rule(TLstates amber_state, TLstates ng);
      return (rules
                                 rule from_amber (state == amber_state && secs >= amberDelay);
                                                next_state(AllRed);
                                                next_green <= ng;
                                 endrule endrules);
   endfunction: make_from_amber_rule

   Rules hprs[7];

   hprs[1] = make_from_green_rule(GreenNS, nsGreenDelay, car_present_NS, AmberNS);
   hprs[2] = make_from_amber_rule(AmberNS, GreenE);
   hprs[3] = make_from_green_rule(GreenE, ewGreenDelay, car_present_E, AmberE);
   hprs[4] = make_from_amber_rule(AmberE, GreenW);
   hprs[5] = make_from_green_rule(GreenW, ewGreenDelay, car_present_W, AmberW);
   hprs[6] = make_from_amber_rule(AmberW, GreenNS);

         hprs[0] = (rules
                        rule fromAllRed (state == AllRed && secs >= allRedDelay);
                                 if (ped_button_pushed) action
                                                ped_button_pushed <= False;
                                                next_state(GreenPed);
                                 endaction else if (car_present(next_green))
                              next_state(next_green);
                           else if (car_present(green_seq(next_green)))
                                    next_state(green_seq(next_green));
         else if (car_present(green_seq(green_seq(next_green))))
                                    next_state(green_seq(green_seq(next_green)));
         else
                                    noAction;
                        endrule: fromAllRed endrules);

         Rules high_priority_rules = hprs[0];
   for (Integer i = 1; i<7; i=i+1)
      high_priority_rules = rJoin(hprs[i], high_priority_rules);

         addRules(preempts(high_priority_rules, low_priority_rule));

   method Action ped_button_push();
      ped_button_pushed <= True;
   endmethod: ped_button_push

   method Action set_car_state_N(b) ; car_present_N <= b; endmethod
   method Action set_car_state_S(b) ; car_present_S <= b; endmethod
   method Action set_car_state_E(b) ; car_present_E <= b; endmethod
   method Action set_car_state_W(b) ; car_present_W <= b; endmethod

   method lampRedNS() = (!(state == GreenNS || state == AmberNS));
   method lampAmberNS() = (state == AmberNS);
   method lampGreenNS() = (state == GreenNS);
   method lampRedE() = (!(state == GreenE || state == AmberE));
   method lampAmberE() = (state == AmberE);
   method lampGreenE() = (state == GreenE);
   method lampRedW() = (!(state == GreenW || state == AmberW));
   method lampAmberW() = (state == AmberW);
   method lampGreenW() = (state == GreenW);

   method lampRedPed() = (!(state == GreenPed || state == AmberPed));
   method lampAmberPed() = (state == AmberPed);
   method lampGreenPed() = (state == GreenPed);

endmodule: sysTL

endpackage: TL
