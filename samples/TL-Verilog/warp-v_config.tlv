
Viewed
@@ -0,0 +1,43 @@
\m4_TLV_version 1d: tl-x.org
m4+definitions(['
   // This file can be used to build a coarsely-configured implementation of WARP-V within makerchip.com.
   // This enables exploration of a generated model (as TL-Verilog (without M4 macros) and/or Verilog/SystemVerilog).
   // Simulation waveforms can be explored.
   //
   // High-level settings are described here. For detailed settings, consult the included warp-v.tlv file.


   // -----------------------------------------------------------
   // CONFIGURATION:

   // ISA. Legal Values: [MINI, RISCV, MIPSI, POWER, DUMMY]
   m4_define(M4_ISA, RISCV)

   // Standard configuration. Legal Values: [1-stage, 4-stage, 6-stage, none (and define individual parameters)]
   m4_define(M4_STANDARD_CONFIG, 4-stage)


   // OPTIONAL CONFIGURATION:

   // Branch predictor. Legal Values: [fallthrough, two_bit]
   // m4_define(M4_BRANCH_PRED, fallthrough)


   // DETAILED CONFIGURATION:

   // (See included warp-v.tlv)

   // -----------------------------------------------------------

'])
\SV
   // Include WARP-V.
   m4_include_lib(['https://raw.githubusercontent.com/stevehoover/warp-v/master/warp-v.tlv'])

m4+module_def
\TLV
   m4+warpv()
   m4+warpv_makerchip_cnt10_tb()
   m4+makerchip_pass_fail()
\SV
   endmodule