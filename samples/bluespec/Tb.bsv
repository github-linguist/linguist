// Copyright 2008 Bluespec, Inc.  All rights reserved.

// ================================================================
// Small Example Suite: Example 1a
// Very basic 'Hello World' example just to go through the
// mechanics of actually building and running a small BSV program
// (and not intended to teach anything about hardware!).
// ================================================================

package Tb;

(* synthesize *)
module mkTb (Empty);

   rule greet;
      $display ("Hello World!");
      $finish (0);
   endrule

endmodule: mkTb

endpackage: Tb

// ================================================================
