// Copyright (c) Runtime Verification, Inc. All Rights Reserved.

module RAND-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Pgm ::= ".Pgm" | Int ";" Pgm | runRand ( )
endmodule


module RAND
  imports RAND-SYNTAX
  imports DOMAINS

  configuration <k> /home/mihai/git/linguist/samples/KFrameworkgm </k>
                <rands> .List </rands>

  rule <k> I ; P => srandInt(I) ~> runRand() ~> P ... </k>

  rule <k> runRand() => .K ... </k>
       <rands> ... .List => ListItem(randInt(1000)) </rands>
endmodule
