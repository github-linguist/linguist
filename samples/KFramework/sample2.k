// Copyright (c) Runtime Verification, Inc. All Rights Reserved.
module IMP-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Pgm ::= Int
endmodule


module IMP
  imports IMP-SYNTAX
  imports DOMAINS
  syntax KResult ::= Int | Bool

  configuration <T color="yellow">
                  <k color="green"> /home/mihai/git/linguist/samples/KFrameworkgm </k>
                  <cpp-enums>
                    <cppenum multiplicity="*" type="Map">
                      <enum-id> .K </enum-id>
                      <enum-type> .K </enum-type>
                      <scoped> false </scoped>
                    </cppenum>
                  </cpp-enums>
                </T>

  syntax Enum

  syntax CppenumCell
  syntax IncompleteInfo ::= "#incomplete"
  syntax EnumInfo ::= CppenumCell | IncompleteInfo
     syntax EnumInfo ::= #getEnumInfo(Enum) [function]

     rule [[ #getEnumInfo(X::Enum) => <cppenum> <enum-id> X </enum-id> B </cppenum> ]]
          <cppenum> <enum-id> X </enum-id> B::Bag </cppenum>

     rule #getEnumInfo(_) => #incomplete [owise]

endmodule
