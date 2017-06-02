// Copyright (c) 2013-2016 K Team. All Rights Reserved.

require "unification.k"
require "substitution.k"

module LAMBDA-SYNTAX
  syntax Exp ::= Int | Bool | Id
               | "(" Exp ")"                      [bracket]
               | Exp Exp                          [strict, left]
               > Exp "*" Exp                      [strict, left]
               | Exp "/" Exp                      [strict]
               > Exp "+" Exp                      [strict, left]
               > Exp "<=" Exp                     [strict]
               > "lambda" Id "." Exp
               | "if" Exp "then" Exp "else" Exp   [strict]
               | "let" Id "=" Exp "in" Exp        [strict(2)]
               | "letrec" Id Id "=" Exp "in" Exp
               | "mu" Id "." Exp

  syntax Type ::= "int" | "bool"
                | Type "->" Type
                | "(" Type ")"                    [bracket]

  syntax Exp ::= Type
endmodule

module LAMBDA
  imports LAMBDA-SYNTAX
  imports UNIFICATION
  imports SUBSTITUTION

  syntax KVariable ::= Id
  syntax KResult ::= Type

  configuration <k color="green"> $PGM:Exp </k>
                <tenv color="red"> .Map </tenv>

  rule I:Int => int
  rule B:Bool => bool
  rule <k> X:Id => T ...</k>  <tenv>... X |-> T:Type ...</tenv>
  rule int * int => int
  rule int / int => int
  rule int + int => int
  rule int <= int => bool

  syntax Exp ::= Exp "->" Exp                  [strict]
  rule <k> lambda X:Id . E => ?T:Type -> E ~> tenv(TEnv) ...</k>
       <tenv> TEnv => TEnv[X <- ?T] </tenv>

  rule (T1:Type -> T2:Type) T1 => T2
  rule if bool then T:Type else T => T

  syntax TypeSchema ::= "(" "forall" Set ")" Type
  rule <k> let X = T:Type in E => E ~> tenv(TEnv) ...</k>
       <tenv> TEnv
        => TEnv[X <- (forall #metaKVariables(T) -Set #metaKVariables(tenv(TEnv))) T]
       </tenv>
  rule <k> X:Id => #renameMetaKVariables(T, Tvs) ...</k>
       <tenv>... X |-> (forall Tvs) T ...</tenv>

  rule letrec F X = E in E' => let F = mu F . lambda X . E in E'        [macro]
  rule <k> mu X:Id . E:Exp => (?T:Type -> ?T) E ~> tenv(TEnv) ...</k>
       <tenv> TEnv => TEnv[X <- ?T] </tenv>

  syntax KItem ::= tenv(Map)
  rule <k> T:Type ~> tenv(TEnv) => T ...</k>  <tenv> _ => TEnv </tenv>
endmodule
