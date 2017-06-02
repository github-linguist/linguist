module C-EXPR-ASSIGNMENT
     imports BOOL
     imports K-REFLECTION
     imports C-BITSIZE-SYNTAX
     imports C-DYNAMIC-SYNTAX
     imports C-ERROR-SYNTAX
     imports C-MEMORY-WRITING-SYNTAX
     imports C-SYMLOC-SYNTAX
     imports C-SYNTAX
     imports C-TYPING-SYNTAX

     rule nclv(Loc:SymLoc, T'::Type) := tv(V:CValue, T::UType)
          => checkOverlap(stripProv(Loc), T, T')
          ~> write(Loc, V, T')
          ~> tv(V, T)
          requires notBool isRestrictType(T')
               andBool notBool fromArrayStatic(T')
               andBool (T ==Type utype(T'))
          [structural]
     rule nclv(Loc:SymLoc, T'::Type) := tv(V:CValue, T::UType)
          => checkOverlap(stripProv(Loc), T, T')
          ~> checkArrayStaticAssign(T', V)
          ~> write(Loc, V, T')
          ~> tv(V, T)
          requires notBool isRestrictType(T')
               andBool fromArrayStatic(T')
               andBool (T ==Type utype(T'))
          [structural]
     rule nclv(Loc:SymLoc, T'::Type) := tv(V:SymLoc, T::UType)
          => checkOverlap(stripProv(Loc), T, T')
          ~> checkRestrictAssign(getRestrictBlock(T'), getRelevantTags(Loc, V))
          ~> write(Loc, V, T')
          ~> tv(V, T)
          requires isRestrictType(T')
               andBool (T ==Type utype(T'))
          [structural]
     rule nclv(Loc:SymLoc, T'::Type) := tv(V:SymLoc, T::UType)
          => checkOverlap(stripProv(Loc), T, T')
          ~> checkArrayStaticAssign(T', V)
          ~> checkRestrictAssign(getRestrictBlock(T'), getRelevantTags(Loc, V))
          ~> write(Loc, V, T')
          ~> tv(V, T)
          requires isRestrictType(T')
               andBool fromArrayStatic(T')
               andBool (T ==Type utype(T'))
          [structural]

     syntax KItem ::= checkOverlap(SymLoc, UType, Type)
     rule checkOverlap(Loc:SymLoc, T::UType, T'::Type) => .K
          requires notBool hasReadFrom(T)
               orBool (getReadFromLoc(T) ==K Loc
                   andBool getReadFromLen(T) ==K byteSizeofType(T'))
               orBool notBool sameBase(getReadFromLoc(T), Loc)
               orBool (getReadFromLoc(T) +bytes byteSizeofType(T)) <=bytes Loc
               orBool (Loc +bytes byteSizeofType(T)) <=bytes getReadFromLoc(T)
          [structural]
     rule (.K => UNDEF("EEA1",
               "Assignment from non-exactly-overlapping overlapping read."))
          ~> checkOverlap(Loc:SymLoc, T::UType, T'::Type)
          requires hasReadFrom(T)
               andBool sameBase(getReadFromLoc(T), Loc)
               andBool (getReadFromLoc(T) =/=K Loc
                   orBool getReadFromLen(T) =/=K byteSizeofType(T'))
               andBool (getReadFromLoc(T) +bytes byteSizeofType(T)) >bytes Loc
               andBool (Loc +bytes byteSizeofType(T)) >bytes getReadFromLoc(T)
          [structural]

     syntax Set ::= getRelevantTags(SymLoc, SymLoc) [function]
     rule getRelevantTags(Loc':SymLoc, Loc:SymLoc)
          => getRelevantTags'(.K, stripProv(Loc'), getBases(Loc))

     syntax Set ::= "getRelevantTags'" "(" K "," SymLoc "," Set ")" [function]
     rule getRelevantTags'((.K => K), _, (SetItem(K:K) => .Set) _)
     rule getRelevantTags'((basedOn(Base, _) => .K), loc(Base:SymBase, _), _)
     rule getRelevantTags'(basedOn(Base':SymBase, Tag:Scope),
               loc(Base:SymBase, Offset:Int), Bases:Set)
          => SetItem(Tag) getRelevantTags'(.K, loc(Base:SymBase, Offset), Bases)
          requires Base =/=K Base'
     rule getRelevantTags'(.K, _, .Set) => .Set

     syntax KItem ::= checkRestrictAssign(Scope, Set)
     rule checkRestrictAssign(Tag:Scope, Tags:Set) => .K
          requires ((Tag =/=K fileScope) andBool notBool (Tag in Tags))
               orBool Tag ==K none
     rule (.K => UNDEF("EEA2", "Attempt to alias restrict qualified pointers associated with the same block."))
          ~> checkRestrictAssign(Tag:Scope, Tags:Set)
          requires Tag in Tags
     rule checkRestrictAssign(fileScope, .Set) => .K
     // TODO(chathhorn): more cases here we don't catch.
     rule (.K => UNDEF("EEA3", "Attempt to alias restrict qualified pointers associated with incompatible blocks."))
          ~> checkRestrictAssign(fileScope, Tags:Set)
          requires notBool (fileScope in Tags)
               andBool Tags =/=K .Set

     syntax Bool ::= fromArrayStatic(Type) [function]
     rule fromArrayStatic(t(Qs::Quals, SetItem(K:K) S:Set, T::SimpleType))
          => (getKLabel(K) ==K #klabel(`arrayStatic`)) orBool fromArrayStatic(t(Qs, S, T))
     rule fromArrayStatic(_) => false [owise]

     syntax KItem ::= checkArrayStaticAssign(Type, CValue)
                    | "checkArrayStaticAssign'" "(" Type "," Modifier "," Provenance ")"

     rule checkArrayStaticAssign(t(_, Mods:Set, pointerType(T:Type)), loc(_, _, Prov:Set))
          => checkArrayStaticAssign'(T,
               {getFirstMatch(Mods, #klabel(`arrayStatic`))}:>Modifier,
               {getFirstMatch(Prov, #klabel(`fromArray`))}:>Provenance)
     rule (.K => UNDEF("EEA4",
               "Passing a pointer not to an array object as a function parameter with a static-qualified array size."))
          ~> checkArrayStaticAssign(_, Loc:SymLoc)
          requires notBool isFromArray(Loc)

     rule checkArrayStaticAssign'(T:Type,
               arrayStatic(N:Int), fromArray(Offset:Int, Sz:Int))
          => .K
          requires (Sz -Int Offset) >=Int (N *Int byteSizeofType(T))
     rule (.K => UNDEF("EEA5",
               "Passing a pointer to an array object of fewer elements than required by a function parameter with a static-qualified array size."))
          ~> checkArrayStaticAssign'(T:Type,
               arrayStatic(N:Int), fromArray(Offset:Int, Sz:Int))
          requires (Sz -Int Offset) <Int (N *Int byteSizeofType(T))

     syntax KItem ::= getFirstMatch(Set, K) [function]
                    | "getFirstMatch'" "(" K "," Set "," K ")" [function]

     rule getFirstMatch(S:Set, L:K) => getFirstMatch'(.K, S, L)

     rule getFirstMatch'((.K => K), (SetItem(K:K) => .Set) _, _)
     rule getFirstMatch'((K:K => .K), _, L:K)
          requires getKLabel(K) =/=K L
               andBool K =/=K .K
     rule getFirstMatch'(K:K, _, L:K) => K
          requires getKLabel(K) ==K L
     rule getFirstMatch'(.K, .Set, _) => .K

     rule compoundAssign(LV:KResult, AExp:K)
          => #compoundAssign(LV, LV, AExp)
          [structural]

     syntax KItem ::= #compoundAssign(K, K, K)
     context #compoundAssign(_, (HOLE:KItem => reval(HOLE)), _) [result(RValue)]
     rule #compoundAssign(LV:KResult, V:RValue, AExp:K)
          => fillLHoles(LV, fillRHoles(V, AExp))
          [structural]

endmodule

