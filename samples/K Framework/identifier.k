module C-EXPR-IDENTIFIER
     imports C-COMMON-EXPR-EVAL-SYNTAX
     imports C-CONFIGURATION
     imports C-DYNAMIC-SYNTAX
     imports C-ERROR-SYNTAX
     imports C-MEMORY-READING-SYNTAX
     imports COMMON-BUILTIN-SYNTAX
     imports C-SYMLOC-SYNTAX
     imports C-TYPING-SYNTAX
     imports STRING

     /*@ \fromStandard{\source[n1570]{\para{6.5.1}{2}}}{
     An identifier is a primary expression, provided it has been declared as
     designating an object (in which case it is an lvalue) or a function (in
     which case it is a function designator).
     }*/
     rule <k> X:CId => lv(lnew(Base), T) ...</k>
          <env>... X |-> Base::SymBase ...</env>
          <types>... X |-> T::Type ...</types>
          [structural]

     rule instantiate(Loc::SymLoc, T::Type)
          => transferProv(basedOn(base(Loc), getRestrictBlock(T)), read(Loc, T))
          requires isRestrictType(T)
          [structural]
     rule instantiate(Loc::SymLoc, T::Type) => read(Loc, T)
          requires notBool isRestrictType(T)
          [structural]

     // From, to.
     syntax KItem ::= transferProv(Provenance, K) [strict(2)]
     rule transferProv(Prov::Provenance, tv(Loc:SymLoc, T::UType))
          => tv(addProv(Prov, Loc), T)
          [structural]
     rule transferProv(_, tv(V::CValue, T::UType)) => tv(V, T)
          requires notBool isSymLoc(V)
          [structural]
     rule transferProv(_, trap(T::UType)) => trap(T)
          [structural]
     rule transferProv(Prov::Provenance, nclv(Loc::SymLoc, T::Type))
          => lv(addProv(Prov, Loc), T)
          [structural]

     rule <k> (.K => SE("CID1",
               "Trying to look up identifier " +String S +String
               ", but no such identifier is in scope.") )
               ~> Identifier(S:String)
          ...</k>
          <env> Env:Map </env>
          <types> Types:Map </types>
          requires (notBool (Identifier(S) in_keys(Env)))
               andBool (notBool (Identifier(S) in_keys(Types)))
          [structural]
     rule <k> (.K => UNDEF("CID2",
               "No definition found for identifier " +String S +String ".") )
               ~> Identifier(S:String)
          ...</k>
          <env> Env:Map </env>
          <types>... Identifier(S) |-> _ ...</types>
          requires (notBool (Identifier(S) in_keys(Env)))
          [structural]

endmodule

