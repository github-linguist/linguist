declare

  fun {Expr X0 ?X}
     choice
        [L _ R] = {Do [Term &+ Expr] X0 ?X} in add(L R)
     [] [L _ R] = {Do [Term &- Expr] X0 ?X} in sub(L R)
     [] {Term X0 X}
     end
  end

  fun {Term X0 ?X}
     choice
        [L _ R] = {Do [Factor &* Term] X0 ?X} in mul(L R)
     [] [L _ R] = {Do [Factor &/ Term] X0 ?X} in 'div'(L R)
     [] {Factor X0 X}
     end
  end

  fun {Factor X0 ?X}
     choice {Parens Expr X0 X}
     [] {Number X0 X}
     end
  end

  fun {Number X0 X}
     Ds = {Many1 Digit X0 X}
  in
     num(Ds)
  end

  fun {Digit X0 ?X}
     D|!X = X0
  in
     D = choice &0 [] &1 [] &2 [] &3 [] &4 [] &5 [] &6 [] &7 [] &8 [] &9 end
  end

  fun {Many1 Rule X0 ?X}
     choice [{Rule X0 X}]
     [] X1 in {Rule X0 X1}|{Many1 Rule X1 X}
     end
  end

  fun {Parens Rule X0 ?X}
     [_ R _] = {Do [&( Rule &)] X0 X}
  in
     R
  end

  fun {Do Rules X0 ?X}
     Res#Xn = {FoldL Rules
               fun {$ Res#Xi Rule}
                  if {Char.is Rule} then
                     !Rule|X2 = Xi
                  in
                     (Rule|Res) # X2
                  elseif {Procedure.is Rule} then
                     X2 in
                     ({Rule Xi X2}|Res) # X2
                  end
               end
               nil#X0}
  in
     X = Xn
     {Reverse Res}
  end

  %% Returns a singleton list if an AST was found or nil otherwise.
  fun {Parse S}
     {SearchOne fun {$} {Expr S nil} end}
  end

  fun {Eval X}
     case X of
        num(Ds)    then {String.toInt Ds}
     [] add(L R)   then {Eval L} + {Eval R}
     [] sub(L R)   then {Eval L} - {Eval R}
     [] mul(L R)   then {Eval L} * {Eval R}
     [] 'div'(L R) then {Eval L} div {Eval R}
     end
  end

  [AST] = {Parse "((11+15)*15)*2-(3)*4*1"}

in

  {Inspector.configure widgetShowStrings true}
  {Inspect AST}
  {Inspect {Eval AST}}
