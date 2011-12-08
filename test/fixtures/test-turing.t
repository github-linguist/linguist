class TuringInterpreter
	 import Scope, BasicScope, ASTNode, OutputHandler, tokens, symPtr, StringSymbol, Symbol, IntSymbol,
		  UserExecutableSymbol, BoolSymbol, ScopeSymbol, ExecutableSymbol, DefineBuiltinSymbols,
		  RangeSymbol, NumericSymbol, RealSymbol, Builtin
	 export construct, RProgram
	 var globalScope, currentScope : ^Scope
	 var o : ^OutputHandler

	 proc construct (out : ^OutputHandler)
		  new BasicScope, globalScope
		  BasicScope (globalScope).ConstructBig ()
		  currentScope := globalScope

		  DefineBuiltinSymbols (currentScope, out)

		  o := out
	 end construct
	 proc Verify (r : ^ASTNode, t : tokens)
		  if r -> token -> token not= t then
				o -> er ("wrong node type. Expected " + intstr (ord (t)) + " got " + intstr (ord (r -> token -> token)))
		  end if
	 end Verify
	 proc Verifystr (r : ^ASTNode, t : string)
		  if r -> token -> text not= t then
				o -> er ("wrong node type. Expected " + t + " got " + (r -> token -> text))
		  end if
	 end Verifystr
	 proc VerifySymbol (s : ^Symbol, t : string)
		  if index (s -> symClass (), t) = 0 then
				o -> er ("wrong symbol type. Expected " + t + " got " + s -> symClass ())
		  end if
	 end VerifySymbol
	 proc VerifyExists (s : ^Symbol)
		  if s = nil then
				o -> er ("Symbol does not exist")
		  end if
	 end VerifyExists
	 fcn Tc (r : ^ASTNode, t : string) : boolean
		  result (r -> token -> text = t)
	 end Tc
	 %scopes
	 proc PushLocal ()
		  var newScope : ^Scope
		  new BasicScope, newScope
		  newScope -> construct (currentScope)
		  currentScope := newScope
		  %o -> de("pushed a local scope")
	 end PushLocal
	 proc PopLocal ()
		  var oldScope := currentScope
		  currentScope := currentScope -> parent
		  free BasicScope, oldScope
		  %o -> de("popped a local scope")
	 end PopLocal
	 %deferred
	 deferred fcn RExpr (r : ^ASTNode) : symPtr
	 deferred fcn RInstructions (r : ^ASTNode) : symPtr
	 %tree walking interpreter
	 proc RPut (r : ^ASTNode)
		  var last : ^ASTNode := r -> getChild (r -> childCount ())
		  if last not= nil then
				%o -> de("printing")
				if last -> token -> text = "." then
					 for i : 1 .. (r -> childCount () - 1)
						  o -> print (RExpr (r -> getChild (i)) -> toString ())
					 end for
				else
					 for i : 1 .. (r -> childCount ())
						  o -> print (RExpr (r -> getChild (i)) -> toString ())
					 end for
					 o -> out ("")
				end if
		  else
				o -> er ("not enough args for put command")
		  end if
	 end RPut
	 proc RGet (r : ^ASTNode)
		  var last : ^ASTNode := r -> getChild (r -> childCount ())
		  if last not= nil then
				if last -> token -> text = "*" then
					 for i : 1 .. (r -> childCount () - 1)
						  %o -> print (RExpr (r -> getChild (i)) -> toString ())
						  var getval : string
						  get getval : *
						  var ret : ^Symbol
						  new StringSymbol, ret
						  ret -> create (o)
						  var cheated := cheat (StringSymbol.ptr, ret)
						  cheated -> setText (getval)
						  currentScope -> define (r -> getChild (i) -> token -> text, ret)
					 end for
				else
					 for i : 1 .. (r -> childCount ())
						  %o -> print (RExpr (r -> getChild (i)) -> toString ())
						  var getval : string
						  get getval
						  var ret : ^Symbol
						  new StringSymbol, ret
						  ret -> create (o)
						  var cheated := cheat (StringSymbol.ptr, ret)
						  cheated -> setText (getval)
						  currentScope -> define (r -> getChild (i) -> token -> text, ret)
					 end for
				end if
		  else
				o -> er ("not enough args for get command")
		  end if
	 end RGet
	 fcn RCallUser (r : ^ASTNode, func : ^UserExecutableSymbol) : ^Symbol
		  %o -> de("calling function " + r -> getChild(1) -> token -> text)
		  PushLocal ()
		  %o -> de("verified symbol is a function")
		  % load up function parameters
		  %Text.Locate(1,1)
		  %put r -> getChild (1) -> token -> text
		  
		  for i : 1 .. (func -> NumArgs ())
				var passedParam := r -> getChild (i + 1)
				% may be nil. But that's what we want.
				currentScope -> define (func -> GetArgName (i), RExpr (passedParam))
		  end for
		  % execute function
		  
		  var rval : ^Symbol := RInstructions (func -> funcnode)
		  
		  PopLocal ()
		  
		  result rval
	 end RCallUser
	 fcn RCall (r : ^ASTNode) : ^Symbol
		  var sym := RExpr (r -> getChild (1))
		  %o -> de("retrieved symbol")
		  if index (sym -> symClass (), "UserExecutableSymbol") ~= 0 then
				var func : ^UserExecutableSymbol := cheat (UserExecutableSymbol.ptr, sym)
				result RCallUser (r, func)
		  elsif index (sym -> symClass (), "ExecutableSymbol") ~= 0 then
				%o -> de("calling built in function")
				var func : ^ExecutableSymbol := cheat (ExecutableSymbol.ptr, sym)
				var params : flexible array 1 .. 0 of ^Symbol

				for i : 2 .. (r -> childCount ())
					 var passedParam := r -> getChild (i)
					 new params, upper (params) + 1
					 params (upper (params)) := RExpr (passedParam)
				end for

				result func -> Eval (params)
		  else
				o -> er ("Not A function! Symclass: " + sym -> symClass ())
				result nil
		  end if
	 end RCall
	 proc RProcCall (r : ^ASTNode)
		  var dummy := RCall (r)
	 end RProcCall
	 fcn RElsifStat (r : ^ASTNode) : boolean

		  var res := RExpr (r -> getChild (1))
		  %o -> de("retrieved symbol")
		  VerifySymbol (res, "BoolSymbol")
		  var boolres : ^BoolSymbol := cheat (BoolSymbol.ptr, res)
		  if boolres -> val then
				PushLocal ()
				var dummy := RInstructions (r -> getChild (2))
				PopLocal ()
		  end if
		  result boolres -> val
	 end RElsifStat
	 proc RElseStat (r : ^ASTNode)
		  PushLocal ()
		  var dummy := RInstructions (r -> getChild (1))
		  PopLocal ()
	 end RElseStat
	 proc RIfStat (r : ^ASTNode)

		  var res := RExpr (r -> getChild (1))
		  %o -> de("retrieved symbol")
		  VerifySymbol (res, "BoolSymbol")
		  var boolres : ^BoolSymbol := cheat (BoolSymbol.ptr, res)
		  if boolres -> val then
				PushLocal ()
				var dummy := RInstructions (r -> getChild (2))
				PopLocal ()
		  else
				for i : 3 .. (r -> childCount ())
					 var elseStat := r -> getChild (i)
					 if elseStat -> token -> text = "else" then
						  RElseStat (elseStat)
						  exit
					 elsif elseStat -> token -> text = "elsif" then
						  var execd := RElsifStat (elseStat)
						  exit when execd
					 end if
				end for
		  end if
	 end RIfStat
	 proc RLoopStat (r : ^ASTNode)
		  loop
				PushLocal ()
				var shouldExit := RInstructions (r -> getChild (1))
				PopLocal ()
				exit when shouldExit ~= nil
		  end loop
	 end RLoopStat
	 proc RForStat (r : ^ASTNode)
		  var rangesym := RExpr (r -> getChild (2))
		  VerifySymbol (rangesym, "RangeSymbol")
		  var rng := cheat (RangeSymbol.ptr, rangesym)
		  %o -> de("gonna loop for " + intstr(rng->low) + " to " + intstr(rng->up) )
		  for i : (rng -> low) .. (rng -> up)
				%o -> de("for looping")
				PushLocal ()
				currentScope -> define (r -> getChild (1) -> token -> text, Builtin.FromInt (i))
				var shouldExit := RInstructions (r -> getChild (3))
				PopLocal ()
				exit when shouldExit ~= nil
		  end for
		  %o -> de("ended loop")
	 end RForStat
	 proc RDecl (r : ^ASTNode)
		  var initval : ^Symbol := RExpr (r -> getChild (1))
		  for i : 2 .. (r -> childCount ())
				currentScope -> define (r -> getChild (i) -> token -> text, initval -> Copy ())
		  end for
	 end RDecl
	 proc RAssign (r : ^ASTNode)
		  var symName := r -> getChild (1)
		  if symName -> token -> text ~= "." then
				%o -> de("assigning to " + r -> getChild (1) -> token -> text)
				currentScope -> assign (r -> getChild (1) -> token -> text, RExpr (r -> getChild (2)) -> Copy ())
		  else
				var givenScope := currentScope -> resolve (symName -> getChild (1) -> token -> text)
				var miniScope : ^ScopeSymbol
				if givenScope = nil then
					 % if scope doesn't exist, create it
					 new ScopeSymbol, miniScope
					 miniScope -> create (o)
					 currentScope -> define (symName -> getChild (1) -> token -> text, miniScope)
				else
					 VerifySymbol (givenScope, "ScopeSymbol")
					 miniScope := cheat (ScopeSymbol.ptr, givenScope)
				end if
				% define the subsymbol

				miniScope -> Define (symName -> getChild (2) -> token -> text, RExpr (r -> getChild (2)) -> Copy ())
		  end if
	 end RAssign
	 proc RIncrement (r : ^ASTNode)
		  var sym1 := RExpr (r -> getChild (1))
		  var sym2 := RExpr (r -> getChild (2))
		  var res := sym1 -> Add (sym2)
		  currentScope -> assign (r -> getChild (1) -> token -> text, res)
	 end RIncrement
	 proc RDecrement (r : ^ASTNode)
		  var sym1 := RExpr (r -> getChild (1))
		  var sym2 := RExpr (r -> getChild (2))
		  var res := sym1 -> Subtract (sym2)
		  currentScope -> assign (r -> getChild (1) -> token -> text, res)
	 end RDecrement
	 % structure
	 proc RInstruc (r : ^ASTNode)
		  Draw.FillBox (maxx - 10, maxy - 10, maxx, maxy, red)
		  %View.Update
		  var tx, ty,tb : int := 0
		  mousewhere(tx,ty,tb)
		  if tb > 0 and tx > maxx - 10 and ty > maxy - 10 then
				quit : 1337
		  end if
		  if Tc (r, "CALL") then
				RProcCall (r)
		  elsif Tc (r, "put") then
				RPut (r)
		  elsif Tc (r, "get") then
				RGet (r)
		  elsif Tc (r, "if") then
				RIfStat (r)
		  elsif Tc (r, "loop") then
				RLoopStat (r)
		  elsif Tc (r, "for") then
				RForStat (r)
		  elsif Tc (r, ":=") then
				RAssign (r)
		  elsif Tc (r, "+=") then
				RIncrement (r)
		  elsif Tc (r, "-=") then
				RDecrement (r)
		  elsif Tc (r, "VARDECL") then
				RDecl (r)
		  end if
	 end RInstruc
	 body fcn RInstructions (r : ^ASTNode) : ^Symbol
		  var ret : ^Symbol := nil
		  Verifystr (r, "INSTRUCTIONS")
		  for i : 1 .. (r -> childCount ())
				var cur := r -> getChild (i)
				if Tc (cur, "result") then
					 ret := RExpr (cur -> getChild (1))
					 exit
				elsif Tc (cur, "exit") then
					 %o -> de("found exit call")
					 if cur -> childCount () > 0 then                          % is an exit when call
						  var cond := RExpr (cur -> getChild (1))
						  VerifySymbol (cond, "BoolSymbol")
						  var boolval := cheat (BoolSymbol.ptr, cond) -> val
						  if boolval then
								ret := cond
								exit
						  end if
					 end if
				else
					 RInstruc (cur)
				end if
		  end for
		  result ret
	 end RInstructions
	 proc RDefineFuncs (r : ^ASTNode)
		  for i : 1 .. (r -> childCount ())
				var cur := r -> getChild (i)
				if Tc (cur, "FUNCDEF") then
					 var funcsym : ^UserExecutableSymbol
					 new UserExecutableSymbol, funcsym
					 funcsym -> create (o)
					 funcsym -> SetNode (cur)

					 currentScope -> define (funcsym -> funcname, funcsym)
					 %o -> de("defined function " + funcsym -> funcname)
				end if
		  end for
	 end RDefineFuncs
	 proc RFirstPass (r : ^ASTNode)
		  RDefineFuncs (r)
	 end RFirstPass
	 proc RProgram (r : ^ASTNode)
		  RFirstPass (r)
		  var dummy := RInstructions (r)
	 end RProgram
	 body fcn RExpr (r : ^ASTNode) : symPtr
		  var ret : symPtr
		  var tname := r -> token -> text
		  %Text.Locate(1,1)
		  %put tname
		  if r -> token -> token = tokens.LITERALSTRING then
				new StringSymbol, ret
				ret -> create (o)
				var cheated := cheat (StringSymbol.ptr, ret)
				cheated -> setText (r -> token -> text)
		  elsif r -> token -> token = tokens.LITERAL then
				var isreal := false
				var txtcopy := r -> token -> text
				for i : 1 .. length (txtcopy)
					 isreal := isreal or (txtcopy (i) = '.')
				end for
				if isreal then
					 new RealSymbol, ret
					 ret -> create (o)
					 RealSymbol (ret).SetValStr (r -> token -> text)
				else
					 new IntSymbol, ret
					 ret -> create (o)
					 var cheated := cheat (IntSymbol.ptr, ret)
					 cheated -> SetValStr (r -> token -> text)
				end if
		  elsif Tc (r, "CALL") then
				%o -> de("returning result of function")
				ret := RCall (r)
		  elsif tname = "true" or tname = "false" then
				new BoolSymbol, ret
				ret -> create (o)
				var cheated := cheat (BoolSymbol.ptr, ret)
				cheated -> SetValStr (r -> token -> text)
		  elsif tname = "-" and (r -> childCount () = 1) then
				var sym1 := RExpr (r -> getChild (1))
				VerifySymbol (sym1, "IntSymbol")
				var num1 := cheat (IntSymbol.ptr, sym1) -> val

				var mathres := -num1

				new IntSymbol, ret
				ret -> create (o)
				var cheated := cheat (IntSymbol.ptr, ret)
				cheated -> SetVal (mathres)
		  elsif tname = ">" or tname = "<" or tname = "<=" or tname = ">=" then
				var sym1 := RExpr (r -> getChild (1))
				VerifySymbol (sym1, "NumericSymbol")
				var num1 := cheat (NumericSymbol.nptr, sym1)

				var sym2 := RExpr (r -> getChild (2))
				VerifySymbol (sym2, "IntSymbol")
				var num2 := cheat (NumericSymbol.nptr, sym2)

				var mathres : boolean := false
				if tname = ">" then
					 ret := num1 -> Greater (num2)
				elsif tname = "<" then
					 ret := num1 -> Less (num2)
				elsif tname = "<=" then
					 ret := num1 -> LessEq (num2)
				elsif tname = ">=" then
					 ret := num1 -> GreaterEq (num2)
				end if
		  elsif tname = "or" or tname = "and" or tname = "xor" then
				var sym1 := RExpr (r -> getChild (1))
				VerifySymbol (sym1, "BoolSymbol")
				var bool1 := cheat (BoolSymbol.ptr, sym1) -> val

				var sym2 := RExpr (r -> getChild (2))
				VerifySymbol (sym2, "BoolSymbol")
				var bool2 := cheat (BoolSymbol.ptr, sym2) -> val

				var mathres : boolean := false
				if tname = "or" then
					 mathres := bool1 or bool2
				elsif tname = "and" then
					 mathres := bool1 and bool2
				elsif tname = "xor" then
					 mathres := bool1 xor bool2
				end if

				new BoolSymbol, ret
				ret -> create (o)
				var cheated := cheat (BoolSymbol.ptr, ret)
				cheated -> SetVal (mathres)
		  elsif tname = "=" or tname = "~=" or tname = "!=" then
				var sym1 := RExpr (r -> getChild (1))
				var sym2 := RExpr (r -> getChild (2))
				var res : boolean := sym1 -> Equals (sym2)

				if tname ~= "=" then                     % not equal to
					 res := not res
				end if

				new BoolSymbol, ret
				ret -> create (o)
				var cheated := cheat (BoolSymbol.ptr, ret)
				cheated -> SetVal (res)
		  elsif tname = "+" or tname = "*" or tname = "mod" or tname = "div" or tname = "-" then
				var sym1 := RExpr (r -> getChild (1))
				var sym2 := RExpr (r -> getChild (2))

				if tname = "-" then
					 ret := sym1 -> Subtract (sym2)
				elsif tname = "+" then
					 ret := sym1 -> Add (sym2)
				elsif tname = "*" then
					 ret := sym1 -> Mult (sym2)
				elsif tname = "mod" then
					 ret := sym1 -> Modulo (sym2)
				elsif tname = "div" then
					 ret := sym1 -> Div (sym2)
				end if
		  elsif tname = "." then
				var firstToken := r -> getChild (1) -> token -> text
				var givenScope := currentScope -> resolve (firstToken)
				var miniScope : ^ScopeSymbol
				if givenScope = nil then
					 var dashname : string := (r -> getChild (1) -> token -> text) + "_" + (r -> getChild (2) -> token -> text)
					 ret := currentScope -> resolve (dashname)
				else
					 VerifySymbol (givenScope, "ScopeSymbol")
					 miniScope := cheat (ScopeSymbol.ptr, givenScope)
					 ret := miniScope -> Resolve (r -> getChild (2) -> token -> text)
				end if
		  elsif tname = "true" or tname = "false" then
				var res := (tname = "true")
				new BoolSymbol, ret
				ret -> create (o)
				var cheated := cheat (BoolSymbol.ptr, ret)
				cheated -> SetVal (res)
		  elsif tname = "RANGE" then
				var sym1 := RExpr (r -> getChild (1))
				VerifySymbol (sym1, "IntSymbol")
				var num1 := cheat (IntSymbol.ptr, sym1) -> val

				var sym2 := RExpr (r -> getChild (2))
				VerifySymbol (sym2, "IntSymbol")
				var num2 := cheat (IntSymbol.ptr, sym2) -> val

				new RangeSymbol, ret
				ret -> create (o)
				var cheated := cheat (RangeSymbol.ptr, ret)
				cheated -> SetVal (num2, num1)
		  else
				%o -> de("trying to resolve variable " + r->token->text)
				ret := currentScope -> resolve (r -> token -> text)
				if ret = nil then
					 o -> er ("could not resolve variable " + r -> token -> text)
				end if
		  end if
		  if ret = nil then
				o -> er ("An expression failed to give a result! Maybe a broken variable?")
		  end if
		  result ret
	 end RExpr
end TuringInterpreter

