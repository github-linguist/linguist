declare
  %% simplest case: just evaluate expressions without bindings
  R1 = {Compiler.virtualStringToValue "{Abs ~42}"}
  {Show R1}

  %% eval expressions with additional bindings and
  %% the possibility to kill the evaluation by calling KillProc
  KillProc
  R2 = {Compiler.evalExpression "{Abs A}" unit('A':~42) ?KillProc}
  {Show R2}

  %% full control: add and remove bindings, eval expressions or
  %% statements, set compiler switches etc.
  Engine = {New Compiler.engine init}
  {Engine enqueue(setSwitch(expression false))} %% statements instead of expr.
  {Engine enqueue(mergeEnv(env('A':42 'System':System)))}
  {Engine enqueue(feedVirtualString("{System.show A}"))}
