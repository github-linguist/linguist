CompilerIf #PB_Compiler_Version<441
  CompilerError "You failed the version check!"
CompilerEndIf

CompilerIf   Defined(bloop,#PB_Variable)
  CompilerIf Defined(Abs(),#PB_Function)
    Abs(bloop)
  CompilerEndIf
CompilerEndIf
