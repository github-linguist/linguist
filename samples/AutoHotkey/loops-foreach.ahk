string = mary,had,a,little,lamb
Loop, Parse, string, `,
  MsgBox %A_LoopField%
