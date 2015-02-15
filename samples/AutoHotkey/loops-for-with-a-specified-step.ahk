SetBatchLines, -1
iterations := 5
step := 10
iterations *= step
Loop,  % iterations
{
   If Mod(A_Index, step)
      Continue
   MsgBox, % A_Index
}
ExitApp
