EnableExplicit
Define.i x, y ,Xmax ,Ymax ,N
Xmax = 13 : Ymax = 20
Dim     world.i(Xmax+1,Ymax+1)
Dim Nextworld.i(Xmax+1,Ymax+1)

; Glider test
;------------------------------------------
 world(1,1)=1 : world(1,2)=0 : world(1,3)=0
 world(2,1)=0 : world(2,2)=1 : world(2,3)=1
 world(3,1)=1 : world(3,2)=1 : world(3,3)=0
;------------------------------------------

OpenConsole()
EnableGraphicalConsole(1)
ClearConsole()
Print("Press any key to interrupt")
Repeat
  ConsoleLocate(0,2)
  PrintN(LSet("", Xmax+2, "-"))
 ;---------- endless world ---------
  For y = 1 To Ymax
    world(0,y)=world(Xmax,y)
    world(Xmax+1,y)=world(1,y)
  Next
  For x = 1 To Xmax
    world(x,0)=world(x,Ymax)
    world(x,Ymax+1)=world(x,1)
  Next
  world(0     ,0     )=world(Xmax,Ymax)
  world(Xmax+1,Ymax+1)=world(1   ,1   )
  world(Xmax+1,0     )=world(1   ,Ymax)
  world(     0,Ymax+1)=world(Xmax,1   )
 ;---------- endless world ---------
  For y = 1 To Ymax
    Print("|")
    For x = 1 To Xmax
      Print(Chr(32+world(x,y)*3))
      N = world(x-1,y-1)+world(x-1,y)+world(x-1,y+1)+world(x,y-1)
      N + world(x,y+1)+world(x+1,y-1)+world(x+1,y)+world(x+1,y+1)
      If (world(x,y) And (N = 2 Or N = 3))Or (world(x,y)=0 And N = 3)
        Nextworld(x,y)=1
      Else
        Nextworld(x,y)=0
      EndIf
    Next
    PrintN("|")
  Next
  PrintN(LSet("", Xmax+2, "-"))
  Delay(100)
  ;Swap world() , Nextworld()    ;PB  <4.50
  CopyArray(Nextworld(), world());PB =>4.50
  Dim Nextworld.i(Xmax+1,Ymax+1)
Until Inkey() <> ""

PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
