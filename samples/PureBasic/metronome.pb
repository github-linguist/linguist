Structure METRONOMEs
string_mil.i
string_err.i
string_bmp.i
volumn.i
image_metronome.i
EndStructure

Enumeration
#STRING_MIL
#STRING_ERR
#STRING_BMP
#BUTTON_ERRM
#BUTTON_ERRP
#BUTTON_VOLM
#BUTTON_VOLP
#TEXT_TUNE
#TEXT_MIL
#TEXT_BMP
#BUTTON_START
#BUTTON_STOP
#WINDOW
#IMAGE_METRONOME
EndEnumeration


If Not InitSound()
MessageRequester("Error", "Sound system is not available",  0)
End
EndIf

; the wav file saved as raw data
; sClick: [?sClick < address of label sClick:]
; Data.a $52,$49,$46,... [?eClick-?sClick the # of bytes]
; eClick: [?eClick < address of label eClick:]
If Not CatchSound(0,?sClick,?eClick-?sClick)
MessageRequester("Error", "Could not CatchSound",  0)
End
EndIf

If LoadFont(0,"tahoma",9,#PB_Font_HighQuality|#PB_Font_Bold)
SetGadgetFont(#PB_Default, FontID(0))
EndIf

Procedure.i Metronome(*m.METRONOMEs)
Protected j
Protected iw            =360
Protected ih            =360
Protected radius        =100
Protected originX       =iw/2
Protected originY       =ih/2
Protected BeatsPerMinute=*m\string_bmp.i
Protected msError.i     =*m\string_err.i
Protected Milliseconds  =int((60*1000)/BeatsPerMinute)
Protected msDword.i     =Milliseconds*2/12

If CreateImage(*m\image_metronome.i,iw,ih)

Repeat

; [sAngleA: < prepare to read starting at address of label]
Restore sAngleA

st1=ElapsedMilliseconds()
; 1          |  90
  ; 2   6    |    75  75
    ; 3   5  |      60  60
      ; 4    |        45
; 90, 75, 60, 45, 60, 75 ; click
For j=1 to 6
st2=ElapsedMilliseconds()
Read.f Angle.f
If StartDrawing(ImageOutput(*m\image_metronome.i))
  StartTime = ElapsedMilliseconds()
  Box(0,0,iw,ih,RGB(0,0,0))
  CircleX=int(radius*cos(Radian(Angle)))
  CircleY=int(radius*sin(Radian(Angle)))
  LineXY(originX,originY, originX+CircleX,originY-CircleY,RGB(255,255,0))
  Circle(originX+CircleX,originY-CircleY,10,RGB(255,0,0))
  StopDrawing() ; don't forget to: StopDrawing() !
  SetGadgetState(*m\image_metronome.i,ImageID(*m\image_metronome.i))
  Endif
Delay(msDword-msError)
Next
While ElapsedMilliseconds()-st1<Milliseconds-msError.i
Wend

SetGadgetText(*m\string_mil.i,str(ElapsedMilliseconds()-st1))
PlaySound(0) ; click

st3=ElapsedMilliseconds():err2=0
      ; 1    |           090
    ; 2   6  |        105   105
  ; 3   5    |     120   120
; 4          |  135
 ; 90,105,120,135,120,105 ; click
For j=1 to 6
st4=ElapsedMilliseconds()
Read.f Angle.f
If StartDrawing(ImageOutput(*m\image_metronome.i))
  StartTime = ElapsedMilliseconds()
  Box(0,0,iw,ih,RGB(0,0,0))
  CircleX=int(radius*cos(Radian(Angle)))
  CircleY=int(radius*sin(Radian(Angle)))
  LineXY(originX,originY, originX+CircleX,originY-CircleY,RGB(255,255,0))
  Circle(originX+CircleX,originY-CircleY,10,RGB(255,0,0))
  StopDrawing() ; don't forget to: StopDrawing() !
  SetGadgetState(*m\image_metronome.i,ImageID(*m\image_metronome.i))
  Endif
Delay(msDword-msError)
Next
While ElapsedMilliseconds()-st3<Milliseconds-msError.i
Wend

SetGadgetText(*m\string_mil.i,str(ElapsedMilliseconds()-st3))
PlaySound(0) ; click

ForEver
Endif
ProcedureReturn i.i
EndProcedure

; width of image
; height of image
; allocate memory for our structure
; the StringGadget number for milliseconds between clicks
; the ImageGadget number for the metronome
; intial value for the volumn [30 %]
; intial value adjustment for time accuracy adjustment
w                     =360
h                     =360
*m.METRONOMEs         =Allocatememory(SizeOf(METRONOMEs))
*m\string_mil.i       =#STRING_MIL
*m\image_metronome.i  =#IMAGE_METRONOME
*m\volumn.i           =30
DelayVal              =5

SoundVolume(0,*m\volumn.i)

radius        =100
originX       =w/2
originY       =h/2
Angle.f       =90

; draw our initial metronome at 90 degrees
      ; *
      ; |
      ; |
      ; |
If CreateImage(#IMAGE_METRONOME,w,h)
If StartDrawing(ImageOutput(#IMAGE_METRONOME))
  Box(0,0,w,h,RGB(0,0,0))
  CircleX=int(radius*cos(Radian(Angle)))
  CircleY=int(radius*sin(Radian(Angle)))
  LineXY(originX,originY, originX+CircleX,originY-CircleY,RGB(255,255,0))
  Circle(originX+CircleX,originY-CircleY,10,RGB(255,0,0))
  StopDrawing() ; don't forget to: StopDrawing() it's time for dinner
  Endif
Endif

; window style
WINDOW_STYLE=#PB_Window_SystemMenu
WINDOW_STYLE|#PB_Window_Screencentered
WINDOW_STYLE|#PB_Window_MinimizeGadget

If Not OpenWindow(#WINDOW,0,0,w+200+12,h+4,"Metronome",WINDOW_STYLE)
MessageRequester("Error", "Not OpenWindow",  0)
End
EndIf
SetWindowColor(#Window,$505050)

TEXT_STYLE=#PB_Text_Center|#PB_Text_Border

; data strings
i=2:wp=18:gh=24
i+1:TextGadget(#TEXT_TUNE   ,w+wp-10 ,gh*(i-1),200,gh,"Fine tuning",TEXT_STYLE)
i+1
i+1:StringGadget(#STRING_ERR,w+wp+100,gh*(i-1),90,gh,"-5",#PB_String_ReadOnly)
i+1
i+1:StringGadget(#STRING_MIL,w+wp+100,gh*(i-1),90,gh,"0",#PB_String_ReadOnly)
i+1
i+1:StringGadget(#STRING_BMP,w+wp+100,gh*(i-1),90,gh,"120",#PB_String_Numeric)

; control buttons
i=2:wp=10:gh=24
i+1
i+1
i+1:ButtonGadget(#BUTTON_ERRM ,w+wp,gh*(i-1),50,gh,"-")
i+0:ButtonGadget(#BUTTON_ERRP ,w+wp+50,gh*(i-1),50,gh,"+")
i+1
i+1:TextGadget(#TEXT_MIL  ,w+wp,gh*(i-1),100,gh,"MilliSeconds ",TEXT_STYLE)
i+1
i+1:TextGadget(#TEXT_BMP  ,w+wp,gh*(i-1),100,gh,"BeatsPerMin " ,TEXT_STYLE)
i+1
i+1:ButtonGadget(#BUTTON_START,w+wp,gh*(i-1),200,gh,"Start",#PB_Button_Toggle)
i+1
i+1:ButtonGadget(#BUTTON_VOLM ,w+wp    ,gh*(i-1),100,gh,"-Volumn")
i+0:ButtonGadget(#BUTTON_VOLP ,w+wp+100,gh*(i-1),100,gh,"+Volumn")

; the metronome image
IMG_STYLE=#PB_Image_Border
ImageGadget(#IMAGE_METRONOME,0,0,360,360,ImageID(#IMAGE_METRONOME),IMG_STYLE)
SetGadgetState(#IMAGE_METRONOME,ImageID(#IMAGE_METRONOME))

Repeat

; the control loop for our application [not all the values are used
; but this makes it easy to add further controls and functionality]
msg=     WaitWindowEvent ()
wid=     EventWindow     ()
mid=     EventMenu       ()
gid=     EventGadget     ()
etp=     EventType       ()
ewp=     EventwParam     ()
elp=     EventlParam     ()  :If msg=#PB_Event_CloseWindow  : End : EndIf

; Esc kills application regardless of window focus
If GetAsyncKeyState_(#VK_ESCAPE) : End : EndIf

Select msg

case #PB_Event_Gadget

Select gid

  Case #BUTTON_VOLP ; +volumn
  If *m\volumn.i<100:*m\volumn.i+10
  SoundVolume(0,*m\volumn.i)
  EndIf

Case #BUTTON_VOLM   ; -volumn
  If *m\volumn.i>0  :*m\volumn.i-10
  SoundVolume(0,*m\volumn.i)
  EndIf

Case #BUTTON_ERRP ; time accuracy adjustment [faster]
  DelayVal-1
  *m\string_err.i       =DelayVal
  SetGadgetText(#STRING_ERR,str(0-DelayVal))
  If GetGadgetState(#BUTTON_START)=1
    GoSub Stop
    GoSub Start
    EndIf

Case #BUTTON_ERRM ; time accuracy adjustment [slower]
  DelayVal+1
  *m\string_err.i       =DelayVal
  SetGadgetText(#STRING_ERR,str(0-DelayVal))
  If GetGadgetState(#BUTTON_START)=1
    GoSub Stop
    GoSub Start
    EndIf

Case #BUTTON_START ; the toggle button for start/stop
  Select GetGadgetState(#BUTTON_START)
  Case 1 :GoSub Stop : GoSub Start   :SetGadgetText(#BUTTON_START,"Stop")
  Case 0 :GoSub Stop                 :SetGadgetText(#BUTTON_START,"Start")
  EndSelect

EndSelect
EndSelect
ForEver
End

Start:     ; start up the thread with new values
*m\string_err.i       =DelayVal
*m\string_bmp.i       =Val(GetGadgetText(#STRING_BMP))

If *m\string_bmp.i
  MetronomeThread=CreateThread(@Metronome(),*m)
  EndIf
Return

Stop:      ; if the thread is running: stop it
If IsThread(MetronomeThread)
  KillThread(MetronomeThread)
  EndIf
Return

DataSection ; an array of angles to be read by: MetronomeThread
sAngleA:
Data.f  90, 75, 60, 45, 60, 75 ; click
Data.f  90,105,120,135,120,105 ; click
eAngleA:
EndDataSection

DataSection ; a small wav file saved as raw data
sClick:
Data.a $52,$49,$46,$46,$2E,$08,$00,$00,$57,$41,$56,$45,$66,$6D,$74,$20
Data.a $10,$00,$00,$00,$01,$00,$01,$00,$44,$AC,$00,$00,$44,$AC,$00,$00
Data.a $01,$00,$08,$00,$64,$61,$74,$61,$02,$06,$00,$00,$83,$84,$84,$84
Data.a $85,$85,$86,$86,$88,$89,$8A,$8B,$8E,$91,$95,$9C,$A2,$A9,$B3,$C0
Data.a $CF,$CF,$D0,$CE,$D3,$9B,$47,$31,$31,$33,$32,$32,$32,$32,$33,$32
Data.a $33,$31,$42,$A1,$C9,$C4,$AE,$BD,$D4,$CD,$D1,$CF,$D0,$CF,$CF,$D0
Data.a $CB,$A9,$70,$37,$33,$32,$32,$32,$32,$32,$33,$32,$33,$31,$34,$2E
Data.a $53,$AF,$CF,$CF,$CA,$CF,$D0,$CF,$D0,$CF,$D0,$CE,$D3,$AA,$83,$97
Data.a $A1,$8A,$44,$32,$33,$31,$33,$32,$33,$32,$32,$33,$32,$33,$30,$44
Data.a $83,$94,$7E,$7D,$AE,$CF,$D0,$CF,$D0,$CE,$D1,$BF,$B8,$C3,$B9,$B7
Data.a $98,$68,$47,$37,$30,$31,$33,$32,$33,$32,$33,$32,$3D,$49,$48,$3E
Data.a $38,$43,$5C,$77,$87,$91,$95,$85,$78,$79,$7A,$80,$8D,$8F,$8A,$89
Data.a $8D,$8D,$88,$81,$83,$89,$7F,$73,$77,$7A,$71,$64,$55,$43,$31,$31
Data.a $34,$32,$34,$3A,$41,$36,$2F,$33,$37,$4D,$5C,$69,$73,$78,$7B,$82
Data.a $8A,$8F,$90,$91,$91,$94,$9B,$9C,$94,$86,$75,$67,$5A,$51,$50,$4D
Data.a $45,$42,$41,$41,$43,$48,$51,$54,$59,$65,$75,$82,$87,$86,$83,$7B
Data.a $6E,$67,$65,$63,$61,$5F,$5D,$56,$4E,$4B,$50,$57,$5F,$67,$71,$78
Data.a $7B,$7C,$7E,$82,$83,$80,$7C,$79,$76,$71,$6D,$6C,$68,$5E,$52,$4D
Data.a $4A,$46,$43,$43,$47,$4B,$4B,$4D,$53,$59,$5D,$65,$6F,$79,$82,$8B
Data.a $92,$93,$90,$8B,$88,$83,$7C,$7B,$7B,$76,$6F,$66,$5E,$59,$53,$51
Data.a $53,$55,$57,$59,$5B,$5B,$5A,$5A,$5B,$5E,$61,$67,$6D,$6E,$6C,$69
Data.a $6B,$6F,$72,$73,$75,$77,$79,$78,$79,$7B,$7C,$79,$76,$73,$72,$71
Data.a $6C,$64,$59,$51,$50,$52,$53,$50,$4A,$41,$3B,$3C,$46,$53,$61,$6B
Data.a $6E,$6D,$70,$79,$83,$90,$9B,$A0,$9C,$94,$8C,$85,$7E,$7A,$76,$6F
Data.a $62,$56,$4E,$48,$45,$48,$4D,$4D,$4C,$4E,$54,$5B,$64,$6F,$79,$80
Data.a $83,$82,$82,$85,$88,$88,$87,$84,$7D,$73,$69,$63,$60,$5D,$5A,$55
Data.a $51,$4E,$4E,$52,$58,$5F,$66,$6A,$6D,$73,$7D,$86,$8B,$8F,$8E,$87
Data.a $81,$80,$7F,$79,$72,$6B,$60,$54,$4D,$4C,$4E,$4E,$4F,$50,$52,$58
Data.a $5F,$67,$6F,$75,$79,$7A,$7B,$7E,$82,$81,$7F,$7C,$75,$6F,$6C,$6B
Data.a $6C,$6E,$6F,$6C,$67,$65,$68,$6D,$73,$77,$77,$74,$70,$6C,$6B,$6E
Data.a $72,$73,$6F,$67,$60,$5D,$5E,$61,$63,$64,$63,$61,$60,$63,$69,$70
Data.a $76,$79,$79,$7A,$7C,$7F,$81,$81,$80,$7E,$79,$72,$6E,$6B,$67,$65
Data.a $62,$5F,$5D,$5D,$5E,$5F,$62,$65,$6A,$6E,$71,$75,$78,$7B,$7C,$7D
Data.a $7E,$7F,$7F,$7E,$7B,$77,$74,$6F,$6A,$67,$63,$5D,$56,$50,$49,$45
Data.a $43,$43,$46,$4B,$50,$55,$5A,$60,$69,$74,$81,$8E,$97,$9D,$9F,$9E
Data.a $9C,$9B,$9A,$98,$93,$8A,$7D,$6E,$61,$58,$50,$4C,$49,$46,$45,$44
Data.a $46,$4C,$53,$5C,$66,$6F,$75,$7B,$80,$85,$88,$88,$87,$85,$82,$7E
Data.a $7A,$75,$70,$6B,$66,$61,$5D,$5C,$5E,$61,$65,$69,$6B,$6B,$6B,$6A
Data.a $6B,$6F,$73,$76,$76,$71,$6B,$64,$61,$62,$67,$6D,$71,$72,$72,$72
Data.a $74,$7A,$81,$85,$88,$87,$85,$82,$80,$7D,$79,$72,$6A,$63,$5F,$5F
Data.a $60,$60,$5E,$5A,$58,$58,$5D,$64,$6D,$74,$77,$78,$77,$79,$7D,$82
Data.a $85,$86,$84,$80,$7C,$79,$78,$78,$75,$70,$6B,$66,$64,$66,$68,$6A
Data.a $6B,$68,$64,$63,$66,$6C,$72,$76,$78,$78,$78,$78,$78,$7B,$7E,$80
Data.a $7F,$7D,$7B,$79,$74,$6F,$6A,$65,$63,$61,$5F,$5D,$5C,$5A,$58,$59
Data.a $5E,$66,$6D,$74,$79,$7C,$80,$84,$89,$8B,$8C,$8B,$87,$81,$7C,$77
Data.a $72,$6B,$63,$5B,$55,$52,$53,$55,$57,$59,$59,$5C,$62,$6B,$77,$82
Data.a $8B,$91,$93,$94,$97,$9A,$9B,$98,$92,$8A,$81,$77,$72,$6E,$6A,$65
Data.a $5E,$58,$56,$56,$57,$57,$54,$53,$53,$57,$5E,$67,$6D,$6F,$6E,$6E
Data.a $74,$7F,$8D,$96,$98,$93,$8C,$89,$8B,$91,$97,$96,$8C,$7E,$71,$69
Data.a $67,$67,$65,$5D,$4F,$41,$39,$3B,$45,$51,$59,$5A,$56,$56,$5B,$69
Data.a $7B,$89,$90,$8F,$8A,$87,$86,$89,$8C,$8A,$84,$7B,$75,$75,$78,$7A
Data.a $76,$6D,$63,$5E,$60,$68,$71,$74,$6F,$63,$57,$52,$58,$65,$73,$7B
Data.a $7C,$77,$72,$72,$77,$81,$89,$8C,$88,$81,$7A,$76,$76,$76,$75,$70
Data.a $69,$62,$5F,$62,$68,$6D,$6D,$6A,$68,$68,$6C,$74,$7D,$84,$87,$86
Data.a $84,$84,$86,$89,$8A,$88,$85,$84,$83,$80,$7B,$74,$6B,$61,$5A,$58
Data.a $5A,$5C,$5C,$59,$55,$53,$55,$5B,$65,$72,$7D,$84,$86,$87,$89,$8C
Data.a $92,$98,$9A,$99,$96,$90,$8C,$88,$83,$7F,$79,$72,$6C,$66,$63,$61
Data.a $5E,$59,$52,$4C,$4B,$4E,$55,$5E,$63,$66,$67,$69,$71,$7C,$88,$91
Data.a $93,$91,$8C,$88,$88,$8A,$8E,$8E,$88,$7E,$73,$6A,$67,$66,$66,$65
Data.a $62,$5B,$55,$52,$53,$58,$5F,$65,$69,$6B,$6F,$75,$7C,$83,$88,$8B
Data.a $8B,$89,$8B,$8E,$8E,$8B,$85,$7E,$74,$6E,$6C,$6D,$6D,$6B,$67,$62
Data.a $5F,$60,$64,$67,$6A,$6B,$6C,$6C,$6F,$73,$75,$77,$78,$78,$79,$7C
Data.a $82,$89,$8D,$8D,$8C,$8C,$8C,$8E,$91,$90,$8C,$85,$7C,$74,$6D,$68
Data.a $65,$62,$5E,$5B,$58,$58,$5C,$62,$68,$6C,$70,$75,$7C,$83,$8A,$90
Data.a $96,$98,$98,$96,$93,$8E,$8A,$84,$7F,$7A,$75,$6E,$67,$60,$59,$54
Data.a $52,$53,$57,$5B,$5D,$5E,$60,$65,$6C,$74,$7C,$87,$90,$98,$9D,$A1
Data.a $A2,$A1,$9F,$9C,$9A,$97,$92,$89,$7E,$70,$64,$5C,$57,$55,$53,$52
Data.a $50,$4F,$51,$56,$5D,$66,$6D,$73,$79,$7E,$82,$86,$89,$8B,$8D,$8D
Data.a $8C,$89,$84,$7F,$7B,$79,$77,$77,$76,$73,$6E,$6A,$6A,$6F,$74,$78
Data.a $79,$78,$74,$72,$73,$76,$79,$7B,$7A,$75,$6F,$6A,$68,$69,$6B,$6C
Data.a $6D,$6C,$6C,$6D,$6F,$74,$78,$7C,$80,$82,$85,$88,$8B,$8C,$8B,$88
Data.a $84,$81,$7F,$7D,$7B,$79,$75,$6F,$6A,$67,$65,$65,$68,$6B,$6D,$6F
Data.a $70,$72,$74,$79,$7D,$81,$85,$87,$88,$89,$8A,$8C,$8D,$8D,$8B,$86
Data.a $82,$7F,$7D,$7C,$7A,$77,$73,$6F,$6A,$66,$65,$64,$65,$68,$6B,$6F
Data.a $72,$74,$75,$76,$79,$7D,$83,$8A,$8D,$8C,$88,$82,$7D,$7A,$78,$76
Data.a $73,$6F,$69,$64,$60,$60,$62,$65,$68,$6B,$6F,$75,$7C,$84,$8A,$8E
Data.a $90,$91,$92,$94,$94,$93,$90,$8D,$88,$80,$78,$6F,$68,$64,$62,$60
Data.a $5F,$5D,$5A,$58,$58,$5D,$68,$73,$7D,$83,$87,$89,$8C,$91,$96,$9B
Data.a $9D,$9B,$95,$8C,$83,$7B,$76,$71,$6D,$68,$63,$5D,$58,$56,$57,$5B
Data.a $62,$69,$6F,$75,$7B,$81,$86,$8C,$91,$95,$97,$97,$95,$93,$8F,$88
Data.a $80,$77,$6F,$69,$66,$63,$60,$5C,$58,$54,$53,$59,$62,$6D,$75,$7A
Data.a $7C,$7E,$83,$8D,$98,$A0,$A2,$9E,$97,$90,$8E,$8F,$8F,$8D,$85,$79
Data.a $6E,$66,$65,$68,$6D,$6E,$6B,$65,$60,$60,$65,$6C,$74,$79,$7A,$79
Data.a $78,$79,$7C,$7E,$7F,$7F,$7F,$7F,$80,$82,$84,$84,$83,$82,$81,$83
Data.a $88,$8D,$91,$92,$8E,$88,$82,$7F,$7F,$7F,$7F,$7C,$75,$6A,$60,$59
Data.a $56,$56,$58,$59,$5A,$5A,$5E,$65,$6E,$79,$82,$8A,$91,$97,$9F,$A5
Data.a $AA,$AB,$A6,$9C,$93,$8A,$84,$7E,$76,$6B,$5E,$51,$48,$45,$45,$48
Data.a $4B,$4E,$51,$57,$60,$6E,$7C,$8A,$94,$9B,$A0,$A4,$A8,$AB,$AC,$A9
Data.a $A3,$98,$8C,$7F,$73,$6A,$62,$5A,$54,$4E,$4A,$48,$49,$4D,$55,$60
Data.a $6B,$77,$81,$8A,$92,$9A,$A1,$A9,$AD,$AF,$AD,$A7,$9F,$96,$8C,$84
Data.a $7B,$71,$67,$5C,$52,$4C,$49,$4B,$4F,$54,$5A,$60,$67,$71,$7D,$8A
Data.a $96,$9F,$A4,$A6,$A6,$A5,$A4,$A2,$9C,$94,$89,$7F,$77,$70,$69,$62
Data.a $5A,$54,$4F,$50,$56,$5E,$65,$6B,$6E,$73,$79,$83,$8F,$98,$9F,$A1
Data.a $9F,$9C,$9A,$98,$96,$92,$8C,$84,$7A,$71,$6B,$67,$65,$64,$62,$61
Data.a $61,$62,$67,$6D,$73,$79,$7E,$80,$83,$85,$88,$8A,$8C,$8B,$89,$86
Data.a $84,$84,$84,$84,$82,$7E,$7A,$79,$7A,$7C,$7E,$7E,$7C,$7A,$77,$77
Data.a $79,$7C,$7E,$7D,$7B,$79,$79,$79,$7A,$7B,$7C,$7C,$7B,$7B,$7C,$7D
Data.a $7D,$7C,$7B,$7A,$7B,$7C,$7D,$7D,$7B,$79,$77,$76,$77,$78,$79,$79
Data.a $78,$76,$75,$76,$79,$7A,$7A,$78,$76,$75,$77,$7C,$81,$80,$53,$41
Data.a $55,$52,$00,$02,$00,$00,$31,$2C,$20,$30,$2C,$20,$36,$2C,$20,$30
eClick:
EndDataSection
