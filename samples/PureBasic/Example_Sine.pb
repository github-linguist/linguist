EnableExplicit

; ##################################################### Includes ####################################################

XIncludeFile "Includes/AudioOut.pbi"

; ##################################################### Prototypes ##################################################

; ##################################################### Structures ##################################################

; ##################################################### Constants ###################################################

#Samplerate = 44100

; ##################################################### Structures ##################################################

Structure Main
  *AudioOut
  
  Quit.i
EndStructure
Global Main.Main

Structure Main_Window
  ID.i
  
  TrackBar.i [10]
EndStructure
Global Main_Window.Main_Window

; ##################################################### Variables ###################################################

Global Frequency.d = 1000
Global Amplitude.d = 0.25

; ##################################################### Procedures ##################################################

Procedure Main_Window_Open()
  Main_Window\ID = OpenWindow(#PB_Any, 0, 0, 800, 100, "AudioOut Example", #PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_ScreenCentered)
  
  If Main_Window\ID
    
    Main_Window\TrackBar[0] = TrackBarGadget(#PB_Any, 10, 10, 780, 30, 0, 20000)
    SetGadgetState(Main_Window\TrackBar[0], Frequency)
    
    Main_Window\TrackBar[1] = TrackBarGadget(#PB_Any, 10, 40, 780, 30, 0, 1000)
    SetGadgetState(Main_Window\TrackBar[1], Amplitude*1000)
    
  EndIf
EndProcedure

Procedure Notifier_CallBack(*AudioOut)
  Protected *Temp, Temp_Size.i
  Static Rotation.d
  
  While AudioOut::GetQueuedBlocks(*AudioOut) <= 3
    
    Temp_Size = AudioOut::GetBufferBlocksize(*AudioOut)
    If Temp_Size > 0
      *Temp = AllocateMemory(Temp_Size)
      
      Define Left.d, Right.d, i
      For i = 0 To Temp_Size / 4 - 1
        Left = Sin(Rotation) * Amplitude
        Right = Sin(Rotation) * Amplitude
        
        PokeW(*Temp + i*4    , Left*32767)
        PokeW(*Temp + i*4 + 2, Right*32767)
        
        Rotation + 2.0*#PI / #Samplerate * Frequency
      Next
      
      AudioOut::Write_Data(Main\AudioOut, *Temp, Temp_Size)
      
      FreeMemory(*Temp)
    EndIf
    
  Wend
EndProcedure

; ##################################################### Initialisation ##############################################

Main_Window_Open()

AudioOut::GetDevices()

ForEach AudioOut::Device()
  Debug PeekS(AudioOut::@Device()\szPname)
Next

Main\AudioOut = AudioOut::Initialize(#WAVE_MAPPER, #Samplerate, 2, 16, @Notifier_CallBack())

If Not Main\AudioOut
  Debug AudioOut::GetError()
  End
EndIf

Notifier_CallBack(Main\AudioOut)

; ##################################################### Main ########################################################

Repeat
  
  Repeat
    Select WaitWindowEvent(100)
      Case #PB_Event_Gadget
        Select EventGadget()
          Case Main_Window\TrackBar[0]
            Frequency = GetGadgetState(Main_Window\TrackBar[0])
            Debug Frequency
            
          Case Main_Window\TrackBar[1]
            Amplitude = GetGadgetState(Main_Window\TrackBar[1]) / 1000
            
        EndSelect
        
      Case #PB_Event_CloseWindow
        Main\Quit = #True
        
      Case 0
        Break
    EndSelect
  ForEver
  
Until Main\Quit

; ##################################################### End #########################################################

AudioOut::Deinitialize(Main\AudioOut)

; IDE Options = PureBasic 5.30 Beta 2 (Windows - x64)
; CursorPosition = 109
; FirstLine = 79
; Folding = -
; EnableUnicode
; EnableThread
; EnableXP
