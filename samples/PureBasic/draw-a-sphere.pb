; Original by Comtois @ 28/03/06
;
; Updated/Formated by Fluid Byte @ March.24,2009
;
; http://www.purebasic.fr/english/viewtopic.php?p=281258#p281258

Declare CreateSphere(M,P)
Declare UpdateMesh()

#_SIZEVERT = 36
#_SIZETRIS = 6
#FULLSCREEN = 0

Structure VECTOR
  X.f
  Y.f
  Z.f
EndStructure

Structure VERTEX
  X.f
  Y.f
  Z.f
  NX.f
  NY.f
  NZ.f
  Color.l
  U.f
  V.f
EndStructure

Structure TRIANGLE
  V1.w
  V2.w
  V3.w
EndStructure

Macro CALC_NORMALS
  *PtrV\NX = *PtrV\X
  *PtrV\NY = *PtrV\Y
  *PtrV\NZ = *PtrV\Z
EndMacro

Global *VBuffer, *IBuffer
Global Meridian = 50, Parallele = 50, PasLength = 4, Length

Define EventID, i, NbSommet, CameraMode, Angle.f, Pas.f = 0.5

InitEngine3D() : InitSprite() : InitKeyboard()

Add3DArchive(GetTemporaryDirectory(),#PB_3DArchive_FileSystem)
Add3DArchive(#PB_Compiler_Home + "Examples\Sources\Data\",#PB_3DArchive_FileSystem)

If #FULLSCREEN
  OpenScreen(800,600,32,"Sphere 3D")
Else
  OpenWindow(0,0,0,800,600,"Sphere 3D",#PB_Window_SystemMenu | 1)
  OpenWindowedScreen(WindowID(0),0,0,800,600,0,0,0)
EndIf

;-Texture
CreateImage(0,128,128)
StartDrawing(ImageOutput(0))
For i = 0 To 127 Step 4
  Box(0,i,ImageWidth(0),2,RGB(255,255,255))
  Box(0,i + 2,ImageWidth(0),2,RGB(0,0,155))
Next i
StopDrawing()
SaveImage(0,GetTemporaryDirectory() + "temp.bmp") : FreeImage(0)

;-Material
CreateMaterial(0,LoadTexture(0,"temp.bmp"))
RotateMaterial(0,0.1,#PB_Material_Animated)

;-Mesh
CreateSphere(Meridian,Parallele)

;-Entity
CreateEntity(0,MeshID(0),MaterialID(0))
ScaleEntity(0,60,60,60)

;-Camera
CreateCamera(0,0,0,100,100)
MoveCamera(0,0,0,-200)
CameraLookAt(0,EntityX(0),EntityY(0),EntityZ(0))

;-Light
AmbientColor(RGB(105, 105, 105))
CreateLight(0, RGB(255, 255,  55), EntityX(0) + 150, EntityY(0)      , EntityZ(0))
CreateLight(1, RGB( 55, 255, 255), EntityX(0) - 150, EntityY(0)      , EntityZ(0))
CreateLight(2, RGB( 55,  55, 255), EntityX(0)      , EntityY(0) + 150, EntityZ(0))
CreateLight(3, RGB(255,  55, 255), EntityX(0)      , EntityY(0) - 150, EntityZ(0))

; ----------------------------------------------------------------------------------------------------
; MAINLOOP
; ----------------------------------------------------------------------------------------------------

Repeat
  If #FULLSCREEN = 0
    Repeat
      EventID = WindowEvent()

      Select EventID
        Case #PB_Event_CloseWindow : End
      EndSelect
    Until EventID = 0
  EndIf

  Angle + Pas
  RotateEntity(0, Angle, Angle,Angle)

  If PasLength > 0 : UpdateMesh() : EndIf

  If ExamineKeyboard()
    If KeyboardReleased(#PB_Key_F1)
      CameraMode = 1 - CameraMode
      CameraRenderMode(0, CameraMode)
    EndIf
  EndIf

  RenderWorld()
  FlipBuffers()
Until KeyboardPushed(#PB_Key_Escape)

; ----------------------------------------------------------------------------------------------------
; FUNCTIONS
; ----------------------------------------------------------------------------------------------------

Procedure CreateSphere(M,P)
  ; M = Meridian
  ; P = Parallele
  ; The radius is 1. Front to remove it later, it's just for the demo.

  If M < 3 Or P < 2  : ProcedureReturn 0 : EndIf

  Protected Normale.VECTOR, NbSommet, i, j, Theta.f, cTheta.f, sTheta.f
  Protected Alpha.f, cAlpha.f, sAlpha.f, *PtrV.VERTEX, *PtrF.TRIANGLE, NbTriangle

  NbSommet = 2 + ((M + 1) * P)
  *VBuffer = AllocateMemory(#_SIZEVERT * Nbsommet)

  For i = 0 To M
    Theta  = i * #PI * 2.0 / M
    cTheta = Cos(theta)
    sTheta = Sin(theta)

    For j = 1 To P
      Alpha  = j * #PI / (P + 1)
      cAlpha = Cos(Alpha)
      sAlpha = Sin(Alpha)
      *PtrV = *VBuffer + #_SIZEVERT * ((i * P) + (j - 1))
      *PtrV\X = sAlpha * cTheta
      *PtrV\Y = sAlpha * sTheta
      *PtrV\Z = cAlpha
      *PtrV\U  = Theta / (2.0 * #PI)
      *PtrV\V  = Alpha / #PI
      CALC_NORMALS
    Next j
  Next i

  ; Southpole
  *PtrV = *VBuffer + #_SIZEVERT * ((M + 1) * P)
  *PtrV\X =  0
  *PtrV\Y =  0
  *PtrV\Z = -1
  *PtrV\U =  0
  *PtrV\V =  0
  CALC_NORMALS

  ; Northpole
  *PtrV + #_SIZEVERT
  *PtrV\X = 0
  *PtrV\Y = 0
  *PtrV\Z = 1
  *PtrV\U = 0
  *PtrV\V = 0
  CALC_NORMALS

  ; Les facettes
  NbTriangle = 4 * M * P
  *IBuffer = AllocateMemory(#_SIZETRIS * NbTriangle)
  *PtrF = *IBuffer

  For i = 0 To M - 1
    For j = 1 To P - 1
      *PtrF\V1 = ((i + 1) * P) + j
      *PtrF\V2 = ((i + 1) * P) + (j - 1)
      *PtrF\V3 = (i * P) + (j - 1)
      *PtrF + #_SIZETRIS
      *PtrF\V3 = ((i + 1) * P) + j        ;Recto
      *PtrF\V2 = ((i + 1) * P) + (j - 1)  ;Recto
      *PtrF\V1 = (i * P) + (j - 1)        ;Recto
      *PtrF + #_SIZETRIS
      *PtrF\V1 = i * P + j
      *PtrF\V2 = ((i + 1) * P) + j
      *PtrF\V3 = (i * P) + (j - 1)
      *PtrF + #_SIZETRIS
      *PtrF\V3 = i * P + j               ;Recto
      *PtrF\V2 = ((i + 1) * P) + j       ;Recto
      *PtrF\V1 = (i * P) + (j - 1)       ;Recto
      *PtrF + #_SIZETRIS
    Next j
  Next i

  ; The Poles
  For i = 0 To M - 1
    *PtrF\V3 = (M + 1) * P + 1
    *PtrF\V2 = (i + 1) * P
    *PtrF\V1 = i * P
    *PtrF + #_SIZETRIS
    *PtrF\V1 = (M + 1) * P + 1   ;Recto
    *PtrF\V2 = (i + 1) * P       ;Recto
    *PtrF\V3 = i * P             ;Recto
    *PtrF + #_SIZETRIS
  Next i

  For i = 0 To M - 1
    *PtrF\V3 = (M + 1) * P
    *PtrF\V2 = i * P + (P - 1)
    *PtrF\V1 = (i + 1) * P + (P - 1)
    *PtrF + #_SIZETRIS
    *PtrF\V1 = (M + 1) * P              ;Recto
    *PtrF\V2 = i * P + (P - 1)          ;Recto
    *PtrF\V3 = (i + 1) * P + (P - 1)    ;Recto
    *PtrF + #_SIZETRIS
  Next i

  If CreateMesh(0,100)
    Protected Flag = #PB_Mesh_Vertex | #PB_Mesh_Normal | #PB_Mesh_UVCoordinate | #PB_Mesh_Color
    SetMeshData(0,Flag,*VBuffer,NbSommet)
    SetMeshData(0,#PB_Mesh_Face,*IBuffer,NbTriangle)
    ProcedureReturn 1
  EndIf

  ProcedureReturn 0
EndProcedure

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Procedure UpdateMesh()
  Protected NbTriangle = 4 * Meridian * Parallele

  Length + PasLength

  If Length >= NbTriangle
    PasLength = 0
    Length = Nbtriangle
  EndIf

  SetMeshData(0,#PB_Mesh_Face,*IBuffer,Length)
EndProcedure
