B4A=true
Group=Default Group
ModulesStructureVersion=1
Type=Class
Version=9.85
@EndOfDesignText@
#Region Shared Files
#CustomBuildAction: folders ready, %WINDIR%\System32\Robocopy.exe,"..\..\Shared Files" "..\Files"
'Ctrl + click to sync files: ide://run?file=%WINDIR%\System32\Robocopy.exe&args=..\..\Shared+Files&args=..\Files&FilesSync=True
#End Region

'Ctrl + click to export as zip: ide://run?File=%B4X%\Zipper.jar&Args=Project.zip&VMArgs=-DZeroSharedFiles%3DTrue

Sub Class_Globals
	Private Root As B4XView
	Private xui As XUI
	Public mGame As Game
End Sub

Public Sub Initialize
'	B4XPages.GetManager.LogEvents = True
End Sub

'This event will be called once, before the page becomes visible.
Private Sub B4XPage_Created (Root1 As B4XView)
 	#if B4A or B4J
    Root = Root1
    #else if B4i
	'handle iPhone safe area
	Root = xui.CreatePanel("")
	Root1.Color = xui.Color_Black
	Root1.AddView(Root, 0, 0, Root1.Width, Root1.Height)
    #end if
	If Root.Width = 0 Or Root.Height = 0 Then
		Wait For  B4XPage_Resize(Width As Int, Height As Int)
	End If
	#if B4i
	Dim r As Rect = B4XPages.GetNativeParent(Me).SafeAreaInsets
	Root.SetLayoutAnimated(0, r.Left, r.Top, Width - r.Right - r.Left, Height - r.Bottom - r.Top)
    #end if
	mGame.Initialize(Root)
	mGame.Start
End Sub

Private Sub B4XPage_Resize (Width As Int, Height As Int)
	mGame.Resize
End Sub

Private Sub B4XPage_Appear

End Sub

Private Sub B4XPage_Disappear
	If mGame.IsInitialized Then
		mGame.Pause
	End If
End Sub