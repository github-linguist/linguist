#ifdef __FB_WIN32__
	'#Compile -exx "Form1.rc"
#else
	'#Compile -exx
#endif
'#Region "Form"
	#include once "mff/Form.bi"
	#include once "mff/CommandButton.bi"
	
	Using My.Sys.Forms
	
	Type Form1 Extends Form
		Declare Constructor
		
		Dim As CommandButton CommandButton1, CommandButton2, CommandButton3
	End Type
	
	Constructor Form1
		' Form1
		With This
			.Name = "Form1"
			.Text = "Form1"
			.SetBounds 0, 0, 350, 300
		End With
		' CommandButton1
		With CommandButton1
			.Name = "CommandButton1"
			.Text = "CommandButton1"
			.SetBounds 40, 40, 210, 40
			.Parent = @This
		End With
		' CommandButton2
		With CommandButton2
			.Name = "CommandButton2"
			.Text = "CommandButton2"
			.SetBounds 40, 100, 220, 40
			.Parent = @This
		End With
		' CommandButton3
		With CommandButton3
			.Name = "CommandButton3"
			.Text = "CommandButton3"
			.SetBounds 20, 130, 260, 40
			.Parent = @This
		End With
	End Constructor
	
	Dim Shared fForm1 As Form1
	
	#ifndef _NOT_AUTORUN_FORMS_
		fForm1.Show
		
		App.Run
	#endif
'#End Region
