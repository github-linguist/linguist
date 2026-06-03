<% Option Explicit %>

<!-- #include file="../Lib/ASPUnit.asp" -->

<%
	Dim objLifecycle
	Set objLifecycle = ASPUnit.CreateLifeCycle("Setup", "Teardown")

	Call ASPUnit.AddModule( _
		ASPUnit.CreateModule( _
			"ASPUnitRunner AddPage Tests", _
			Array( _
				ASPUnit.CreateTest("ASPUnitRunnerAddPage"), _
				ASPUnit.CreateTest("ASPUnitRunnerAddPages") _
			), _
			objLifecycle _
		) _
	)

	Call ASPUnit.AddModule( _
		ASPUnit.CreateModule( _
			"ASPUnitRunner Run Tests", _
			Array( _
				ASPUnit.CreateTest("ASPUnitRunnerRunAddsCurrentPage"), _
				ASPUnit.CreateTest("ASPUnitRunnerPassesToRenderer") _
			), _
			objLifecycle _
		) _
	)

	Call ASPUnit.Run()

	' Create a global instance of ASPUnitRunner for testing

	Sub Setup()
		Call ExecuteGlobal("Dim objService")
		Set objService = New ASPUnitRunner
	End Sub

	Sub Teardown()
		Set objService = Nothing
	End Sub

	' Create a mock theme service that sets a global variable to indicate Render method executed

	Class ASPUnitUIRendererMockTheme
		Public Sub Render(objValue)
			Call ExecuteGlobal("Dim blnRendererRan")
			blnRendererRan = True
		End Sub
	End Class

	' Test that ASPUnitRunner adds specified pages

	Sub ASPUnitRunnerAddPage()
		Call objService.AddPage(Nothing)
		Call ASPUnit.Equal(objService.Pages.Count, 1, "AddPage method should add page to collection")
	End Sub

	Sub ASPUnitRunnerAddPages()
		Call objService.AddPages(Array(Nothing, Nothing))
		Call ASPUnit.Equal(objService.Pages.Count, 2, "AddPages method should add pages to collection")
	End Sub

	' Test that ASPUnitRunner adds current page if no pages are specified

	Sub ASPUnitRunnerRunAddsCurrentPage()
		Set objService.Theme = New ASPUnitUIRendererMockTheme
		Call objService.Run()
		Call ASPUnit.Equal(objService.Pages.Count, 1, "Run method should add current page if no pages are specified")
	End Sub

	' Test that ASPUnitRunner implments renderer on Run

	Sub ASPUnitRunnerPassesToRenderer()
		Set objService.Theme = New ASPUnitUIRendererMockTheme
		Call objService.Run()
		Call ASPUnit.Equal(blnRendererRan, True, "Run method should execute renderer render method")
	End Sub
%>