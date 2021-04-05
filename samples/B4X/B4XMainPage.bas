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

'Ctrl + click to export as zip: ide://run?File=%B4X%\Zipper.jar&Args=Project.zip

Sub Class_Globals
	Private Root As B4XView
	Private xui As XUI
	Private BBCodeView1 As BBCodeView
	Private TextEngine As BCTextEngine
	Private xui As XUI
End Sub

Public Sub Initialize
	
End Sub

'B4i - don't miss the inline OBJC code in the main module.
Private Sub B4XPage_Created (Root1 As B4XView)
	Root = Root1
	Root.LoadLayout("1")
	TextEngine.Initialize(Root)
	BBCodeView1.TextEngine = TextEngine
	Dim btn As Button
	#if B4i
	btn.Initialize("btn", btn.STYLE_SYSTEM)
	#Else If B4J or B4A
	btn.Initialize("btn")
	#End If
	Dim xbtn As B4XView = btn
	If xui.IsB4i Then xbtn.SetColorAndBorder(xui.Color_Transparent, 1dip, xui.Color_Black, 2dip)
	xbtn.Text = "Click!"
	xbtn.SetLayoutAnimated(0, 0, 0, 100dip, 40dip)
	Dim pnl As B4XView = xui.CreatePanel("")
	pnl.SetLayoutAnimated(0, 0, 0, 200dip, 120dip)
	BBCodeView1.Views.Put("btn", btn)
	BBCodeView1.Text = _
$"[Alignment=Center][TextSize=20][b][u]This is the title [FontAwesome=0xF034/] [FontAwesome=0xF035/][/u][/b][/TextSize][/Alignment]
Emoji made of multiple "characters": 👨‍💻 and a flag: [TextSize=30][e=🇺🇳/][/TextSize]
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc fermentum elit vitae nisi faucibus, vitae lacinia turpis mattis.
Lets add a [Color=#ff0000][b]button:[/b][/color] [View=btn Vertical=10/]  and more text here...
We can also add links, for example: [url]https://www.google.com[/url]. You can click on the link. The title will be updated.
Here is a nice logo: [img FileName="logo.png" width=50/]
We can also download an image here: [url="DonManfred's avatar"][img url="https://b4x-4c17.kxcdn.com/android/forum/data/avatars/l/42/42649.jpg?1432374732" width=60 height=60/][/url]
Note that you must explicitly set the dimensions of such images.
Colors:
[list]
[*][Color=#ff0000][b][u]Red[/u][/b][/Color]
[*][Color=#00ff00][b][u color=Magenta]Green[/u][/b][/Color]
[*][Color=#0000ff][b][u color=DarkGray Thickness=3]Blue[/u][/b][/Color]
[/list]
Items:
[list style=ordered]
[*]Item A
[*]Item B
[*]Item C
[/list]
${TableRow("[b]Column 1[/b]", "[b]Column 2[/b]", "[b]Column 3[/b]")}
${TableRow($"[img FileName="logo.png" width=30/]000"$, "100", "200")}
${TableRow($"[color=#ff0000][b]-232[/b][/color]"$, "AAA", "CCC")}
${TableRow($"[color=#ff0000][b]-232[/b][/color]"$, "AAA", "CCC")}
${TableRow($"[color=#ff0000][b]-232[/b][/color]"$, "AAA", "CCC")}
${TableRow($"[color=#ff0000][b]-232[/b][/color]"$, "AAA", "CCC")}

"$
End Sub

Sub TableRow(Field1 As String, Field2 As String, Field3 As String) As String
	Return $"[Span MinWidth=33%x Alignment=center]${Field1}[/Span][Span MinWidth=33%x Alignment=center]${Field2}[/Span][Span MinWidth=33%x Alignment=center]${Field3}[/Span]"$
End Sub

Sub btn_Click
	Log("click")
End Sub

Sub BBCodeView1_LinkClicked (URL As String)
	B4XPages.SetTitle(Me, URL)
End Sub

'************ B4i - adds a cancel action to touch events:
#if OBJC
@end
@interface B4IPanelView  (touchcancelled)
@end
@implementation B4IPanelView  (touchcancelled)

- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event {
        B4I* bi = [self valueForKey:@"bi"];
        NSString* eventName = [self valueForKey:@"eventName"];
        for (UITouch *u in touches) {
            CGPoint p = [u locationInView:self];
            [bi raiseEvent:self event:eventName params:@[@4, @(p.x), @(p.y)]];
        }
}
#End If
'**************************

