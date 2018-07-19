XIncludeFile "OpenGL.pbi"
pfd.PIXELFORMATDESCRIPTOR
FlatMode = 0 ; Enable Or disable the 'Flat' rendering
WindowWidth = 800 ; The window & GLViewport dimensions
WindowHeight = 600
hWnd = OpenWindow(0, 0, 0, WindowWidth, WindowHeight, "OpenGL Triangle", #PB_Window_SystemMenu)
hdc = GetDC_(hWnd)
pfd\nSize = SizeOf(PIXELFORMATDESCRIPTOR)
pfd\nVersion = 1
pfd\dwFlags = #PFD_SUPPORT_OPENGL | #PFD_DOUBLEBUFFER | #PFD_DRAW_TO_WINDOW
pfd\dwLayerMask = #PFD_MAIN_PLANE
pfd\iPixelType = #PFD_TYPE_RGBA
pfd\cColorBits = 24
pfd\cDepthBits = 16
pixformat = ChoosePixelFormat_(hdc, pfd)
SetPixelFormat_(hdc, pixformat, pfd)
hrc = wglCreateContext_(hdc)
wglMakeCurrent_(hdc,hrc)
glViewport_ (0, 0, WindowWidth-30, WindowHeight-30)
glPushMatrix_()
glMatrixMode_(#GL_MODELVIEW)
glBegin_(#GL_TRIANGLES );
glColor3f_(1.0, 0.0, 0.0 )
glVertex2f_( 0.0, 1.0 )
glColor3f_( 0.0, 1.0, 0.0 )
glVertex2f_( 0.87, -0.5 );
glColor3f_( 0.0, 0.0, 1.0 )
glVertex2f_( -0.87, -0.5 );
glEnd_()
glPopMatrix_()
glFinish_()
SwapBuffers_(hdc)
While Quit = 0
  Repeat
    EventID = WindowEvent()
    Select EventID
    Case #PB_Event_CloseWindow
      Quit = 1
    EndSelect
  Until EventID = 0
Wend
