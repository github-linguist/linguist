hOpenGL32 := DllCall("LoadLibrary", "Str", "opengl32")
Gui, +LastFound +Resize
hDC := DllCall("GetDC", "uInt", WinExist())

VarSetCapacity(pfd, 40, 0)
NumPut(40, pfd, 0, "uShort")
NumPut(1, pfd, 2, "uShort")
NumPut(37, pfd, 4, "uInt")
NumPut(24, pfd, 9, "uChar")
NumPut(16, pfd, 23, "uChar")
DllCall("SetPixelFormat", "uInt", hDC, "uInt", DllCall("ChoosePixelFormat", "uInt", hDC, "uInt", &pfd), "uInt", &pfd)

hRC := DllCall("opengl32\wglCreateContext", "uInt", hDC)
DllCall("opengl32\wglMakeCurrent", "uInt", hDC, "uInt", hRC)
Gui, Show, w640 h480, Triangle
OnExit, ExitSub
SetTimer, Paint, 50
return


Paint:
DllCall("opengl32\glClearColor", "Float", 0.3, "Float", 0.3, "Float", 0.3, "Float", 0)
DllCall("opengl32\glClear", "uInt", 0x4100) ;GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT

DllCall("opengl32\glShadeModel", "uInt", 0x1D01) ;GL_SMOOTH

DllCall("opengl32\glLoadIdentity")
DllCall("opengl32\glTranslatef", "Float", -15, "Float", -15, "Float", 0)

DllCall("opengl32\glBegin", "uInt", 0x0004) ;GL_TRIANGLES
DllCall("opengl32\glColor3f", "Float", 1, "Float", 0, "Float", 0)
DllCall("opengl32\glVertex2f", "Float", 0, "Float", 0)
DllCall("opengl32\glColor3f", "Float", 0, "Float", 1, "Float", 0)
DllCall("opengl32\glVertex2f", "Float", 30, "Float", 0)
DllCall("opengl32\glColor3f", "Float", 0, "Float", 0, "Float", 1)
DllCall("opengl32\glVertex2f", "Float", 0, "Float", 30)
DllCall("opengl32\glEnd")

DllCall("SwapBuffers", "uInt", hDC)
return


GuiSize:
DllCall("opengl32\glViewport", "Int", 0, "Int", 0, "Int", A_GuiWidth, "Int", A_GuiHeight)
DllCall("opengl32\glMatrixMode", "uInt", 0x1701) ;GL_PROJECTION
DllCall("opengl32\glLoadIdentity")
DllCall("opengl32\glOrtho", "Double", -30, "Double", 30, "Double", -30, "Double", 30, "Double", -30, "Double", 30)
DllCall("opengl32\glMatrixMode", "uInt", 0x1700) ;GL_MODELVIEW
return


GuiClose:
ExitApp


ExitSub:
DllCall("opengl32\wglMakeCurrent", "uInt", 0, "uInt", 0)
DllCall("opengl32\wglDeleteContext", "uInt", hRC)
DllCall("ReleaseDC", "uInt", hDC)
DllCall("FreeLibrary", "uInt", hOpenGL32)
ExitApp
