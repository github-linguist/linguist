# Microsoft Developer Studio Project File - Name="freeglut" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=freeglut - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "freeglut.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "freeglut.mak" CFG="freeglut - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "freeglut - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "freeglut - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "freeglut - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FREEGLUT_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FREEGLUT_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "freeglut - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FREEGLUT_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "FREEGLUT_EXPORTS" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "freeglut - Win32 Release"
# Name "freeglut - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\src\freeglut_callbacks.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_cursor.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_display.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_ext.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_font.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_font_data.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_gamemode.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_geometry.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_glutfont_definitions.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_init.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_input_devices.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_joystick.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_main.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_menu.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_misc.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_overlay.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_state.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_stroke_mono_roman.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_stroke_roman.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_structure.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_teapot.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_videoresize.c
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_window.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\include\GL\freeglut.h
# End Source File
# Begin Source File

SOURCE=.\include\GL\freeglut_ext.h
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_internal.h
# End Source File
# Begin Source File

SOURCE=.\include\GL\freeglut_std.h
# End Source File
# Begin Source File

SOURCE=.\src\freeglut_teapot_data.h
# End Source File
# Begin Source File

SOURCE=.\include\GL\glut.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
