; bigtest.nsi
;
; This script attempts to test most of the functionality of the NSIS exehead.

;--------------------------------

!ifdef HAVE_UPX
!packhdr tmp.dat "upx\upx -9 tmp.dat"
!endif

!ifdef NOCOMPRESS
SetCompress off
!endif

;--------------------------------

Name "BigNSISTest"
Caption "NSIS Big Test"
Icon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-install.ico"
OutFile "bigtest.exe"

SetDateSave on
SetDatablockOptimize on
CRCCheck on
SilentInstall normal
BGGradient 000000 800000 FFFFFF
InstallColors FF8080 000030
XPStyle on

InstallDir "$PROGRAMFILES\NSISTest\BigNSISTest"
InstallDirRegKey HKLM "Software\NSISTest\BigNSISTest" "Install_Dir"

CheckBitmap "${NSISDIR}\Contrib\Graphics\Checks\classic-cross.bmp"

LicenseText "A test text, make sure it's all there"
LicenseData "bigtest.nsi"

RequestExecutionLevel admin

;--------------------------------

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

!ifndef NOINSTTYPES ; only if not defined
  InstType "Most"
  InstType "Full"
  InstType "More"
  InstType "Base"
  ;InstType /NOCUSTOM
  ;InstType /COMPONENTSONLYONCUSTOM
!endif

AutoCloseWindow false
ShowInstDetails show

;--------------------------------

Section "" ; empty string makes it hidden, so would starting with -

  ; write reg info
  StrCpy $1 "POOOOOOOOOOOP"
  DetailPrint "I like to be able to see what is going on (debug) $1"
  WriteRegStr HKLM SOFTWARE\NSISTest\BigNSISTest "Install_Dir" "$INSTDIR"

  ; write uninstall strings
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\BigNSISTest" "DisplayName" "BigNSISTest (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\BigNSISTest" "UninstallString" '"$INSTDIR\bt-uninst.exe"'

  SetOutPath $INSTDIR
  File /a "silent.nsi"
  CreateDirectory "$INSTDIR\MyProjectFamily\MyProject" ; 2 recursively create a directory for fun.
  WriteUninstaller "bt-uninst.exe"
  
  Nop ; for fun

SectionEnd

Section "TempTest"

SectionIn 1 2 3
  Start: MessageBox MB_OK "Start:"

  MessageBox MB_YESNO "Goto MyLabel" IDYES MyLabel

  MessageBox MB_OK "Right before MyLabel:"

  MyLabel: MessageBox MB_OK "MyLabel:"
  
  MessageBox MB_OK "Right after MyLabel:"

  MessageBox MB_YESNO "Goto Start:?" IDYES Start

SectionEnd

SectionGroup /e SectionGroup1

Section "Test Registry/INI functions"

SectionIn 1 4 3

  WriteRegStr HKLM SOFTWARE\NSISTest\BigNSISTest "StrTest_INSTDIR" "$INSTDIR"
  WriteRegDword HKLM SOFTWARE\NSISTest\BigNSISTest "DwordTest_0xDEADBEEF" 0xdeadbeef
  WriteRegDword HKLM SOFTWARE\NSISTest\BigNSISTest "DwordTest_123456" 123456
  WriteRegDword HKLM SOFTWARE\NSISTest\BigNSISTest "DwordTest_0123" 0123
  WriteRegBin HKLM SOFTWARE\NSISTest\BigNSISTest "BinTest_deadbeef01f00dbeef" "DEADBEEF01F00DBEEF"
  StrCpy $8 "$SYSDIR\IniTest"
  WriteINIStr "$INSTDIR\test.ini"  "MySection" "Value1" $8
  WriteINIStr "$INSTDIR\test.ini"  "MySectionIni" "Value1" $8
  WriteINIStr "$INSTDIR\test.ini"  "MySectionIni" "Value2" $8
  WriteINIStr "$INSTDIR\test.ini"  "IniOn" "Value1" $8

  Call MyFunctionTest

  DeleteINIStr "$INSTDIR\test.ini" "IniOn" "Value1"
  DeleteINISec "$INSTDIR\test.ini" "MySectionIni"

  ReadINIStr $1 "$INSTDIR\test.ini" "MySectionIni" "Value1"
  StrCmp $1 "" INIDelSuccess
    MessageBox MB_OK "DeleteINISec failed"
  INIDelSuccess:

  ClearErrors
  ReadRegStr $1 HKCR "software\microsoft" xyz_cc_does_not_exist
  IfErrors 0 NoError
    MessageBox MB_OK "could not read from HKCR\software\microsoft\xyz_cc_does_not_exist"
    Goto ErrorYay
  NoError:
    MessageBox MB_OK "read '$1' from HKCR\software\microsoft\xyz_cc_does_not_exist"
  ErrorYay:
  
SectionEnd

Section "Test CreateShortCut"

  SectionIn 1 2 3

  Call CSCTest

SectionEnd

SectionGroup Group2

Section "Test Branching" 
  
  BeginTestSection:
  SectionIn 1 2 3
 
  SetOutPath $INSTDIR

  IfFileExists "$INSTDIR\LogicLib.nsi" 0 BranchTest69
    
    MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to overwrite $INSTDIR\LogicLib.nsi?" IDNO NoOverwrite ; skipped if file doesn't exist

    BranchTest69:
  
    SetOverwrite ifnewer ; NOT AN INSTRUCTION, NOT COUNTED IN SKIPPINGS

  NoOverwrite:

  File "LogicLib.nsi" ; skipped if answered no
  SetOverwrite try ; NOT AN INSTRUCTION, NOT COUNTED IN SKIPPINGS

  MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to skip the rest of this section?" IDYES EndTestBranch
  MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to go back to the beginning of this section?" IDYES BeginTestSection
  MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to hide the installer and wait five seconds?" IDNO NoHide

    HideWindow
    Sleep 5000
    BringToFront

  NoHide:

  MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to call the function 5 times?" IDNO NoRecurse

    StrCpy $1 "x"

  LoopTest: 
      
    Call myfunc
    StrCpy $1 "x$1"
    StrCmp $1 "xxxxxx" 0 LoopTest
      
  NoRecurse:

  EndTestBranch:

SectionEnd

SectionGroupEnd

Section "Test CopyFiles"

  SectionIn 1 2 3

  SetOutPath $INSTDIR\cpdest
  CopyFiles "$WINDIR\*.ini" "$INSTDIR\cpdest" 0

SectionEnd

SectionGroupEnd

Section "Test Exec functions" TESTIDX

  SectionIn 1 2 3
  
  SearchPath $1 notepad.exe

  MessageBox MB_OK "notepad.exe=$1"
  Exec '"$1"'
  ExecShell "open" '"$INSTDIR"'
  Sleep 500
  BringToFront

SectionEnd

Section "Test ActiveX control registration"

  SectionIn 2

  UnRegDLL "$SYSDIR\spin32.ocx"
  Sleep 1000
  RegDLL "$SYSDIR\spin32.ocx"
  Sleep 1000
  
SectionEnd

;--------------------------------

Function "CSCTest"
  
  CreateDirectory "$SMPROGRAMS\Big NSIS Test"
  SetOutPath $INSTDIR ; for working directory
  CreateShortCut "$SMPROGRAMS\Big NSIS Test\Uninstall BIG NSIS Test.lnk" "$INSTDIR\bt-uninst.exe" ; use defaults for parameters, icon, etc.
  ; this one will use notepad's icon, start it minimized, and give it a hotkey (of Ctrl+Shift+Q)
  CreateShortCut "$SMPROGRAMS\Big NSIS Test\silent.nsi.lnk" "$INSTDIR\silent.nsi" "" "$WINDIR\notepad.exe" 0 SW_SHOWMINIMIZED CONTROL|SHIFT|Q
  CreateShortCut "$SMPROGRAMS\Big NSIS Test\TheDir.lnk" "$INSTDIR\" "" "" 0 SW_SHOWMAXIMIZED CONTROL|SHIFT|Z

FunctionEnd

Function myfunc

  StrCpy $2 "MyTestVar=$1"
  MessageBox MB_OK "myfunc: $2"

FunctionEnd

Function MyFunctionTest

  ReadINIStr $1 "$INSTDIR\test.ini" "MySectionIni" "Value1"
  StrCmp $1 $8 NoFailedMsg
    MessageBox MB_OK "WriteINIStr failed"
  
  NoFailedMsg:

FunctionEnd

Function .onSelChange

  SectionGetText ${TESTIDX} $0
  StrCmp $0 "" e
    SectionSetText ${TESTIDX} ""
  Goto e2
e:
  SectionSetText ${TESTIDX} "TextInSection"
e2:

FunctionEnd

;--------------------------------

; Uninstaller

UninstallText "This will uninstall example2. Hit next to continue."
UninstallIcon "${NSISDIR}\Contrib\Graphics\Icons\nsis1-uninstall.ico"

Section "Uninstall"

  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\BigNSISTest"
  DeleteRegKey HKLM "SOFTWARE\NSISTest\BigNSISTest"
  Delete "$INSTDIR\silent.nsi"
  Delete "$INSTDIR\LogicLib.nsi"
  Delete "$INSTDIR\bt-uninst.exe"
  Delete "$INSTDIR\test.ini"
  Delete "$SMPROGRAMS\Big NSIS Test\*.*"
  RMDir "$SMPROGRAMS\BiG NSIS Test"
  
  MessageBox MB_YESNO|MB_ICONQUESTION "Would you like to remove the directory $INSTDIR\cpdest?" IDNO NoDelete
    Delete "$INSTDIR\cpdest\*.*"
    RMDir "$INSTDIR\cpdest" ; skipped if no
  NoDelete:
  
  RMDir "$INSTDIR\MyProjectFamily\MyProject"
  RMDir "$INSTDIR\MyProjectFamily"
  RMDir "$INSTDIR"

  IfFileExists "$INSTDIR" 0 NoErrorMsg
    MessageBox MB_OK "Note: $INSTDIR could not be removed!" IDOK 0 ; skipped if file doesn't exist
  NoErrorMsg:

SectionEnd
