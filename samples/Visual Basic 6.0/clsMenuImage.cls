VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsMenuImage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'This code was originally written by Leandro I. Ascierto.
' Many thanks to Leandro for providing a way to apply PNG images to program menus in VB6
'
'NOTE: this file has been heavily modified for use within PhotoDemon.  Modifications include bugfixes relating to use
' under non-standard Windows themes (e.g. "Classic Theme" on Win 7 and earlier), a full conversion of the subclassing
' system to comctl32, and rewriting the class so that persistent DCs are not required for each menu image.
' (Instead, we generate them on-the-fly.)
'
'These changes rely on a number of internal PhotoDemon checks, variables, and objects, so I do not recommend using PD's
' version of the class in your own projects.  You may download the original version of this code from the following link
' (good as of June 2012): http://leandroascierto.com/blog/clsmenuimage/

Option Explicit

' ---------------------------------------------------
' Autor:            Leandro I. Ascierto
' Date:             17 de Julio de 2010
' Web:              www.leandroascierto.com.ar
' Requirements:     Windows XP or Later
' History:          17/07/2010 First
' ---------------------------------------------------


'Tanner's addition 20 September '12:
' We must check if theming is enabled. On Vista/7, use of the "Classic Theme" disables theming and screws up menu icon placement.
Private Declare Function OpenThemeData Lib "uxtheme" (ByVal hWnd As Long, ByVal pszClassList As Long) As Long
Private Declare Function CloseThemeData Lib "uxtheme" (ByVal hTheme As Long) As Long

' ------------
Private Declare Function CreateDIBSection Lib "gdi32" (ByVal hDC As Long, ByRef pBitmapInfo As BITMAPINFO, ByVal un As Long, ByRef lplpVoid As Long, ByVal Handle As Long, ByVal dw As Long) As Long
Private Declare Function GetDC Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hWnd As Long, ByVal hDC As Long) As Long
Private Declare Function SelectObject Lib "gdi32" (ByVal hDC As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function SetMenuItemInfo Lib "user32" Alias "SetMenuItemInfoW" (ByVal hMenu As Long, ByVal uItem As Long, ByVal fByPosition As Long, lpmii As MENUITEMINFO) As Long
Private Declare Function GetMenuItemInfo Lib "user32" Alias "GetMenuItemInfoW" (ByVal hMenu As Long, ByVal un As Long, ByVal b As Boolean, ByRef lpMenuItemInfo As MENUITEMINFO) As Long
Private Declare Function GetMenu Lib "user32" (ByVal hWnd As Long) As Long
Private Declare Function GetSubMenu Lib "user32" (ByVal hMenu As Long, ByVal nPos As Long) As Long
Private Declare Function GetMenuInfo Lib "user32" (ByVal hMenu As Long, ByRef LPMENUINFO As MENUINFO) As Long
Private Declare Function SetMenuInfo Lib "user32" (ByVal hMenu As Long, ByRef LPCMENUINFO As MENUINFO) As Long
Private Declare Function DrawMenuBar Lib "user32" (ByVal hWnd As Long) As Long
'Private Declare Function GetMenuItemID Lib "user32" (ByVal hMenu As Long, ByVal nPos As Long) As Long
Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
Private Declare Function Rectangle Lib "gdi32" (ByVal hDC As Long, ByVal x1 As Long, ByVal y1 As Long, ByVal x2 As Long, ByVal y2 As Long) As Long
Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Private Declare Function GetStockObject Lib "gdi32" (ByVal nIndex As Long) As Long
Private Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long

' ------------------------------------------------------- GDI Plus -------------------------------------------------------------------
Private Declare Function GdipDrawImageRectRectI Lib "gdiplus" (ByVal hGraphics As Long, ByVal hImage As Long, ByVal dstX As Long, ByVal dstY As Long, ByVal dstWidth As Long, ByVal dstHeight As Long, ByVal srcX As Long, ByVal srcY As Long, ByVal srcWidth As Long, ByVal srcHeight As Long, ByVal srcUnit As Long, Optional ByVal imageAttributes As Long = 0, Optional ByVal callback As Long = 0, Optional ByVal callbackData As Long = 0) As Long
Private Declare Function GdipCreateFromHDC Lib "gdiplus" (ByVal hDC As Long, ByRef graphics As Long) As Long
Private Declare Function GdipDeleteGraphics Lib "gdiplus" (ByVal graphics As Long) As Long
Private Declare Function GdipDisposeImage Lib "gdiplus" (ByVal Image As Long) As Long
Private Declare Function GdipGetImageDimension Lib "gdiplus" (ByVal Image As Long, ByRef Width As Single, ByRef Height As Single) As Long
Private Declare Function GdipCreateBitmapFromScan0 Lib "gdiplus" (ByVal Width As Long, ByVal Height As Long, ByVal Stride As Long, ByVal PixelFormat As Long, Scan0 As Any, Bitmap As Long) As Long
Private Declare Function GdipSetImageAttributesColorMatrix Lib "gdiplus" (ByVal imageattr As Long, ByVal ColorAdjust As Long, ByVal EnableFlag As Long, ByRef MatrixColor As COLORMATRIX, ByRef MatrixGray As Any, ByVal Flags As Long) As Long
Private Declare Function GdipCreateImageAttributes Lib "gdiplus" (ByRef imageattr As Long) As Long
Private Declare Function GdipDisposeImageAttributes Lib "gdiplus" (ByVal imageattr As Long) As Long

Private Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

Private Type MENUINFO
    cbSize As Long
    fMask As Long
    dwStyle As Long
    cyMax As Long
    RhbrBack As Long
    dwContextHelpID As Long
    dwMenuData As Long
End Type

Private Type MENUITEMINFO
    cbSize As Long
    fMask As Long
    fType As Long
    fState As Long
    wID As Long
    hSubMenu As Long
    hbmpChecked As Long
    hbmpUnchecked As Long
    dwItemData As Long
    dwTypeData As Long
    cch As Long
    hbmpItem As Long
End Type

Private Type MEASUREITEMSTRUCT
    ctlType As Long
    CtlID As Long
    itemID As Long
    itemWidth As Long
    itemHeight As Long
    itemData As Long
End Type

Private Type DRAWITEMSTRUCT
    ctlType As Long
    CtlID As Long
    itemID As Long
    itemAction As Long
    ItemState As Long
    hWndItem As Long
    hDC As Long
    rcItem As RECT
    itemData As Long
End Type

Private Type ARGB
    Blue As Byte
    Green As Byte
    Red As Byte
    Alpha As Byte
End Type

Private Type BITMAPINFOHEADER
    biSize As Long
    biWidth As Long
    biHeight As Long
    biPlanes As Integer
    biBitCount As Integer
    biCompression As Long
    biSizeImage As Long
    biXPelsPerMeter As Long
    biYPelsPerMeter As Long
    biClrUsed As Long
    biClrImportant As Long
End Type

Private Type BITMAPINFO
    bmiHeader As BITMAPINFOHEADER
    bmiColors As ARGB
End Type

Private Type COLORMATRIX
    m(0 To 4, 0 To 4) As Single
End Type

'Added by Tanner: GDI+ images use premultiplied alpha!
Private Const PixelFormat32bppPARGB As Long = &HE200B
    
Private Const DIB_RGB_COLORS As Long = 0&

Private Const MIIM_ID As Long = &H2
Private Const MIIM_DATA As Long = &H20
Private Const MIIM_BITMAP As Long = &H80
    
Private Const MIM_APPLYTOSUBMENUS = &H80000000
Private Const MIM_STYLE As Long = &H10

Private Const ODT_MENU As Long = 1
Private Const ODS_GRAYED As Long = &H2
Private Const ODS_CHECKED As Long = &H8
Private Const MNS_CHECKORBMP As Long = &H4000000
Private Const MNS_NOCHECK As Long = &H80000000
Private Const HBMMENU_CALLBACK As Long = -1

Private Const NULL_BRUSH As Long = 5
Private Const COLOR_GRAYTEXT As Long = 17
Private Const COLOR_APPWORKSPACE As Long = 12
Private Const SM_CXMENUCHECK As Long = 71
Private Const WM_MEASUREITEM As Long = &H2C
Private Const WM_DRAWITEM As Long = &H2B

Private m_hWnd As Long

Private m_lWidth As Long
Private m_lHeight As Long

Private Type MemoDIB
    hDIB As Long
    ptr As Long
End Type
Private mDIB() As MemoDIB
Private cColl As Collection

Private m_ClassicThemeWorkaround As Boolean

'Tanner's addition: convert this class to use IDE-safe subclassing
Implements ISubclass
Private m_SubclassActive As Boolean

'Edit by Tanner: instead of relying on UBound() for tracking the number of active DIB handles, track the
' number internally.  (This lets us be *much* more efficient about memory allocations.)
Private Const INITIAL_DIB_ARRAY_SIZE As Long = 32
Private m_NumOfDIBs As Long


Friend Property Get CanWeTheme() As Boolean
    CanWeTheme = Not m_ClassicThemeWorkaround
End Property

Friend Property Get ImageCount() As Long
    ImageCount = m_NumOfDIBs
End Property


Friend Function RemoveImage(ByVal Index As Long) As Boolean
    
    If (Index < 0) Or (Index >= m_NumOfDIBs) Then Exit Function
    If (DeleteObject(mDIB(Index).hDIB) <> 0) Then PDDebug.UpdateResourceTracker PDRT_hDIB, -1
        
    Dim i As Long
    For i = Index To m_NumOfDIBs - 2
        mDIB(i) = mDIB(i + 1)
    Next

    m_NumOfDIBs = m_NumOfDIBs - 1
    
    RemoveImage = True
    
End Function

Friend Function PutImageToVBMenu(ByVal imageID As Long, ByVal MenuPos As Long, ParamArray vSubMenuPos() As Variant) As Boolean
    
    On Error Resume Next
    
    Dim hMenu As Long
    Dim hSubMenu As Long
    Dim MII As MENUITEMINFO
    Dim v As Variant
    Dim sKey As String

    hMenu = GetMenu(m_hWnd)
    
    Dim tmpInfo As MENUINFO
    
    If m_ClassicThemeWorkaround Then
        tmpInfo.cbSize = Len(tmpInfo)
        tmpInfo.fMask = MIM_APPLYTOSUBMENUS Or MIM_STYLE
        tmpInfo.dwStyle = MNS_CHECKORBMP
        SetMenuInfo hMenu, tmpInfo
    End If
    
    hSubMenu = hMenu
    
    For Each v In vSubMenuPos
        hSubMenu = GetSubMenu(hSubMenu, v)
    Next
    
    With MII
        .cbSize = Len(MII)
        .fMask = MIIM_ID
    End With
    
    If (GetMenuItemInfo(hSubMenu, MenuPos, True, MII) <> 0) Then
        
        sKey = hSubMenu & "-" & MII.wID
    
        With MII
            '.cbSize = Len(MII)
            .fMask = MIIM_BITMAP 'Or MIIM_DATA
            
            If (imageID = -1) Then
                .hbmpItem = 0
                
                'Edit by Tanner: retain the key, but just remove the bitmap from the menu.  (We may decide to
                ' reactivate this item later.)
                'If KeyExists(sKey) Then Call cColl.Remove(sKey)
                
            Else
                If OS.IsVistaOrLater Then
                    .hbmpItem = mDIB(imageID).hDIB
                Else
                    .hbmpItem = HBMMENU_CALLBACK
                    
                    'Edit by Tanner: this check is unnecessary, as we apply our own check prior to even
                    ' calling this function.
                    'If KeyExists(sKey) Then Call cColl.Remove(sKey)
                    cColl.Add imageID, sKey
                    
                End If
            End If
            
            '.dwItemData = ImageID
            
        End With
        
        PutImageToVBMenu = SetMenuItemInfo(hSubMenu, MenuPos, True, MII)
        
        If (hSubMenu = hMenu) Then DrawMenuBar m_hWnd
    
    End If
    
End Function

Friend Function PutImageToApiMenu(ByVal imageID As Long, ByVal hMenu As Long, ByVal MenuPos As Long, Optional ByVal itemData As Long) As Boolean

    Dim MII As MENUITEMINFO
    Dim sKey As String

    With MII
        .cbSize = Len(MII)
        .fMask = MIIM_ID
    End With
    
    If GetMenuItemInfo(hMenu, MenuPos, True, MII) = 0 Then Exit Function
        
    sKey = hMenu & "-" & MII.wID

    With MII
        .fMask = MIIM_BITMAP Or MIIM_DATA
        
        If imageID = -1 Then
            .hbmpItem = 0
            If KeyExists(sKey) Then cColl.Remove sKey
        Else
            If OS.IsVistaOrLater Then
                .hbmpItem = mDIB(imageID).hDIB
            Else
                .hbmpItem = HBMMENU_CALLBACK
                If KeyExists(sKey) Then cColl.Remove sKey
                cColl.Add imageID, sKey
            End If
        End If
        .dwItemData = itemData
    End With
    
    PutImageToApiMenu = SetMenuItemInfo(hMenu, MenuPos, True, MII)

End Function

Friend Sub RemoveMenuCheckApi(ByVal hMenu As Long)
    Dim MI As MENUINFO
    
    With MI
        .cbSize = Len(MI)
        .fMask = MIM_STYLE
        .dwStyle = MNS_NOCHECK
    End With

    SetMenuInfo hMenu, MI
End Sub

Friend Sub RemoveMenuCheckVB(ParamArray vSubMenuPos() As Variant)
    Dim MI As MENUINFO
    Dim hMenu As Long
    Dim hSubMenu As Long
    Dim v As Variant

    hMenu = GetMenu(m_hWnd)
    
    hSubMenu = hMenu
    
    For Each v In vSubMenuPos
        hSubMenu = GetSubMenu(hSubMenu, v)
    Next
    
    With MI
        .cbSize = Len(MI)
        .fMask = MIM_STYLE
        .dwStyle = MNS_NOCHECK
    End With

    SetMenuInfo hSubMenu, MI
End Sub

Private Sub DrawCheck(ByVal hDC As Long, ByVal x As Long, ByVal y As Long, ByVal x2 As Long, ByVal y2 As Long, bDisabled As Boolean)
    
    Dim hPen As Long, oldPen As Long
    Dim hBrush As Long, oldBrush As Long
    
    hPen = CreatePen(0, 1, GetSysColor(IIf(bDisabled, COLOR_GRAYTEXT, COLOR_APPWORKSPACE)))
    hBrush = GetStockObject(NULL_BRUSH)
    
    oldPen = SelectObject(hDC, hPen)
    oldBrush = SelectObject(hDC, hBrush)
    
    Rectangle hDC, x, y, x + x2, y + y2
    
    SelectObject hDC, oldPen
    DeleteObject hPen
    SelectObject hDC, oldBrush
    
End Sub

Private Function CreateNewDib() As Long

    'Prepare a bmp header for the DIB
    Dim tBITMAPINFO As BITMAPINFO
    With tBITMAPINFO.bmiHeader
        .biSize = Len(tBITMAPINFO.bmiHeader)
        .biBitCount = 32
        .biHeight = m_lWidth
        .biWidth = m_lHeight
        .biPlanes = 1
        .biSizeImage = .biWidth * .biHeight * 4&
    End With
    
    'Make sure we have enough room in our tracking array
    If (m_NumOfDIBs > UBound(mDIB)) Then ReDim Preserve mDIB(0 To m_NumOfDIBs * 2 - 1)
    Dim Index As Long
    Index = m_NumOfDIBs
    
    With mDIB(Index)
    
        'Create a temporary memory DC, because CreateDIBSection requires a DC for palette matching
        Dim tmpDC As Long
        tmpDC = GetDC(0&)
        .hDIB = CreateDIBSection(tmpDC, tBITMAPINFO, DIB_RGB_COLORS, .ptr, 0&, 0&)
        ReleaseDC 0&, tmpDC
        
        If (.hDIB <> 0) Then
            CreateNewDib = Index
            PDDebug.UpdateResourceTracker PDRT_hDIB, 1
        Else
            CreateNewDib = -1
            Exit Function
        End If
        
    End With

    m_NumOfDIBs = m_NumOfDIBs + 1

End Function

Friend Sub Clear()

    Dim i As Long
    
    For i = 0 To m_NumOfDIBs - 1
        If (mDIB(i).hDIB <> 0) Then
            If (DeleteObject(mDIB(i).hDIB) <> 0) Then
                PDDebug.UpdateResourceTracker PDRT_hDIB, -1
                mDIB(i).hDIB = 0
            End If
        End If
    Next
    
    m_NumOfDIBs = 0
    
End Sub

Private Sub DrawDIB(ByVal DestHdc As Long, ByVal x As Long, ByVal y As Long, ByVal Index As Long, Disabled As Boolean)

    Dim hGraphics As Long
    Dim hImage As Long
    Dim hAttributes As Long
    Dim tMatrixColor As COLORMATRIX
    Dim tMatrixGray As COLORMATRIX
    
    If (Index < 0) Or (Index >= m_NumOfDIBs) Then Exit Sub

    If (GdipCreateBitmapFromScan0(m_lWidth, m_lHeight, m_lWidth * 4&, PixelFormat32bppPARGB, ByVal mDIB(Index).ptr, hImage) = 0) Then

        If (GdipCreateFromHDC(DestHdc, hGraphics) = 0) Then
            
            'Edit by Tanner: Leandro's original code flipped the DIB to make it top-down; we don't require this for DIBs
            ' we've created and cached ourselves.
            'GdipImageRotateFlip hImage, &H6
            
            If Disabled Then
            
                GdipCreateImageAttributes hAttributes
                
                With tMatrixColor
                    .m(0, 0) = 0.299
                    .m(1, 0) = .m(0, 0)
                    .m(2, 0) = .m(0, 0)
                    .m(0, 1) = 0.587
                    .m(1, 1) = .m(0, 1)
                    .m(2, 1) = .m(0, 1)
                    .m(0, 2) = 0.114
                    .m(1, 2) = .m(0, 2)
                    .m(2, 2) = .m(0, 2)
                    .m(3, 3) = 0.5
                    .m(4, 4) = 1
                End With
                
                Const ColorAdjustTypeDefault As Long = 0, ColorMatrixFlagsDefault As Long = 0
                GdipSetImageAttributesColorMatrix hAttributes, ColorAdjustTypeDefault, 1&, tMatrixColor, tMatrixGray, ColorMatrixFlagsDefault
    
            End If
            
            GdipDrawImageRectRectI hGraphics, hImage, x, y, m_lWidth, m_lHeight, 0, 0, m_lWidth, m_lHeight, &H2, hAttributes, 0&, 0&
            
            If (hAttributes <> 0) Then GdipDisposeImageAttributes hAttributes
            If (hGraphics <> 0) Then GdipDeleteGraphics hGraphics
        
        End If
        
        If (hImage <> 0) Then GdipDisposeImage hImage
    
    End If
    
End Sub

'CREATED BY TANNER: load an image from an existing pdDIB object
Friend Function AddImageFromDIB(ByRef srcDIB As pdDIB, Optional bGhosted As Boolean) As Boolean

    Dim hImage As Long
    If (Not srcDIB Is Nothing) Then
        
        'Update 29 August 2017 - I've written a new, much faster function that simply clones the incoming DIB.
        If (Not bGhosted) Then
            AddImageFromDIB = pvAddImagen_Tanner(srcDIB)
        Else
            GDI_Plus.GetGdipBitmapHandleFromDIB hImage, srcDIB
            If (hImage <> 0) Then AddImageFromDIB = pvAddImagen(hImage, bGhosted) Else PDDebug.LogAction "WARNING!  AddImageFromDIB failed; hImage = 0"
        End If
        
    Else
        PDDebug.LogAction "WARNING!  clsMenuImage.AddImageFromDIB failed; source DIB was null."
    End If
    
End Function

Private Function pvAddImagen_Tanner(ByRef srcDIB As pdDIB) As Boolean
    
    If (m_NumOfDIBs > UBound(mDIB)) Then ReDim Preserve mDIB(0 To m_NumOfDIBs * 2 - 1)
    srcDIB.TransferDIBOwnership mDIB(m_NumOfDIBs).hDIB, mDIB(m_NumOfDIBs).ptr
    
    m_NumOfDIBs = m_NumOfDIBs + 1
    pvAddImagen_Tanner = True
    
End Function

Private Function pvAddImagen(ByVal hImage As Long, Optional bGhosted As Boolean) As Boolean
    
    Dim hGraphics As Long
    Dim imgWidth As Single
    Dim imgHeight As Single
    Dim Index As Long
    Dim oldHDib As Long
    
    If (hImage <> 0) Then
    
        Index = CreateNewDib()
        
        If (Index <> -1) Then
            
            Dim tmpDC As Long
            tmpDC = GDI.GetMemoryDC()
            
            oldHDib = SelectObject(tmpDC, mDIB(Index).hDIB)
        
            GdipCreateFromHDC tmpDC, hGraphics
        
            GdipGetImageDimension hImage, imgWidth, imgHeight
            
            If bGhosted Then
                Dim tMatrixColor As COLORMATRIX
                Dim tMatrixGray As COLORMATRIX
                Dim hAttributes As Long
            
                GdipCreateImageAttributes hAttributes
                
                With tMatrixColor
                    .m(0, 0) = 1
                    .m(1, 1) = 1
                    .m(2, 2) = 1
                    .m(3, 3) = 0.7
                    .m(4, 4) = 1

                End With
                
                Const ColorAdjustTypeDefault As Long = 0, ColorMatrixFlagsDefault As Long = 0
                GdipSetImageAttributesColorMatrix hAttributes, ColorAdjustTypeDefault, True, tMatrixColor, tMatrixGray, ColorMatrixFlagsDefault
                
            End If

            GdipDrawImageRectRectI hGraphics, hImage, 0, 0, m_lWidth, m_lHeight, 0, 0, imgWidth, imgHeight, &H2, hAttributes, 0&, 0&
            
            If (hAttributes <> 0) Then GdipDisposeImageAttributes hAttributes
    
            GdipDisposeImage hImage
            GdipDeleteGraphics hGraphics
            
            SelectObject tmpDC, oldHDib
            GDI.FreeMemoryDC tmpDC
            
            pvAddImagen = True
        
        End If

    End If
End Function

Friend Function Init(ByVal hWnd As Long, imgWidth As Long, ByVal imgHeight As Long, Optional ByVal bRaiseEvent As Boolean = False) As Boolean

    If (Drawing2D.IsRenderingEngineActive(P2_GDIPlusBackend) And PDMain.IsProgramRunning()) Then
    
        m_lWidth = imgWidth
        m_lHeight = imgHeight
        
        If (m_NumOfDIBs > 0) Then Me.Clear
        
        If (Not OS.IsVistaOrLater) Or bRaiseEvent Then
            If (m_hWnd <> 0) Then StopSubclassing
            m_hWnd = hWnd
            If (m_hWnd <> 0) Then Init = SetSubclassing() Else Init = False
            PDDebug.LogAction "clsMenuImage requires subclassing; SetSubclassing() returned " & CStr(Init)
        Else
            m_hWnd = hWnd
            Init = True
        End If
    
    End If
    
End Function

Private Function SetSubclassing() As Boolean
    If PDMain.IsProgramRunning() Then
        m_SubclassActive = VBHacks.StartSubclassing(m_hWnd, Me)
        SetSubclassing = m_SubclassActive
    Else
        SetSubclassing = True
    End If
End Function

Private Function StopSubclassing() As Boolean

    If (m_SubclassActive And (m_hWnd <> 0)) Then
        VBHacks.StopSubclassing m_hWnd, Me
        m_SubclassActive = False
        StopSubclassing = True
    Else
        StopSubclassing = False
    End If

End Function

Private Sub Class_Initialize()
    
    Set cColl = New Collection
    
    'Tanner's addition 20 September '12:
    ' Double-check that theming is enabled. If it isn't, fall back to subclassing (e.g. Windows XP style)
    Dim hTheme As Long
    Dim sClass As String
    sClass = "Window"
    hTheme = OpenThemeData(FormMain.hWnd, StrPtr(sClass))
    If hTheme = 0 Then
        m_ClassicThemeWorkaround = True
    Else
        CloseThemeData hTheme
    End If
    
    ReDim mDIB(0 To INITIAL_DIB_ARRAY_SIZE - 1)
    m_NumOfDIBs = 0
    
End Sub

Private Sub Class_Terminate()
    StopSubclassing
    Me.Clear
End Sub

Private Function KeyExists(ByRef sKey As String) As Boolean
    On Error GoTo HandleError:
    Dim tVal As String 'Variant
    
    tVal = cColl(sKey)
  
    KeyExists = True
    Exit Function

HandleError:
    Err.Clear
End Function

Private Function HandleMeasureItem(ByVal hWnd As Long, ByVal uiMsg As Long, ByVal wParam As Long, ByVal lParam As Long, ByRef msgEaten As Boolean) As Long
    
    'Start by calling the default wndproc.  (We want to let the default menu handler populate bits like text,
    ' and once it's done, we'll overwrite the image handling bits with our own code.)
    HandleMeasureItem = VBHacks.DefaultSubclassProc(hWnd, uiMsg, wParam, lParam)
    
    Dim MIS As MEASUREITEMSTRUCT
    CopyMemoryStrict VarPtr(MIS), lParam, LenB(MIS)
    
    If (MIS.ctlType = ODT_MENU) Then

        If (MIS.itemHeight < m_lHeight + 4) Then MIS.itemHeight = m_lHeight + 4
        
        If OS.IsVistaOrLater Then
            If m_ClassicThemeWorkaround Then MIS.itemWidth = MIS.itemWidth + m_lWidth + 6&
        Else
            MIS.itemWidth = MIS.itemWidth + m_lWidth + 2&
        End If
        
        CopyMemoryStrict lParam, VarPtr(MIS), LenB(MIS)
        HandleMeasureItem = 1
        
    End If
    
    msgEaten = True
    
End Function

Private Function HandleDrawItem(ByVal hWnd As Long, ByVal uiMsg As Long, ByVal wParam As Long, ByVal lParam As Long, ByRef msgEaten As Boolean) As Long
    
    'Start by calling the default wndproc.  (We want to let the default menu handler populate bits like text,
    ' and once it's done, we'll overwrite the image handling bits with our own code.)
    HandleDrawItem = VBHacks.DefaultSubclassProc(hWnd, uiMsg, wParam, lParam)
    
    Dim DIS As DRAWITEMSTRUCT
    Dim isDisabled As Boolean, isCheckStyle As Boolean, isChecked As Boolean
    Dim lLeft As Long
    Dim MI As MENUINFO
    
    CopyMemoryStrict VarPtr(DIS), lParam, LenB(DIS)

    If (DIS.ctlType = ODT_MENU) Then
    
        If (DIS.hWndItem <> GetMenu(hWnd)) Then
            
            With MI
                .cbSize = Len(MI)
                .fMask = MIM_STYLE
            End With

            GetMenuInfo DIS.hWndItem, MI

            isCheckStyle = (MI.dwStyle And MNS_NOCHECK) <> MNS_NOCHECK
            isChecked = (DIS.ItemState And ODS_CHECKED) = ODS_CHECKED
            lLeft = IIf(isCheckStyle, GetSystemMetrics(SM_CXMENUCHECK), 0)
        
        End If
        
        isDisabled = (DIS.ItemState And ODS_GRAYED) = ODS_GRAYED
        
        If Not isCheckStyle And isChecked Then
            DrawCheck DIS.hDC, lLeft, DIS.rcItem.Top, m_lWidth + 4, m_lHeight + 4, isDisabled
        End If
        
        'Tanner edit: this is used to prevent a bug where the hDC value is invalid for disabled
        ' and non-hovered menu entries for users of the "classic theme"; we also need to check for key
        ' existence as images only exist for some menu items.
        If (Not (m_ClassicThemeWorkaround And isDisabled)) And KeyExists(DIS.hWndItem & "-" & DIS.itemID) Then
            DrawDIB DIS.hDC, lLeft - 4, DIS.rcItem.Top + 2, cColl(DIS.hWndItem & "-" & DIS.itemID), isDisabled
        End If
        
        HandleDrawItem = 1
        
    End If
    
    msgEaten = True
    
End Function

Private Function ISubclass_WindowMsg(ByVal hWnd As Long, ByVal uiMsg As Long, ByVal wParam As Long, ByVal lParam As Long, ByVal dwRefData As Long) As Long
    
    'Sometimes we consume messages before passing them on.  Sometimes we don't.  Child functions will update this as necessary.
    Dim msgEaten As Boolean: msgEaten = False
    
    If (uiMsg = WM_MEASUREITEM) Then
        ISubclass_WindowMsg = HandleMeasureItem(hWnd, uiMsg, wParam, lParam, msgEaten)
    
    ElseIf (uiMsg = WM_DRAWITEM) Then
        ISubclass_WindowMsg = HandleDrawItem(hWnd, uiMsg, wParam, lParam, msgEaten)
    
    ElseIf (uiMsg = WM_NCDESTROY) Then
        VBHacks.StopSubclassing hWnd, Me
        m_SubclassActive = False
    End If
    
    If (Not msgEaten) Then ISubclass_WindowMsg = VBHacks.DefaultSubclassProc(hWnd, uiMsg, wParam, lParam)
    
End Function
