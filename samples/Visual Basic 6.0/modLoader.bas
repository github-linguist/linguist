Attribute VB_Name = "modLoader"
'[modLoader.bas]

' // modLoader.bas - EXE (VB6) loader from memory
' // © Krivous Anatoly Anatolevich (The trick), 2016

Option Explicit

Public Enum MessagesID
    MID_ERRORLOADINGCONST = 100     ' // Errors
    MID_ERRORREADINGPROJECT = 101   '
    MID_ERRORCOPYINGFILE = 102      '
    MID_ERRORWIN32 = 103            '
    MID_ERROREXECUTELINE = 104      '
    MID_ERRORSTARTUPEXE = 105       '
    Project = 200                   ' // Project resource ID
    API_LIB_KERNEL32 = 300          ' // Library names
    API_LIB_NTDLL = 350             '
    API_LIB_USER32 = 400            '
    MSG_LOADER_ERROR = 500
End Enum

Private Enum ERROR_MESSAGES
    EM_NO_ERRORS
    EM_UNABLE_TO_GET_NT_HEADERS
    EM_INVALID_DATA_DIRECTORY
    EM_UNABLE_TO_ALLOCATE_MEMORY
    EM_UNABLE_TO_PROTECT_MEMORY
    EM_LOADLIBRARY_FAILED
    EM_PROCESS_INFORMATION_NOT_FOUND
    EM_END
End Enum

Private Type IMAGE_DOS_HEADER
    e_magic_e_cblp                  As Long
    e_cp                            As Integer
    e_crlc                          As Integer
    e_cparhdr                       As Integer
    e_minalloc                      As Integer
    e_maxalloc                      As Integer
    e_ss                            As Integer
    e_sp                            As Integer
    e_csum                          As Integer
    e_ip                            As Integer
    e_cs                            As Integer
    e_lfarlc                        As Integer
    e_ovno                          As Integer
    e_res(0 To 3)                   As Integer
    e_oemid                         As Integer
    e_oeminfo                       As Integer
    e_res2(0 To 9)                  As Integer
    e_lfanew                        As Long
End Type
Private Type IMAGE_DATA_DIRECTORY
    VirtualAddress                  As Long
    Size                            As Long
End Type
Private Type IMAGE_OPTIONAL_HEADER
    Magic                           As Integer
    MajorLinkerVersion              As Byte
    MinorLinkerVersion              As Byte
    SizeOfCode                      As Long
    SizeOfInitializedData           As Long
    SizeOfUnitializedData           As Long
    AddressOfEntryPoint             As Long
    BaseOfCode                      As Long
    BaseOfData                      As Long
    ImageBase                       As Long
    SectionAlignment                As Long
    FileAlignment                   As Long
    MajorOperatingSystemVersion     As Integer
    MinorOperatingSystemVersion     As Integer
    MajorImageVersion               As Integer
    MinorImageVersion               As Integer
    MajorSubsystemVersion           As Integer
    MinorSubsystemVersion           As Integer
    W32VersionValue                 As Long
    SizeOfImage                     As Long
    SizeOfHeaders                   As Long
    CheckSum                        As Long
    SubSystem                       As Integer
    DllCharacteristics              As Integer
    SizeOfStackReserve              As Long
    SizeOfStackCommit               As Long
    SizeOfHeapReserve               As Long
    SizeOfHeapCommit                As Long
    LoaderFlags                     As Long
    NumberOfRvaAndSizes             As Long
    DataDirectory(15)               As IMAGE_DATA_DIRECTORY
End Type
Private Type IMAGE_FILE_HEADER
    Machine                         As Integer
    NumberOfSections                As Integer
    TimeDateStamp                   As Long
    PointerToSymbolTable            As Long
    NumberOfSymbols                 As Long
    SizeOfOptionalHeader            As Integer
    Characteristics                 As Integer
End Type
Private Type IMAGE_NT_HEADERS
    Signature                       As Long
    FileHeader                      As IMAGE_FILE_HEADER
    OptionalHeader                  As IMAGE_OPTIONAL_HEADER
End Type
Private Type IMAGE_SECTION_HEADER
    SectionName(1)                  As Long
    VirtualSize                     As Long
    VirtualAddress                  As Long
    SizeOfRawData                   As Long
    PointerToRawData                As Long
    PointerToRelocations            As Long
    PointerToLinenumbers            As Long
    NumberOfRelocations             As Integer
    NumberOfLinenumbers             As Integer
    Characteristics                 As Long
End Type
Private Type IMAGE_IMPORT_DESCRIPTOR
    Characteristics                 As Long
    TimeDateStamp                   As Long
    ForwarderChain                  As Long
    pName                           As Long
    FirstThunk                      As Long
End Type

Private Type IMAGE_BASE_RELOCATION
    VirtualAddress                  As Long
    SizeOfBlock                     As Long
End Type

Private Type UNICODE_STRING
    Length                          As Integer
    MaxLength                       As Integer
    lpBuffer                        As Long
End Type
Private Type PROCESS_BASIC_INFORMATION
    ExitStatus                      As Long
    PebBaseAddress                  As Long
    AffinityMask                    As Long
    BasePriority                    As Long
    UniqueProcessId                 As Long
    InheritedFromUniqueProcessId    As Long
End Type
Public Type LIST_ENTRY
    Flink                           As Long
    Blink                           As Long
End Type
Public Type PEB_LDR_DATA
    Length                          As Long
    Initialized                     As Long
    SsHandle                        As Long
    InLoadOrderModuleList           As LIST_ENTRY
    InMemoryOrderModuleList         As LIST_ENTRY
    InInitializationOrderModuleList As LIST_ENTRY
End Type
Public Type LDR_MODULE
    InLoadOrderModuleList           As LIST_ENTRY
    InMemoryOrderModuleList         As LIST_ENTRY
    InInitOrderModuleList           As LIST_ENTRY
    BaseAddress                     As Long
    EntryPoint                      As Long
    SizeOfImage                     As Long
    FullDllName                     As UNICODE_STRING
    BaseDllName                     As UNICODE_STRING
    Flags                           As Long
    LoadCount                       As Integer
    TlsIndex                        As Integer
    HashTableEntry                  As LIST_ENTRY
    TimeDateStamp                   As Long
End Type

Private Type PEB
    NotUsed                         As Long
    Mutant                          As Long
    ImageBaseAddress                As Long
    LoaderData                      As Long ' // Pointer to PEB_LDR_DATA
    ProcessParameters               As Long
    ' // ....
End Type

Private Const IMAGE_FILE_MACHINE_I386               As Long = &H14C
Private Const IMAGE_DOS_SIGNATURE                   As Long = &H5A4D
Private Const IMAGE_NT_SIGNATURE                    As Long = &H4550&
Private Const IMAGE_NT_OPTIONAL_HDR32_MAGIC         As Long = &H10B&
Private Const IMAGE_FILE_RELOCS_STRIPPED            As Long = &H1
Private Const IMAGE_FILE_EXECUTABLE_IMAGE           As Long = &H2
Private Const IMAGE_FILE_32BIT_MACHINE              As Long = &H100
Private Const IMAGE_DIRECTORY_ENTRY_IMPORT          As Long = 1
Private Const IMAGE_DIRECTORY_ENTRY_BASERELOC       As Long = 5
Private Const IMAGE_SCN_MEM_EXECUTE                 As Long = &H20000000
Private Const IMAGE_SCN_MEM_READ                    As Long = &H40000000
Private Const IMAGE_SCN_MEM_WRITE                   As Long = &H80000000
Private Const IMAGE_REL_BASED_HIGHLOW               As Long = 3
Private Const HEAP_NO_SERIALIZE                     As Long = &H1
Private Const STATUS_SUCCESS                        As Long = 0
Private Const STATUS_INFO_LENGTH_MISMATCH           As Long = &HC0000004
Private Const ProcessBasicInformation               As Long = 0

'Private Declare Function SysAllocStringByteLen Lib "oleaut32.dll" (ByVal pszStrPtr As Long, ByVal length As Long) As Long


' // Obtain string from resource (it should be less or equal MAX_PATH)
Public Function ResGetString( _
                ByVal id As MessagesID) As Long
        
    Dim hInstance As Long
    
    ResGetString = llib.SysAllocStringLen(0, MAX_PATH)
    
    If ResGetString Then
        
        hInstance = llib.GetModuleHandle(ByVal 0&)
    
        If llib.LoadString(hInstance, id, ResGetString, MAX_PATH) = 0 Then llib.SysFreeString ResGetString: ResGetString = 0: Exit Function
        If llib.SysReAllocString(ResGetString, ResGetString) = 0 Then llib.SysFreeString ResGetString: ResGetString = 0: Exit Function
        
    End If
    
    'Dbg "ID: " & ID & ", Result: " & GetString
    
End Function

' // Run exe from project in memory
Public Function RunExeFromMemory(pFileInMemory As Long, dwSize As Long) As Boolean
    Dim pFileData   As Long
    
    ' // Alloc memory within top memory addresses
    pFileData = llib.VirtualAlloc(ByVal 0&, dwSize, MEM_TOP_DOWN Or MEM_COMMIT, PAGE_READWRITE)
    If pFileData = 0 Then Exit Function
    
    ' // Copy raw exe file to this memory
    llib.CopyMemory ByVal pFileData, ByVal pFileInMemory, dwSize
    
    ' // Free decompressed project data
    'HeapFree GetProcessHeap(), HEAP_NO_SERIALIZE, pProjectData
    'pProjectData = 0
    
    Dbg "pFileData = " & pFileData
    
    ' // Run exe from memory
    RunExeFromMemory = RunExeFromMemoryEx(pFileData, True)
    
    ' ----------------------------------------------------
    ' // An error occurs
    ' // Clean memory
    
    llib.VirtualFree ByVal pFileData, 0, MEM_RELEASE
    
End Function

' // Run EXE file by memory address
Private Function RunExeFromMemoryEx( _
                ByVal pExeData As Long, _
                ByVal IgnoreError As Boolean) As Boolean
    Dim Length  As Long:    Dim pCode       As Long
    Dim pszMsg  As Long:    Dim pMsgTable   As Long
    Dim Index   As Long:    Dim pCurMsg     As Long
    
    ' // Get size of shellcode
    Length = GetAddr(AddressOf ENDSHELLLOADER) - GetAddr(AddressOf BEGINSHELLLOADER)
    
    ' // Alloc memory within top addresses
    pCode = llib.VirtualAlloc(ByVal 0&, Length, MEM_TOP_DOWN Or MEM_COMMIT, PAGE_EXECUTE_READWRITE)
    
    ' // Copy shellcode to allocated memory
    llib.CopyMemory ByVal pCode, ByVal GetAddr(AddressOf BEGINSHELLLOADER), Length
    
    Dbg "pCode = " & pCode
    
    Dbg "InitShellLoader"
    
    ' // Initialization of shellcode
    If Not InitShellLoader(pCode) Then GoTo CleanUp
    
    Dbg "Splice"
    
    ' // Splice CallLoader function in order to call shellcode
    Splice AddressOf CallLoader, pCode + GetAddr(AddressOf LoadExeFromMemory) - GetAddr(AddressOf BEGINSHELLLOADER)
    
    ' // Check ignore errors
    If Not IgnoreError Then
        
        Dbg "VirtualAlloc"
        
        ' // Alloc memory for messages table
        pMsgTable = llib.VirtualAlloc(ByVal 0&, 1024, MEM_TOP_DOWN Or MEM_COMMIT, PAGE_READWRITE)
        If pMsgTable = 0 Then GoTo CleanUp
        
        ' // Skip pointers
        pCurMsg = pMsgTable + EM_END * 4
        
        For Index = 0 To EM_END - 1
        
            Dbg "GetString" & Index
        
            ' // Load message string
            pszMsg = ResGetString(MSG_LOADER_ERROR + Index)
            If pszMsg = 0 Then GoTo CleanUp
            
            Length = llib.SysStringLen(pszMsg)

            llib.lstrcpyn ByVal pCurMsg, ByVal pszMsg, Length + 1
            
            ' // Store pointer
            llib.CopyMemory ByVal pMsgTable + Index * 4, pCurMsg, Len(pCurMsg)
            
            ' // Next message offset
            pCurMsg = pCurMsg + (Length + 1) * 2
            
            llib.SysFreeString pszMsg
            
        Next
        
    End If
    
    Dbg "CallLoader: pExeData = " & CStr(pExeData)
    
    ' // Call shellcode
    CallLoader pExeData, pCode, pMsgTable
    
CleanUp:
    
    If pMsgTable Then
        llib.VirtualFree ByVal pMsgTable, 0, MEM_RELEASE
    End If
    
    If pCode Then
        llib.VirtualFree ByVal pCode, 0, MEM_RELEASE
    End If
    
End Function

' // Shellcode initialization
Private Function InitShellLoader( _
                 ByVal pShellCode As Long) As Boolean
    Dim hLib    As Long:        Dim sName   As Long
    Dim sFunc   As Long:        Dim lpAddr  As Long
    Dim libIdx  As Long:        Dim fncIdx  As Long
    Dim libName As MessagesID   ':  Dim fncName As MessagesID
    Dim fncSpc  As Long:        Dim splAddr As Long
    
    ' // +----------------------------------------------------------------+
    ' // |                  Fixing of API addresses                       |
    ' // +----------------------------------------------------------------+
    ' // | In order to call api function from shellcode i use splicing of |
    ' // |    our VB functions and redirect call to corresponding api.    |
    ' // |     I did same in the code that injects to other process.      |
    ' // +----------------------------------------------------------------+
    
    splAddr = GetAddr(AddressOf tVirtualAlloc) - GetAddr(AddressOf BEGINSHELLLOADER) + pShellCode
    
    ' // Get size in bytes between stub functions
    fncSpc = GetAddr(AddressOf tVirtualProtect) - GetAddr(AddressOf tVirtualAlloc)

    ' // Use 3 library: kernel32, ntdll è user32
    For libIdx = 0 To 2
    
        ' // Get number of imported functions depending on library
        Select Case libIdx
        Case 0: libName = API_LIB_KERNEL32: fncIdx = 13
        Case 1: libName = API_LIB_NTDLL:    fncIdx = 1
        Case 2: libName = API_LIB_USER32:   fncIdx = 1
        End Select
        
        ' // Get library name from resources
        sName = ResGetString(libName): If sName = 0 Then Exit Function
        
        Dbg "Get module handle"
        
        ' // Get module handle
        hLib = llib.GetModuleHandle(ByVal sName): If hLib = 0 Then Exit Function
        llib.SysFreeString sName
        
        ' // Go thru functions
        Do While fncIdx
        
            libName = libName + 1
            ' // Get function name
            sName = ResGetString(libName): If sName = 0 Then Exit Function
            
            ' // Because of GetProcAddress works with ANSI string translate it to ANSI
            sFunc = ToAnsi(sName): If sFunc = 0 Then Exit Function
            
            ' // Get function address
            lpAddr = llib.GetProcAddress(hLib, sFunc)
            llib.SysFreeString sName: llib.SysFreeString sFunc
            
            Dbg "Addr of function: " & libName & " is " & lpAddr
            
            ' // Error
            If lpAddr = 0 Then Exit Function
            
            ' // Splice stub
            Splice splAddr, lpAddr
            
            ' // Next stub
            splAddr = splAddr + fncSpc
            fncIdx = fncIdx - 1
            
        Loop
        
    Next
    
    Dbg "Modify CallByPointer"
    
    ' // Modify CallByPointer
    lpAddr = GetAddr(AddressOf CallByPointer) - GetAddr(AddressOf BEGINSHELLLOADER) + pShellCode
    
    ' // pop eax    - 0x58
    ' // pop ecx    - 0x59
    ' // push eax   - 0x50
    ' // jmp ecx    - 0xFFE1
    
    llib.CopyMemory ByVal lpAddr, &HFF505958, 4
    llib.CopyMemory ByVal lpAddr + 4, &HE1, 1

    ' // Success
    InitShellLoader = True
    
End Function

' // Splice function
Private Sub Splice( _
            ByVal Func As Long, _
            ByVal NewAddr As Long)
    ' // Set memory permissions
    llib.VirtualProtect ByVal Func, 5, PAGE_EXECUTE_READWRITE, 0
    llib.CopyMemory ByVal Func, &HE9, 1                      ' // JMP
    llib.CopyMemory ByVal Func + 1, NewAddr - Func - 5, 4    ' // Relative address
End Sub

' // Unicode->Ansi
Private Function ToAnsi( _
                 ByVal s As Long) As Long
    Dim Size As Long
    
    ' // Get string size
    Size = llib.SysStringLen(s)
    
    ' // Alloc memory for ansi string
    ToAnsi = llib.SysAllocStringByteLen(0, Size)
    
    ' // Translate
    llib.WideCharToMultiByte CP_ACP, 0, s, Size, ToAnsi, Size, 0, 0
    
End Function

' // Stub for calling shellcode
Private Function CallLoader( _
                 ByVal Pointer As Long, _
                 ByVal MyBaseAddress As Long, _
                 ByVal ErrMsgTable As Long) As Boolean
    CallLoader = 1
End Function

' // Begin of shellcode
Private Function BEGINSHELLLOADER() As Integer: End Function

' // Parse exe in memory
Private Function LoadExeFromMemory( _
                 ByVal pRawData As Long, _
                 ByVal pMyBaseAddress As Long, _
                 ByVal pErrMsgTable As Long) As Boolean
    Dim NtHdr   As IMAGE_NT_HEADERS
    Dim pBase   As Long
    'Dim Index   As Long
    Dim iError  As ERROR_MESSAGES
    'Dim pszMsg  As Long
    
    ' // Get IMAGE_NT_HEADERS
    If GetImageNtHeaders(pRawData, NtHdr) = 0 Then
        iError = EM_UNABLE_TO_GET_NT_HEADERS
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Check flags
    If NtHdr.FileHeader.Machine <> IMAGE_FILE_MACHINE_I386 Or _
       (NtHdr.FileHeader.Characteristics And IMAGE_FILE_EXECUTABLE_IMAGE) = 0 Or _
       (NtHdr.FileHeader.Characteristics And IMAGE_FILE_32BIT_MACHINE) = 0 Then Exit Function

    ' // Release main EXE memory. After that main exe is unloaded from memory.
    'llib.ZwUnmapViewOfSection llib.GetCurrentProcess(), llib.GetModuleHandle(ByVal 0&)

    ' // Reserve memory for EXE
    iError = ReserveMemory(pRawData, pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Place data
    iError = ProcessSectionsAndHeaders(pRawData, pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Update new base address
    iError = UpdateNewBaseAddress(pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Import table processing
    iError = ProcessImportTable(pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Relocations processing
    iError = ProcessRelocations(pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Set the memory attributes
    iError = SetMemoryPermissions(pBase)
    If iError Then
        EndProcess pErrMsgTable, iError
        Exit Function
    End If
    
    ' // Release error message table
    If pErrMsgTable Then
        tVirtualFree pErrMsgTable, 0, MEM_RELEASE
    End If
    
    ' // Call entry point
    CallByPointer NtHdr.OptionalHeader.AddressOfEntryPoint + pBase
    
    ' // End process
    EndProcess
    
End Function

' // Update new base address
Private Function UpdateNewBaseAddress( _
                 ByVal pBase As Long) As ERROR_MESSAGES
    Dim pPBI    As Long:                        Dim PBIlen  As Long
    Dim PBI     As PROCESS_BASIC_INFORMATION:   Dim cPEB    As PEB
    Dim ntstat  As Long
    Dim ldrData As PEB_LDR_DATA
    Dim ldrMod  As LDR_MODULE
    
    ntstat = tNtQueryInformationProcess(tGetCurrentProcess(), ProcessBasicInformation, IntPtr(PBI.ExitStatus), Len(PBI), PBIlen)
    
    Do While ntstat = STATUS_INFO_LENGTH_MISMATCH
        
        PBIlen = PBIlen * 2
        
        If pPBI Then
            tHeapFree tGetProcessHeap(), HEAP_NO_SERIALIZE, pPBI
        End If
        
        pPBI = tHeapAlloc(tGetProcessHeap(), HEAP_NO_SERIALIZE, PBIlen)
        ntstat = tNtQueryInformationProcess(tGetCurrentProcess(), ProcessBasicInformation, pPBI, PBIlen, PBIlen)
        
    Loop
    
    If ntstat <> STATUS_SUCCESS Then
        UpdateNewBaseAddress = EM_PROCESS_INFORMATION_NOT_FOUND
        GoTo CleanUp
    End If
    
    If pPBI Then
        ' // Copy to PROCESS_BASIC_INFORMATION
        tCopyMemory IntPtr(PBI.ExitStatus), pPBI, Len(PBI)
    End If

    ' // Get PEB
    tCopyMemory IntPtr(cPEB.NotUsed), PBI.PebBaseAddress, Len(cPEB)
    
    ' // Modify image base
    cPEB.ImageBaseAddress = pBase
    
    ' // Restore PEB
    tCopyMemory PBI.PebBaseAddress, IntPtr(cPEB.NotUsed), Len(cPEB)
    
    ' // Fix base address in PEB_LDR_DATA list
    tCopyMemory IntPtr(ldrData.Length), cPEB.LoaderData, Len(ldrData)
    
    ' // Get first element
    tCopyMemory IntPtr(ldrMod.InLoadOrderModuleList.Flink), ldrData.InLoadOrderModuleList.Flink, Len(ldrMod)
    
    ' // Fix base
    ldrMod.BaseAddress = pBase
    
    ' // Restore
    tCopyMemory ldrData.InLoadOrderModuleList.Flink, IntPtr(ldrMod.InLoadOrderModuleList.Flink), Len(ldrMod)
    
CleanUp:
    
    ' // Free memory
    If pPBI Then
        tHeapFree tGetProcessHeap(), HEAP_NO_SERIALIZE, pPBI
    End If
    
End Function

' // Set memory permissions
Private Function SetMemoryPermissions( _
                 ByVal pBase As Long) As ERROR_MESSAGES
    Dim iSec    As Long:                    Dim pNtHdr  As Long
    Dim NtHdr   As IMAGE_NT_HEADERS:        Dim sec     As IMAGE_SECTION_HEADER
    Dim Attr    As MEMPROTECT:              Dim pSec    As Long
    Dim ret     As Long
    
    pNtHdr = GetImageNtHeaders(pBase, NtHdr)
    If pNtHdr = 0 Then
        SetMemoryPermissions = EM_UNABLE_TO_GET_NT_HEADERS
        Exit Function
    End If

    ' // Get address of first section header
    pSec = pNtHdr + 4 + Len(NtHdr.FileHeader) + NtHdr.FileHeader.SizeOfOptionalHeader
    
    ' // Go thru section headers
    For iSec = 0 To NtHdr.FileHeader.NumberOfSections - 1
    
        ' // Copy section descriptor
        tCopyMemory IntPtr(sec.SectionName(0)), pSec, Len(sec)
        
        ' // Get type
        If sec.Characteristics And IMAGE_SCN_MEM_EXECUTE Then
            If sec.Characteristics And IMAGE_SCN_MEM_READ Then
                If sec.Characteristics And IMAGE_SCN_MEM_WRITE Then
                    Attr = PAGE_EXECUTE_READWRITE
                Else
                    Attr = PAGE_EXECUTE_READ
                End If
            Else
                If sec.Characteristics And IMAGE_SCN_MEM_WRITE Then
                    Attr = PAGE_EXECUTE_WRITECOPY
                Else
                    Attr = PAGE_EXECUTE
                End If
            End If
        Else
            If sec.Characteristics And IMAGE_SCN_MEM_READ Then
                If sec.Characteristics And IMAGE_SCN_MEM_WRITE Then
                    Attr = PAGE_READWRITE
                Else
                    Attr = PAGE_READONLY
                End If
            Else
                If sec.Characteristics And IMAGE_SCN_MEM_WRITE Then
                    Attr = PAGE_WRITECOPY
                Else
                    Attr = PAGE_NOACCESS
                End If
            End If
        End If
        
        ' // Set memory permissions
        If tVirtualProtect(sec.VirtualAddress + pBase, sec.VirtualSize, Attr, IntPtr(ret)) = 0 Then
            SetMemoryPermissions = EM_UNABLE_TO_PROTECT_MEMORY
            Exit Function
        End If
        
        ' // Next section
        pSec = pSec + Len(sec)
        
    Next
    
End Function

' // Process import table
Private Function ProcessImportTable( _
                 ByVal pBase As Long) As ERROR_MESSAGES
    Dim NtHdr           As IMAGE_NT_HEADERS:        Dim datDirectory    As IMAGE_DATA_DIRECTORY
    Dim dsc             As IMAGE_IMPORT_DESCRIPTOR: Dim hLib            As Long
    Dim thnk            As Long:                    Dim Addr            As Long
    Dim fnc             As Long:                    Dim pData           As Long
        
    If GetImageNtHeaders(pBase, NtHdr) = 0 Then
        ProcessImportTable = EM_UNABLE_TO_GET_NT_HEADERS
        Exit Function
    End If
    
    ' // Import table processing
    If NtHdr.OptionalHeader.NumberOfRvaAndSizes > 1 Then
        
        If GetDataDirectory(pBase, IMAGE_DIRECTORY_ENTRY_IMPORT, datDirectory) = 0 Then
            ProcessImportTable = EM_INVALID_DATA_DIRECTORY
            Exit Function
        End If

        ' // If import table exists
        If datDirectory.Size > 0 And datDirectory.VirtualAddress > 0 Then
        
            ' // Copy import descriptor
            pData = datDirectory.VirtualAddress + pBase
            tCopyMemory IntPtr(dsc.Characteristics), pData, Len(dsc)
            
            ' // Go thru all descriptors
            Do Until dsc.Characteristics = 0 And _
                     dsc.FirstThunk = 0 And _
                     dsc.ForwarderChain = 0 And _
                     dsc.pName = 0 And _
                     dsc.TimeDateStamp = 0
                
                If dsc.pName > 0 Then
                
                    ' // Load needed library
                    hLib = tLoadLibrary(dsc.pName + pBase)
                    
                    If hLib = 0 Then
                        ProcessImportTable = EM_LOADLIBRARY_FAILED
                        Exit Function
                    End If

                    If dsc.Characteristics Then fnc = dsc.Characteristics + pBase Else fnc = dsc.FirstThunk + pBase
                    
                    ' // Go to names table
                    tCopyMemory IntPtr(thnk), fnc, 4
                    
                    ' // Go thru names table
                    Do While thnk
                    
                        ' // Check import type
                        If thnk < 0 Then
                            ' // By ordinal
                            Addr = tGetProcAddress(hLib, thnk And &HFFFF&)
                        Else
                            ' // By name
                            Addr = tGetProcAddress(hLib, thnk + 2 + pBase)
                        End If
                        
                        ' // Next function
                        fnc = fnc + 4
                        tCopyMemory IntPtr(thnk), fnc, 4
                        tCopyMemory dsc.FirstThunk + pBase, IntPtr(Addr), 4
                        dsc.FirstThunk = dsc.FirstThunk + 4
                        
                    Loop
                    
                End If
                
                ' // Next descriptor
                pData = pData + Len(dsc)
                tCopyMemory IntPtr(dsc.Characteristics), pData, Len(dsc)
                
            Loop
            
        End If
        
    End If
                 
End Function

' // Process relocations
Private Function ProcessRelocations( _
                 ByVal pBase As Long) As ERROR_MESSAGES
    Dim NtHdr           As IMAGE_NT_HEADERS:        Dim datDirectory    As IMAGE_DATA_DIRECTORY
    Dim relBase         As IMAGE_BASE_RELOCATION:   Dim entriesCount    As Long
    Dim relType         As Long:                    Dim dwAddress       As Long
    Dim dwOrig          As Long:                    Dim pRelBase        As Long
    Dim delta           As Long:                    Dim pData           As Long
    
    ' // Check if module has not been loaded to image base value
    If GetImageNtHeaders(pBase, NtHdr) = 0 Then
        ProcessRelocations = EM_UNABLE_TO_GET_NT_HEADERS
        Exit Function
    End If
    
    delta = pBase - NtHdr.OptionalHeader.ImageBase
    
    ' // Process relocations
    If delta Then
        
        ' // Get address of relocation table
        If GetDataDirectory(pBase, IMAGE_DIRECTORY_ENTRY_BASERELOC, datDirectory) = 0 Then
            ProcessRelocations = EM_INVALID_DATA_DIRECTORY
            Exit Function
        End If
        
        If datDirectory.Size > 0 And datDirectory.VirtualAddress > 0 Then
        
            ' // Copy relocation base
            pRelBase = datDirectory.VirtualAddress + pBase
            tCopyMemory IntPtr(relBase.VirtualAddress), pRelBase, Len(relBase)
            
            Do While relBase.VirtualAddress
            
                ' // To first reloc chunk
                pData = pRelBase + Len(relBase)
                
                entriesCount = (relBase.SizeOfBlock - Len(relBase)) \ 2
                
                Do While entriesCount > 0
                    
                    tCopyMemory IntPtr(relType), pData, 2
                    
                    Select Case (relType \ 4096) And &HF
                    Case IMAGE_REL_BASED_HIGHLOW
                        
                        ' // Calculate address
                        dwAddress = relBase.VirtualAddress + (relType And &HFFF&) + pBase
                        
                        ' // Get original address
                        tCopyMemory IntPtr(dwOrig), dwAddress, Len(dwOrig)
                        
                        ' // Add delta
                        dwOrig = dwOrig + delta
                        
                        ' // Save
                        tCopyMemory dwAddress, IntPtr(dwOrig), Len(dwOrig)
                        
                    End Select
                    
                    pData = pData + 2
                    entriesCount = entriesCount - 1
                    
                Loop
                
                ' // Next relocation base
                pRelBase = pRelBase + relBase.SizeOfBlock
                tCopyMemory IntPtr(relBase.VirtualAddress), pRelBase, Len(relBase)
                
            Loop
            
        End If
        
    End If

End Function

' // Reserve memory for EXE
Private Function ReserveMemory( _
                 ByVal pRawExeData As Long, _
                 ByRef pBase As Long) As ERROR_MESSAGES
    Dim NtHdr       As IMAGE_NT_HEADERS
    Dim pLocBase    As Long
    
    If GetImageNtHeaders(pRawExeData, NtHdr) = 0 Then
        ReserveMemory = EM_UNABLE_TO_GET_NT_HEADERS
        Exit Function
    End If
    
    ' // Reserve memory for EXE
    pLocBase = tVirtualAlloc(ByVal NtHdr.OptionalHeader.ImageBase, _
                          NtHdr.OptionalHeader.SizeOfImage, _
                          MEM_RESERVE, PAGE_EXECUTE_READWRITE)
    If pLocBase = 0 Then
        
        ' // If relocation information not found error
        If NtHdr.FileHeader.Characteristics And IMAGE_FILE_RELOCS_STRIPPED Then
        
            ReserveMemory = EM_UNABLE_TO_ALLOCATE_MEMORY
            Exit Function
            
        Else
            ' // Reserve memory in other region
            pLocBase = tVirtualAlloc(ByVal 0&, NtHdr.OptionalHeader.SizeOfImage, _
                                 MEM_RESERVE, PAGE_EXECUTE_READWRITE)
            
            If pLocBase = 0 Then
            
                ReserveMemory = EM_UNABLE_TO_ALLOCATE_MEMORY
                Exit Function
                
            End If

        End If
        
    End If
    
    pBase = pLocBase
    
End Function

' // Allocate memory for sections and copy them data to there
Private Function ProcessSectionsAndHeaders( _
                 ByVal pRawExeData As Long, _
                 ByVal pBase As Long) As ERROR_MESSAGES

    Dim iSec    As Long
    Dim pNtHdr  As Long
    Dim NtHdr   As IMAGE_NT_HEADERS
    Dim sec     As IMAGE_SECTION_HEADER
    Dim lpSec   As Long
    Dim pData   As Long
    
    pNtHdr = GetImageNtHeaders(pRawExeData, NtHdr)
    If pNtHdr = 0 Then
        ProcessSectionsAndHeaders = EM_UNABLE_TO_GET_NT_HEADERS
        Exit Function
    End If
    
    ' // Alloc memory for headers
    pData = tVirtualAlloc(ByVal pBase, NtHdr.OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE)
    If pData = 0 Then
        ProcessSectionsAndHeaders = EM_UNABLE_TO_ALLOCATE_MEMORY
        Exit Function
    End If
    
    ' // Copy headers
    tCopyMemory pData, pRawExeData, NtHdr.OptionalHeader.SizeOfHeaders
    
    ' // Get address of beginnig of sections headers
    pData = pNtHdr + Len(NtHdr.Signature) + Len(NtHdr.FileHeader) + NtHdr.FileHeader.SizeOfOptionalHeader
    
    ' // Go thru sections
    For iSec = 0 To NtHdr.FileHeader.NumberOfSections - 1
    
        ' // Copy section descriptor
        tCopyMemory IntPtr(sec.SectionName(0)), pData, Len(sec)
        
        ' // Alloc memory for section
        lpSec = tVirtualAlloc(sec.VirtualAddress + pBase, sec.VirtualSize, MEM_COMMIT, PAGE_READWRITE)
        If lpSec = 0 Then
            ProcessSectionsAndHeaders = EM_UNABLE_TO_ALLOCATE_MEMORY
            Exit Function
        End If
        
        ' If there is initialized data
        If sec.SizeOfRawData Then
        
            ' // Take into account  file alignment
            If sec.SizeOfRawData > sec.VirtualSize Then sec.SizeOfRawData = sec.VirtualSize
            
            ' // Copy initialized data to section
            tCopyMemory lpSec, pRawExeData + sec.PointerToRawData, sec.SizeOfRawData
            lpSec = lpSec + sec.SizeOfRawData
            sec.VirtualSize = sec.VirtualSize - sec.SizeOfRawData
            
        End If

        ' // Fill remain part with zero
        tFillMemory lpSec, sec.VirtualSize, 0
        
        ' // Next section
        pData = pData + Len(sec)
        
    Next
    
End Function

' // Get NT headers and return its address
Private Function GetImageNtHeaders( _
                 ByVal pBase As Long, _
                 ByRef pNtHeaders As IMAGE_NT_HEADERS) As Long
    Dim dosHdr  As IMAGE_DOS_HEADER
    Dim NtHdr   As IMAGE_NT_HEADERS
    Dim pNtHdr  As Long
    
    ' // Get DOS header
    tCopyMemory IntPtr(dosHdr.e_magic_e_cblp), pBase, Len(dosHdr)
    
    ' // Check MZ signature and alignment
    If (dosHdr.e_magic_e_cblp And &HFFFF&) <> IMAGE_DOS_SIGNATURE Or _
       (dosHdr.e_lfanew And &H3) <> 0 Then
        Exit Function
    End If
    
    ' // Get pointer to NT headers
    pNtHdr = pBase + dosHdr.e_lfanew
    
    ' // Get NT headers
    tCopyMemory IntPtr(NtHdr.Signature), pNtHdr, Len(NtHdr)
    
    ' // Check NT signature
    If (NtHdr.Signature <> IMAGE_NT_SIGNATURE) Or _
        NtHdr.OptionalHeader.Magic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC Or _
        NtHdr.FileHeader.SizeOfOptionalHeader <> Len(NtHdr.OptionalHeader) Then
        Exit Function
    End If
    
    tCopyMemory IntPtr(pNtHeaders.Signature), IntPtr(NtHdr.Signature), Len(NtHdr)
    GetImageNtHeaders = pNtHdr
    
End Function

' // Get data directory and return its data
Private Function GetDataDirectory( _
                 ByVal pBase As Long, _
                 ByVal lIndex As Long, _
                 ByRef pDirectory As IMAGE_DATA_DIRECTORY) As Long
    Dim NtHdr   As IMAGE_NT_HEADERS
    Dim pNtHdr  As Long
    
    
    ' // Get NT headers
    pNtHdr = GetImageNtHeaders(pBase, NtHdr)
    If pNtHdr = 0 Then
        Exit Function
    End If
    
    ' // Check directory index
    If lIndex < 0 Or lIndex >= NtHdr.OptionalHeader.NumberOfRvaAndSizes Then
        Exit Function
    End If
    
    ' // Copy directory data
    tCopyMemory IntPtr(pDirectory.VirtualAddress), IntPtr(NtHdr.OptionalHeader.DataDirectory(lIndex).VirtualAddress), Len(pDirectory)
    GetDataDirectory = pNtHdr + Len(NtHdr.Signature) + Len(NtHdr.FileHeader) + &H60 + lIndex * Len(pDirectory)
    
End Function

' // Error message
Private Sub EndProcess( _
            Optional ByVal pMsgTable As Long = 0, _
            Optional ByVal lMsgNumber As Long = 0)
    
    Dim pszMsg As Long
    
    If pMsgTable Then
        ' // Get message offset
        tCopyMemory IntPtr(pszMsg), pMsgTable + lMsgNumber * 4, 4
        ' // Show message box
        tMessageBox 0, pszMsg, 0, MB_ICONERROR
        
    End If
    
    tExitProcess 0
    
End Sub

' // Call function by pointer
Private Sub CallByPointer( _
            ByVal pFuncAddress As Long)
    
End Sub

' // Stubs for API calling
Private Function tVirtualAlloc( _
                 ByVal lpAddress As Long, _
                 ByVal dwSize As Long, _
                 ByVal flAllocationType As ALLOCATIONTYPE, _
                 ByVal flProtect As MEMPROTECT) As Long
    tVirtualAlloc = 2
End Function
Private Function tVirtualProtect( _
                 ByVal lpAddress As Long, _
                 ByVal dwSize As Long, _
                 ByVal flNewProtect As MEMPROTECT, _
                 ByVal flOldProtect As MEMPROTECT) As Long
    tVirtualProtect = 3
End Function
Private Function tVirtualFree( _
                 ByVal lpAddress As Long, _
                 ByVal dwSize As Long, _
                 ByVal dwFreeType As FREETYPE) As Long
    tVirtualFree = 4
End Function
Private Function tCopyMemory( _
                 ByVal lpDst As Long, _
                 ByVal lpSrc As Long, _
                 ByVal Size As Long) As Long
    tCopyMemory = 5
End Function
Private Function tFillMemory( _
                 ByVal lpDst As Long, _
                 ByVal dwSize As Long, _
                 ByVal Char As Byte) As Long
    tFillMemory = 6
End Function
Private Function tlstrcpyn( _
                 ByRef lpString1 As Long, _
                 ByRef lpString2 As Long, _
                 ByVal iMaxLength As Long) As Long
    tlstrcpyn = 7
End Function
Private Function tLoadLibrary( _
                 ByVal lpFileName As Long) As Long
    tLoadLibrary = 8
End Function
Private Function tGetProcAddress( _
                 ByVal hModule As Long, _
                 ByVal lpProcName As Long) As Long
    tGetProcAddress = 9
End Function
Private Function tExitProcess( _
                 ByVal uExitCode As Long) As Long
    tExitProcess = 10
End Function
Private Function tHeapAlloc( _
                 ByVal hHeap As Long, _
                 ByVal dwFlags As Long, _
                 ByVal dwBytes As Long) As Long
    tHeapAlloc = 11
End Function
Private Function tHeapFree( _
                 ByVal hHeap As Long, _
                 ByVal dwFlags As Long, _
                 ByVal lpMem As Long) As Long
    tHeapFree = 12
End Function
Private Function tGetProcessHeap() As Long
    tGetProcessHeap = 13
End Function
Private Function tGetCurrentProcess() As Long
    tGetCurrentProcess = 14
End Function
Private Function tNtQueryInformationProcess( _
                 ByVal ProcessHandle As Long, _
                 ByVal InformationClass As Long, _
                 ByVal ProcessInformation As Long, _
                 ByVal ProcessInformationLength As Long, _
                 ByRef ReturnLength As Long) As Long
    tNtQueryInformationProcess = 16
End Function
Private Function tMessageBox( _
                 ByVal hwnd As Long, _
                 ByVal lpText As Long, _
                 ByVal lpCaption As Long, _
                 ByVal uType As MESSAGEBOXCONSTANTS) As MESSAGEBOXRETURN
    tMessageBox = 17
End Function

' // VarPtr analog
Private Function IntPtr( _
                 ByRef Value As Long) As Long
    IntPtr = tlstrcpyn(Value, 0, 0)
End Function

' // Get AddressOf
Private Function GetAddr( _
                 ByVal Addr As Long) As Long
    GetAddr = Addr: Exit Function
End Function

' // End of shellcode
Private Function ENDSHELLLOADER() As Long: End Function

