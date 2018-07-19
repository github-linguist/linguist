; By ABCza, http://www.autohotkey.com/board/topic/76170-function-send-windows-log-events/
h := RegisterForEvents("AutoHotkey")
SendWinLogEvent(h, "Test Message")
DeregisterForEvents(h)

/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: SendWinLogEvent
--------------------------------------------------------------------------------------------------------------------------------
Writes an entry at the end of the specified Windows event log. Returns nonzero if the function succeeds or zero if it fails.

PARAMETERS:
~~~~~~~~~~~
hSource		- Handle to a previously registered events source with RegisterForEvents.
evType		- EVENTLOG_SUCCESS			:= 0x0000
			  EVENTLOG_AUDIT_FAILURE	:= 0x0010
			  EVENTLOG_AUDIT_SUCCESS	:= 0x0008
			  EVENTLOG_ERROR_TYPE		:= 0x0001
			  EVENTLOG_INFORMATION_TYPE	:= 0x0004
			  EVENTLOG_WARNING_TYPE		:= 0x0002
evId		- Event ID, can be any dword value.
evCat		- Any value, used to organize events in categories.
pStrings	- A continuation section with newline separated strings (each max 31839 chars).
pData		- A buffer containing the binary data.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
ReportEvent										- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363679(v=vs.85).aspx
Event Identifiers								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363651(v=vs.85).aspx
Event categories								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363649(v=vs.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
SendWinLogEvent(hSource, String="", evType=0x0004, evId=0x03EA, evCat=0, pData=0) {
	Ptr := A_PtrSize ? "Ptr" : "UInt"
	StringPut := A_IsUnicode ? "StrPut" : "StrPut2"

	; Reserve and initialise space for the event message.
	VarSetCapacity(eventMessage, StrLen(String), 0)
	%StringPut%(String, eventMessage)

	r := DllCall("Advapi32.dll\ReportEvent" (A_IsUnicode ? "W" : "A")
		, UInt, hSource			; handle
		, UShort, evType		; WORD, eventlog_information_type
		, UShort, evCat			; WORD, category
		, UInt, evId			; DWORD, event ID, 0x03EA
		, Ptr, 0				; PSID, ptr to user security ID
		, UShort, 1				; WORD, number of strings
		, UInt, VarSetCapacity(pData)					; DWORD, data size
		, Ptr, &eventMessage							; LPCTSTR*, ptr to a buffer ...
		, Ptr, (VarSetCapacity(pData)) ? &pData : 0 )	; ptr to a buffer of binary data
	
	; Release memory.
	VarSetCapacity(eventMessage, 0)
	
	Return r
}
/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: RegisterForEvents
--------------------------------------------------------------------------------------------------------------------------------
Registers the application to send Windows log events. Returns a handle to the registered source.

PARAMETERS:
~~~~~~~~~~~
logName	 - Can be "Application", "System" or a custom log name.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
RegisterEventSource							- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363678(v=VS.85).aspx
Event Sources								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363661(v=VS.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
RegisterForEvents(logName) {
	Return DllCall("Advapi32.dll\RegisterEventSource" (A_IsUnicode ? "W" : "A")
		, UInt, 0				; LPCTSTR, Local computer
		, Str, logName)			; LPCTSTR Source name
}
/*
--------------------------------------------------------------------------------------------------------------------------------
FUNCTION: DeregisterForEvents
--------------------------------------------------------------------------------------------------------------------------------
Deregisters the previously registered application.

PARAMETERS:
~~~~~~~~~~~
hSource	 - Handle to a registered source.
--------------------------------------------------------------------------------------------------------------------------------
SYSTEM CALLS, STRUCTURES AND INFO:
--------------------------------------------------------------------------------------------------------------------------------
DeregisterEventSource						- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363642(v=vs.85).aspx
Event Sources								- http://msdn.microsoft.com/en-us/library/windows/desktop/aa363661(v=VS.85).aspx
--------------------------------------------------------------------------------------------------------------------------------
*/
DeregisterForEvents(hSource) {
	IfNotEqual, hSource, 0, Return DllCall( "Advapi32.dll\DeregisterEventSource"
		, UInt, hSource )
}

; StrPut for AutoHotkey Basic
StrPut2(String, Address="", Length=-1, Encoding=0)
{
	; Flexible parameter handling:
	if Address is not integer		 ; StrPut(String [, Encoding])
		Encoding := Address,	Length := 0,	Address := 1024
	else if Length is not integer	 ; StrPut(String, Address, Encoding)
		Encoding := Length,	Length := -1
	
	; Check for obvious errors.
	if (Address+0 < 1024)
		return
	
	; Ensure 'Encoding' contains a numeric identifier.
	if Encoding = UTF-16
		Encoding = 1200
	else if Encoding = UTF-8
		Encoding = 65001
	else if SubStr(Encoding,1,2)="CP"
		Encoding := SubStr(Encoding,3)
	
	if !Encoding ; "" or 0
	{
		; No conversion required.
		char_count := StrLen(String) + 1 ; + 1 because generally a null-terminator is wanted.
		if (Length)
		{
			; Check for sufficient buffer space.
			if (StrLen(String) <= Length || Length == -1)
			{
				if (StrLen(String) == Length)
					; Exceptional case: caller doesn't want a null-terminator.
					char_count--
				; Copy the string, including null-terminator if requested.
				DllCall("RtlMoveMemory", "uint", Address, "uint", &String, "uint", char_count)
			}
			else
				; For consistency with the sections below, don't truncate the string.
				char_count = 0
		}
		;else: Caller just wants the the required buffer size (char_count), which will be returned below.
	}
	else if Encoding = 1200 ; UTF-16
	{
		; See the 'else' to this 'if' below for comments.
		if (Length <= 0)
		{
			char_count := DllCall("MultiByteToWideChar", "uint", 0, "uint", 0, "uint", &String, "int", StrLen(String), "uint", 0, "int", 0) + 1
			if (Length == 0)
				return char_count
			Length := char_count
		}
		char_count := DllCall("MultiByteToWideChar", "uint", 0, "uint", 0, "uint", &String, "int", StrLen(String), "uint", Address, "int", Length)
		if (char_count && char_count < Length)
			NumPut(0, Address+0, char_count++*2, "UShort")
	}
	else if Encoding is integer
	{
		; Convert native ANSI string to UTF-16 first.	NOTE - wbuf_len includes the null-terminator.
		VarSetCapacity(wbuf, 2 * wbuf_len := StrPut2(String, "UTF-16")), StrPut2(String, &wbuf, "UTF-16")
		
		; UTF-8 and some other encodings do not support this flag.	Avoid it for UTF-8
		; (which is probably common) and rely on the fallback behaviour for other encodings.
		flags := Encoding=65001 ? 0 : 0x400	; WC_NO_BEST_FIT_CHARS
		if (Length <= 0) ; -1 or 0
		{
			; Determine required buffer size.
			loop 2 {
				char_count := DllCall("WideCharToMultiByte", "uint", Encoding, "uint", flags, "uint", &wbuf, "int", wbuf_len, "uint", 0, "int", 0, "uint", 0, "uint", 0)
				if (char_count || A_LastError != 1004) ; ERROR_INVALID_FLAGS
					break
				flags := 0	; Try again without WC_NO_BEST_FIT_CHARS.
			}
			if (!char_count)
				return ; FAIL
			if (Length == 0) ; Caller just wants the required buffer size.
				return char_count
			; Assume there is sufficient buffer space and hope for the best:
			Length := char_count
		}
		; Convert to target encoding.
		char_count := DllCall("WideCharToMultiByte", "uint", Encoding, "uint", flags, "uint", &wbuf, "int", wbuf_len, "uint", Address, "int", Length, "uint", 0, "uint", 0)
		; Since above did not null-terminate, check for buffer space and null-terminate if there's room.
		; It is tempting to always null-terminate (potentially replacing the last byte of data),
		; but that would exclude this function as a means to copy a string into a fixed-length array.
		if (char_count && char_count < Length)
			NumPut(0, Address+0, char_count++, "Char")
		; else no space to null-terminate; or conversion failed.
	}
	; Return the number of characters copied.
	return char_count
}
