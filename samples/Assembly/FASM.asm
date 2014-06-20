
; flat assembler interface for Win32
; Copyright (c) 1999-2014, Tomasz Grysztar.
; All rights reserved.

	format	PE console

section '.text' code readable executable

start:

	mov	[con_handle],STD_OUTPUT_HANDLE
	mov	esi,_logo
	call	display_string

	call	get_params
	jc	information

	call	init_memory

	mov	esi,_memory_prefix
	call	display_string
	mov	eax,[memory_end]
	sub	eax,[memory_start]
	add	eax,[additional_memory_end]
	sub	eax,[additional_memory]
	shr	eax,10
	call	display_number
	mov	esi,_memory_suffix
	call	display_string

	call	[GetTickCount]
	mov	[start_time],eax

	call	preprocessor
	call	parser
	call	assembler
	call	formatter

	call	display_user_messages
	movzx	eax,[current_pass]
	inc	eax
	call	display_number
	mov	esi,_passes_suffix
	call	display_string
	call	[GetTickCount]
	sub	eax,[start_time]
	xor	edx,edx
	mov	ebx,100
	div	ebx
	or	eax,eax
	jz	display_bytes_count
	xor	edx,edx
	mov	ebx,10
	div	ebx
	push	edx
	call	display_number
	mov	dl,'.'
	call	display_character
	pop	eax
	call	display_number
	mov	esi,_seconds_suffix
	call	display_string
      display_bytes_count:
	mov	eax,[written_size]
	call	display_number
	mov	esi,_bytes_suffix
	call	display_string
	xor	al,al
	jmp	exit_program

information:
	mov	esi,_usage
	call	display_string
	mov	al,1
	jmp	exit_program

get_params:
	mov	[input_file],0
	mov	[output_file],0
	mov	[symbols_file],0
	mov	[memory_setting],0
	mov	[passes_limit],100
	call	[GetCommandLine]
	mov	esi,eax
	mov	edi,params
    find_command_start:
	lodsb
	cmp	al,20h
	je	find_command_start
	cmp	al,22h
	je	skip_quoted_name
    skip_name:
	lodsb
	cmp	al,20h
	je	find_param
	or	al,al
	jz	all_params
	jmp	skip_name
    skip_quoted_name:
	lodsb
	cmp	al,22h
	je	find_param
	or	al,al
	jz	all_params
	jmp	skip_quoted_name
    find_param:
	lodsb
	cmp	al,20h
	je	find_param
	cmp	al,'-'
	je	option_param
	cmp	al,0Dh
	je	all_params
	or	al,al
	jz	all_params
	cmp	[input_file],0
	jne	get_output_file
	mov	[input_file],edi
	jmp	process_param
      get_output_file:
	cmp	[output_file],0
	jne	bad_params
	mov	[output_file],edi
    process_param:
	cmp	al,22h
	je	string_param
    copy_param:
	stosb
	lodsb
	cmp	al,20h
	je	param_end
	cmp	al,0Dh
	je	param_end
	or	al,al
	jz	param_end
	jmp	copy_param
    string_param:
	lodsb
	cmp	al,22h
	je	string_param_end
	cmp	al,0Dh
	je	param_end
	or	al,al
	jz	param_end
	stosb
	jmp	string_param
    option_param:
	lodsb
	cmp	al,'m'
	je	memory_option
	cmp	al,'M'
	je	memory_option
	cmp	al,'p'
	je	passes_option
	cmp	al,'P'
	je	passes_option
	cmp	al,'s'
	je	symbols_option
	cmp	al,'S'
	je	symbols_option
    bad_params:
	stc
	ret
    get_option_value:
	xor	eax,eax
	mov	edx,eax
    get_option_digit:
	lodsb
	cmp	al,20h
	je	option_value_ok
	cmp	al,0Dh
	je	option_value_ok
	or	al,al
	jz	option_value_ok
	sub	al,30h
	jc	invalid_option_value
	cmp	al,9
	ja	invalid_option_value
	imul	edx,10
	jo	invalid_option_value
	add	edx,eax
	jc	invalid_option_value
	jmp	get_option_digit
    option_value_ok:
	dec	esi
	clc
	ret
    invalid_option_value:
	stc
	ret
    memory_option:
	lodsb
	cmp	al,20h
	je	memory_option
	cmp	al,0Dh
	je	bad_params
	or	al,al
	jz	bad_params
	dec	esi
	call	get_option_value
	or	edx,edx
	jz	bad_params
	cmp	edx,1 shl (32-10)
	jae	bad_params
	mov	[memory_setting],edx
	jmp	find_param
    passes_option:
	lodsb
	cmp	al,20h
	je	passes_option
	cmp	al,0Dh
	je	bad_params
	or	al,al
	jz	bad_params
	dec	esi
	call	get_option_value
	or	edx,edx
	jz	bad_params
	cmp	edx,10000h
	ja	bad_params
	mov	[passes_limit],dx
	jmp	find_param
    symbols_option:
	mov	[symbols_file],edi
      find_symbols_file_name:
	lodsb
	cmp	al,20h
	jne	process_param
	jmp	find_symbols_file_name
    param_end:
	dec	esi
    string_param_end:
	xor	al,al
	stosb
	jmp	find_param
    all_params:
	cmp	[input_file],0
	je	bad_params
	clc
	ret

include 'system.inc'

include '..\errors.inc'
include '..\symbdump.inc'
include '..\preproce.inc'
include '..\parser.inc'
include '..\exprpars.inc'
include '..\assemble.inc'
include '..\exprcalc.inc'
include '..\formats.inc'
include '..\x86_64.inc'
include '..\avx.inc'

include '..\tables.inc'
include '..\messages.inc'

section '.data' data readable writeable

include '..\version.inc'

_copyright db 'Copyright (c) 1999-2014, Tomasz Grysztar',0Dh,0Ah,0

_logo db 'flat assembler  version ',VERSION_STRING,0
_usage db 0Dh,0Ah
       db 'usage: fasm <source> [output]',0Dh,0Ah
       db 'optional settings:',0Dh,0Ah
       db ' -m <limit>    set the limit in kilobytes for the available memory',0Dh,0Ah
       db ' -p <limit>    set the maximum allowed number of passes',0Dh,0Ah
       db ' -s <file>     dump symbolic information for debugging',0Dh,0Ah
       db 0
_memory_prefix db '  (',0
_memory_suffix db ' kilobytes memory)',0Dh,0Ah,0
_passes_suffix db ' passes, ',0
_seconds_suffix db ' seconds, ',0
_bytes_suffix db ' bytes.',0Dh,0Ah,0

align 4

include '..\variable.inc'

con_handle dd ?
memory_setting dd ?
start_time dd ?
bytes_count dd ?
displayed_count dd ?
character db ?
last_displayed rb 2

params rb 1000h
options rb 1000h
buffer rb 4000h

stack 10000h

section '.idata' import data readable writeable

  dd 0,0,0,rva kernel_name,rva kernel_table
  dd 0,0,0,0,0

  kernel_table:
    ExitProcess dd rva _ExitProcess
    CreateFile dd rva _CreateFileA
    ReadFile dd rva _ReadFile
    WriteFile dd rva _WriteFile
    CloseHandle dd rva _CloseHandle
    SetFilePointer dd rva _SetFilePointer
    GetCommandLine dd rva _GetCommandLineA
    GetEnvironmentVariable dd rva _GetEnvironmentVariable
    GetStdHandle dd rva _GetStdHandle
    VirtualAlloc dd rva _VirtualAlloc
    VirtualFree dd rva _VirtualFree
    GetTickCount dd rva _GetTickCount
    GetSystemTime dd rva _GetSystemTime
    GlobalMemoryStatus dd rva _GlobalMemoryStatus
    dd 0

  kernel_name db 'KERNEL32.DLL',0

  _ExitProcess dw 0
    db 'ExitProcess',0
  _CreateFileA dw 0
    db 'CreateFileA',0
  _ReadFile dw 0
    db 'ReadFile',0
  _WriteFile dw 0
    db 'WriteFile',0
  _CloseHandle dw 0
    db 'CloseHandle',0
  _SetFilePointer dw 0
    db 'SetFilePointer',0
  _GetCommandLineA dw 0
    db 'GetCommandLineA',0
  _GetEnvironmentVariable dw 0
    db 'GetEnvironmentVariableA',0
  _GetStdHandle dw 0
    db 'GetStdHandle',0
  _VirtualAlloc dw 0
    db 'VirtualAlloc',0
  _VirtualFree dw 0
    db 'VirtualFree',0
  _GetTickCount dw 0
    db 'GetTickCount',0
  _GetSystemTime dw 0
    db 'GetSystemTime',0
  _GlobalMemoryStatus dw 0
    db 'GlobalMemoryStatus',0

section '.reloc' fixups data readable discardable
