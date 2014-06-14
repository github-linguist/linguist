
; flat assembler interface for Win32
; Copyright (c) 1999-2014, Tomasz Grysztar.
; All rights reserved.

CREATE_NEW	       = 1
CREATE_ALWAYS	       = 2
OPEN_EXISTING	       = 3
OPEN_ALWAYS	       = 4
TRUNCATE_EXISTING      = 5

FILE_SHARE_READ        = 1
FILE_SHARE_WRITE       = 2
FILE_SHARE_DELETE      = 4

GENERIC_READ	       = 80000000h
GENERIC_WRITE	       = 40000000h

STD_INPUT_HANDLE       = 0FFFFFFF6h
STD_OUTPUT_HANDLE      = 0FFFFFFF5h
STD_ERROR_HANDLE       = 0FFFFFFF4h

MEM_COMMIT	       = 1000h
MEM_RESERVE	       = 2000h
MEM_DECOMMIT	       = 4000h
MEM_RELEASE	       = 8000h
MEM_FREE	       = 10000h
MEM_PRIVATE	       = 20000h
MEM_MAPPED	       = 40000h
MEM_RESET	       = 80000h
MEM_TOP_DOWN	       = 100000h

PAGE_NOACCESS	       = 1
PAGE_READONLY	       = 2
PAGE_READWRITE	       = 4
PAGE_WRITECOPY	       = 8
PAGE_EXECUTE	       = 10h
PAGE_EXECUTE_READ      = 20h
PAGE_EXECUTE_READWRITE = 40h
PAGE_EXECUTE_WRITECOPY = 80h
PAGE_GUARD	       = 100h
PAGE_NOCACHE	       = 200h

init_memory:
	xor	eax,eax
	mov	[memory_start],eax
	mov	eax,esp
	and	eax,not 0FFFh
	add	eax,1000h-10000h
	mov	[stack_limit],eax
	mov	eax,[memory_setting]
	shl	eax,10
	jnz	allocate_memory
	push	buffer
	call	[GlobalMemoryStatus]
	mov	eax,dword [buffer+20]
	mov	edx,dword [buffer+12]
	cmp	eax,0
	jl	large_memory
	cmp	edx,0
	jl	large_memory
	shr	eax,2
	add	eax,edx
	jmp	allocate_memory
    large_memory:
	mov	eax,80000000h
    allocate_memory:
	mov	edx,eax
	shr	edx,2
	mov	ecx,eax
	sub	ecx,edx
	mov	[memory_end],ecx
	mov	[additional_memory_end],edx
	push	PAGE_READWRITE
	push	MEM_COMMIT
	push	eax
	push	0
	call	[VirtualAlloc]
	or	eax,eax
	jz	not_enough_memory
	mov	[memory_start],eax
	add	eax,[memory_end]
	mov	[memory_end],eax
	mov	[additional_memory],eax
	add	[additional_memory_end],eax
	ret
    not_enough_memory:
	mov	eax,[additional_memory_end]
	shl	eax,1
	cmp	eax,4000h
	jb	out_of_memory
	jmp	allocate_memory

exit_program:
	movzx	eax,al
	push	eax
	mov	eax,[memory_start]
	test	eax,eax
	jz	do_exit
	push	MEM_RELEASE
	push	0
	push	eax
	call	[VirtualFree]
    do_exit:
	call	[ExitProcess]

get_environment_variable:
	mov	ecx,[memory_end]
	sub	ecx,edi
	cmp	ecx,4000h
	jbe	buffer_for_variable_ok
	mov	ecx,4000h
    buffer_for_variable_ok:
	push	ecx
	push	edi
	push	esi
	call	[GetEnvironmentVariable]
	add	edi,eax
	cmp	edi,[memory_end]
	jae	out_of_memory
	ret

open:
	push	0
	push	0
	push	OPEN_EXISTING
	push	0
	push	FILE_SHARE_READ
	push	GENERIC_READ
	push	edx
	call	[CreateFile]
	cmp	eax,-1
	je	file_error
	mov	ebx,eax
	clc
	ret
    file_error:
	stc
	ret
create:
	push	0
	push	0
	push	CREATE_ALWAYS
	push	0
	push	FILE_SHARE_READ
	push	GENERIC_WRITE
	push	edx
	call	[CreateFile]
	cmp	eax,-1
	je	file_error
	mov	ebx,eax
	clc
	ret
write:
	push	0
	push	bytes_count
	push	ecx
	push	edx
	push	ebx
	call	[WriteFile]
	or	eax,eax
	jz	file_error
	clc
	ret
read:
	mov	ebp,ecx
	push	0
	push	bytes_count
	push	ecx
	push	edx
	push	ebx
	call	[ReadFile]
	or	eax,eax
	jz	file_error
	cmp	ebp,[bytes_count]
	jne	file_error
	clc
	ret
close:
	push	ebx
	call	[CloseHandle]
	ret
lseek:
	movzx	eax,al
	push	eax
	push	0
	push	edx
	push	ebx
	call	[SetFilePointer]
	ret

display_string:
	push	[con_handle]
	call	[GetStdHandle]
	mov	ebp,eax
	mov	edi,esi
	or	ecx,-1
	xor	al,al
	repne	scasb
	neg	ecx
	sub	ecx,2
	push	0
	push	bytes_count
	push	ecx
	push	esi
	push	ebp
	call	[WriteFile]
	ret
display_character:
	push	ebx
	mov	[character],dl
	push	[con_handle]
	call	[GetStdHandle]
	mov	ebx,eax
	push	0
	push	bytes_count
	push	1
	push	character
	push	ebx
	call	[WriteFile]
	pop	ebx
	ret
display_number:
	push	ebx
	mov	ecx,1000000000
	xor	edx,edx
	xor	bl,bl
      display_loop:
	div	ecx
	push	edx
	cmp	ecx,1
	je	display_digit
	or	bl,bl
	jnz	display_digit
	or	al,al
	jz	digit_ok
	not	bl
      display_digit:
	mov	dl,al
	add	dl,30h
	push	ecx
	call	display_character
	pop	ecx
      digit_ok:
	mov	eax,ecx
	xor	edx,edx
	mov	ecx,10
	div	ecx
	mov	ecx,eax
	pop	eax
	or	ecx,ecx
	jnz	display_loop
	pop	ebx
	ret

display_user_messages:
	mov	[displayed_count],0
	call	show_display_buffer
	cmp	[displayed_count],1
	jb	line_break_ok
	je	make_line_break
	mov	ax,word [last_displayed]
	cmp	ax,0A0Dh
	je	line_break_ok
	cmp	ax,0D0Ah
	je	line_break_ok
      make_line_break:
	mov	word [buffer],0A0Dh
	push	[con_handle]
	call	[GetStdHandle]
	push	0
	push	bytes_count
	push	2
	push	buffer
	push	eax
	call	[WriteFile]
      line_break_ok:
	ret
display_block:
	add	[displayed_count],ecx
	cmp	ecx,1
	ja	take_last_two_characters
	jb	block_displayed
	mov	al,[last_displayed+1]
	mov	ah,[esi]
	mov	word [last_displayed],ax
	jmp	block_ok
      take_last_two_characters:
	mov	ax,[esi+ecx-2]
	mov	word [last_displayed],ax
      block_ok:
	push	ecx
	push	[con_handle]
	call	[GetStdHandle]
	pop	ecx
	push	0
	push	bytes_count
	push	ecx
	push	esi
	push	eax
	call	[WriteFile]
      block_displayed:
	ret

fatal_error:
	mov	[con_handle],STD_ERROR_HANDLE
	mov	esi,error_prefix
	call	display_string
	pop	esi
	call	display_string
	mov	esi,error_suffix
	call	display_string
	mov	al,0FFh
	jmp	exit_program
assembler_error:
	mov	[con_handle],STD_ERROR_HANDLE
	call	display_user_messages
	push	dword 0
	mov	ebx,[current_line]
      get_error_lines:
	mov	eax,[ebx]
	cmp	byte [eax],0
	je	get_next_error_line
	push	ebx
	test	byte [ebx+7],80h
	jz	display_error_line
	mov	edx,ebx
      find_definition_origin:
	mov	edx,[edx+12]
	test	byte [edx+7],80h
	jnz	find_definition_origin
	push	edx
      get_next_error_line:
	mov	ebx,[ebx+8]
	jmp	get_error_lines
      display_error_line:
	mov	esi,[ebx]
	call	display_string
	mov	esi,line_number_start
	call	display_string
	mov	eax,[ebx+4]
	and	eax,7FFFFFFFh
	call	display_number
	mov	dl,']'
	call	display_character
	pop	esi
	cmp	ebx,esi
	je	line_number_ok
	mov	dl,20h
	call	display_character
	push	esi
	mov	esi,[esi]
	movzx	ecx,byte [esi]
	inc	esi
	call	display_block
	mov	esi,line_number_start
	call	display_string
	pop	esi
	mov	eax,[esi+4]
	and	eax,7FFFFFFFh
	call	display_number
	mov	dl,']'
	call	display_character
      line_number_ok:
	mov	esi,line_data_start
	call	display_string
	mov	esi,ebx
	mov	edx,[esi]
	call	open
	mov	al,2
	xor	edx,edx
	call	lseek
	mov	edx,[esi+8]
	sub	eax,edx
	jz	line_data_displayed
	push	eax
	xor	al,al
	call	lseek
	mov	ecx,[esp]
	mov	edx,[additional_memory]
	lea	eax,[edx+ecx]
	cmp	eax,[additional_memory_end]
	ja	out_of_memory
	call	read
	call	close
	pop	ecx
	mov	esi,[additional_memory]
      get_line_data:
	mov	al,[esi]
	cmp	al,0Ah
	je	display_line_data
	cmp	al,0Dh
	je	display_line_data
	cmp	al,1Ah
	je	display_line_data
	or	al,al
	jz	display_line_data
	inc	esi
	loop	get_line_data
      display_line_data:
	mov	ecx,esi
	mov	esi,[additional_memory]
	sub	ecx,esi
	call	display_block
      line_data_displayed:
	mov	esi,cr_lf
	call	display_string
	pop	ebx
	or	ebx,ebx
	jnz	display_error_line
	mov	esi,error_prefix
	call	display_string
	pop	esi
	call	display_string
	mov	esi,error_suffix
	call	display_string
	mov	al,2
	jmp	exit_program

make_timestamp:
	push	buffer
	call	[GetSystemTime]
	movzx	ecx,word [buffer]
	mov	eax,ecx
	sub	eax,1970
	mov	ebx,365
	mul	ebx
	mov	ebp,eax
	mov	eax,ecx
	sub	eax,1969
	shr	eax,2
	add	ebp,eax
	mov	eax,ecx
	sub	eax,1901
	mov	ebx,100
	div	ebx
	sub	ebp,eax
	mov	eax,ecx
	xor	edx,edx
	sub	eax,1601
	mov	ebx,400
	div	ebx
	add	ebp,eax
	movzx	ecx,word [buffer+2]
	mov	eax,ecx
	dec	eax
	mov	ebx,30
	mul	ebx
	add	ebp,eax
	cmp	ecx,8
	jbe	months_correction
	mov	eax,ecx
	sub	eax,7
	shr	eax,1
	add	ebp,eax
	mov	ecx,8
      months_correction:
	mov	eax,ecx
	shr	eax,1
	add	ebp,eax
	cmp	ecx,2
	jbe	day_correction_ok
	sub	ebp,2
	movzx	ecx,word [buffer]
	test	ecx,11b
	jnz	day_correction_ok
	xor	edx,edx
	mov	eax,ecx
	mov	ebx,100
	div	ebx
	or	edx,edx
	jnz	day_correction
	mov	eax,ecx
	mov	ebx,400
	div	ebx
	or	edx,edx
	jnz	day_correction_ok
      day_correction:
	inc	ebp
      day_correction_ok:
	movzx	eax,word [buffer+6]
	dec	eax
	add	eax,ebp
	mov	ebx,24
	mul	ebx
	movzx	ecx,word [buffer+8]
	add	eax,ecx
	mov	ebx,60
	mul	ebx
	movzx	ecx,word [buffer+10]
	add	eax,ecx
	mov	ebx,60
	mul	ebx
	movzx	ecx,word [buffer+12]
	add	eax,ecx
	adc	edx,0
	ret

error_prefix db 'error: ',0
error_suffix db '.'
cr_lf db 0Dh,0Ah,0
line_number_start db ' [',0
line_data_start db ':',0Dh,0Ah,0
