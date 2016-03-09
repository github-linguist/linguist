;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Forth by Chris Hinsley
;; nasm -f macho forth.nasm
;; ld -o forth -e _main forth.o
;; ./forth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	%define VERSION_NUM 30

	; various buffer area sizes
	%define DATA_STACK_SIZE 1024
	%define USER_DEFS_SIZE (64*1024)
	%define NUM_HASH_CHAINS 64
	%define MAX_LINE_SIZE 128

	%define SYS_exit 1
	%define SYS_read 3
	%define SYS_write 4
	%define SYS_open 5
	%define SYS_close 6
	%define SYS_unlink 10
	%define	SYS_mprotect 74
	%define SYS_fsync 95
	%define SYS_rename 128
	%define SYS_stat 188
	%define SYS_lseek 199
	%define SYS_fstat 189
	%define SYS_ftruncate 201

	%define PROT_READ 0x01		;pages can be read
	%define PROT_WRITE 0x02		;pages can be written
	%define PROT_EXEC 0x04		;pages can be executed
	%define PROT_ALL (PROT_READ | PROT_WRITE | PROT_EXEC)
	%define PAGE_SIZE 4096

;;;;;;;;;;;;;;;;;;;;;;;;;;
; some NASM codeing macros
;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro loopstart 0
		%push loopstart
	%$loop_start:
	%endmacro

	%macro break 0
		jmp %$loop_exit
	%endmacro

	%macro breakif 1
		j%+1 %$loop_exit
	%endmacro

	%macro loopend 0
		jmp %$loop_start
	%$loop_exit:
		%pop
	%endmacro

	%macro repeat 0
		%push repeat
	%$loop_start:
	%endmacro

	%macro until 1
		j%-1 %$loop_start
	%$loop_exit:
		%pop
	%endmacro

	%macro if 1
		%push if
		j%-1 %$ifnot
	%endmacro

	%macro else 0
		%ifctx if
			%repl else
			jmp %$ifend
		%$ifnot:
		%else
			%error "expected `if' before `else'"
		%endif
	%endmacro

	%macro endif 0
		%ifctx if
		%$ifnot:
			%pop
		%elifctx else
		%$ifend:
			%pop
		%else
			%error "expected `if' or `else' before `endif'"
		%endif
	%endmacro

;;;;;;;;;;;;;;;;
; base VM macros
;;;;;;;;;;;;;;;;

	; eip	Forths IP
	; esp	Forths R
	; ebp	Forths S
	; ebx	Forths TOS

	; push on to return stack
	%macro PUSHRSP 1
		push %1
	%endm

	; pop top of return stack
	%macro POPRSP 1
		pop %1
	%endm

	; save into return stack
	%macro PUTRSP 2
		%if (%2 = 0)
			mov [esp], %1
		%elif ((%2 >= -128) && (%2 < 128))
			mov [byte esp + %2], %1
		%else
			mov [long esp + %2], %1
		%endif
	%endm

	; load from return stack
	%macro PICKRSP 2
		%if (%2 = 0)
			mov %1, [esp]
		%elif ((%2 >= -128) && (%2 < 128))
			mov %1, [byte esp + %2]
		%else
			mov %1, [long esp + %2]
		%endif
	%endm

	; set return stack
	%macro SETRSP 1
		mov esp, %1
	%endm

	; get return stack
	%macro GETRSP 1
		mov %1, esp
	%endm

	; adjust return stack
	%macro ADDRSP 1
		%if ((%1 >= -128) && (%1 < 128))
			add esp, byte %1
		%else
			add esp, %1
		%endif
	%endm

	; push on to data stack
	%macro PUSHDSP 1
		sub ebp, byte 4
		mov [ebp], %1
	%endm

	; pop top of data stack
	%macro POPDSP 1
		mov %1, [ebp]
		add ebp, byte 4
	%endm

	; save into data stack
	%macro PUTDSP 2
		%if (%2 = 0)
			mov [ebp], %1
		%elif ((%2 >= -128) && (%2 < 128))
			mov [byte ebp + %2], %1
		%else
			mov [long ebp + %2], %1
		%endif
	%endm

	; load from data stack
	%macro PICKDSP 2
		%if (%2 = 0)
			mov %1, [ebp]
		%elif ((%2 >= -128) && (%2 < 128))
			mov %1, [byte ebp + %2]
		%else
			mov %1, [long ebp + %2]
		%endif
	%endm

	; set data stack
	%macro SETDSP 1
		mov ebp, %1
	%endm

	; get data stack
	%macro GETDSP 1
		mov %1, ebp
	%endm

	; adjust data stack
	%macro ADDDSP 1
		%if ((%1 >= -128) && (%1 < 128))
			add ebp, byte %1
		%else
			add ebp, %1
		%endif
	%endm

	; load value onto data stack
	%macro LOADTOS 1
		PUSHDSP ebx
		mov ebx, %1
	%endm

	; move from data to return stack
	%macro TORSP 0
		PUSHRSP ebx
		POPDSP ebx
	%endm

	; move from return to data stack
	%macro FROMRSP 0
		PUSHDSP ebx
		POPRSP ebx
	%endm

	; copy from return to data stack
	%macro FETCHRSP 0
		PUSHDSP ebx
		PICKRSP ebx, 0
	%endm

	; align reg
	%define DP_ALIGN 3
	%macro ALIGNREG 1
		add %1, byte DP_ALIGN
		and %1, byte ~DP_ALIGN
	%endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary building macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; format of dictionary entry flag byte
	%define F_IMMED 0x80
	%define F_HIDDEN 0x20
	%define F_LENMASK 0x1f

	%define NULL 0
	%define H_LLINK 0
	%define H_HLINK 4
	%define H_NSIZE 8
	%define H_NAME 9

	%define XT_BODY -12
	%define XT_LENGTH -8
	%define XT_COMPILE -4
	%define XT_SIZE 12
	
	%macro defword 4
		%push newword
		%strlen len %1
		align 4
	dic_%3:
		dd NULL				; LATEST list link
		dd NULL				; hash chain link
		db len + %2			; flags + length byte
		db %1				; the name
		dd %3				; body pointer
		dd %$code_end - %3	; code length
		dd %4				; compile action word
	%3:
	%endm					; assembler code follows

	%macro defword_end 0
	%$code_end:
		%pop
	%endm

	%macro defvar 4
		defword %1, %2, %3, WORD_INLINE_COMMA
		LOADTOS var_%3
		ret
		defword_end
		align 4
	var_%3:
		dd %4
	%endm

	%macro defvar2 5
		defword %1, %2, %3, WORD_INLINE_COMMA
		LOADTOS var_%3
		ret
		defword_end
		align 4
	var_%3:
		dd %4
		dd %5
	%endm

	%macro defconst 4
		defword %1, %2, %3, WORD_INLINE_COMMA
		LOADTOS %4
		ret
		defword_end
	%endm

;;;;;;;;;;;;;;;;;;;;;;;;;;
; entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;

	SECTION .text
	global _main
_main:
	; use mprotect to allow read/write/execute of the data section
	mov edx, forth_start
	and edx, -PAGE_SIZE		;start address
	mov ecx, forth_end
	sub ecx, edx			;length
	mov ebx, PROT_ALL		;flags
	push ebx
	push ecx
	push edx
	push 0					;padding
	mov eax, SYS_mprotect
	int 0x80
	add esp, 16
	jmp forth_start

	SECTION .data
forth_start:
	; init data and return stacks, saving initial positions
	; in Forth vars R0 and S0
	cld
	GETRSP [var_WORD_SZ]
	SETDSP [var_WORD_SZ]
	ADDRSP -DATA_STACK_SIZE
	GETRSP [var_WORD_RZ]

	; link built in dictionary
	mov esi, dictionary_start
	xor edi, edi
	repeat
		lodsd
		mov [eax + H_LLINK], edi
		mov edi, eax
		push esi
		mov cl, [eax + H_NSIZE]
		and ecx, F_LENMASK
		lea esi, [eax + H_NAME]
		call strhashi
		and ebx, NUM_HASH_CHAINS-1
		mov esi, hash_buckets
		mov eax, [esi + (ebx * 4)]
		mov [esi + (ebx * 4)], edi
		mov [edi + H_HLINK], eax
		pop esi
		cmp esi, dictionary_end
	until z
	mov [var_WORD_LATEST], edi

	; run temp interpreter loop till we can get into the real QUIT word
	call WORD_LBRAC			; interpret state
	LOADTOS 666q			; octal !
	TORSP
	LOADTOS 0
	TORSP
	LOADTOS bootfile
	TORSP
	call WORD_SYS_OPEN
	call WORD_SYSCALL
	ADDRSP 12
	TORSP					; ( fd ) of "forth.f"
	loopstart
		LOADTOS tib_buffer
		LOADTOS MAX_LINE_SIZE
		FETCHRSP			; ( c-addr len fd )
		call WORD_READLINE	; ( num flag flag )
		call WORD_DROP2
		LOADTOS tib_buffer
		call WORD_SWAP
		call WORD_INHASH
		call WORD_STORE2
		LOADTOS 0
		call WORD_TOIN
		call WORD_STORE
		call WORD_INTERPRET
	loopend					; and loop till QUIT takes over

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a few case insensative string operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	%macro to_lower 1
		; lower case check
		cmp %1, 'A'
		if ge
			cmp %1, 'Z'
			if le
				; make it lower case
				add %1, byte 'a' - 'A'
			endif
		endif
	%endm

strcpyi:
	test ecx, ecx
	if nz
	strcpyi_l1:
		lodsb
		to_lower al
		stosb
		loop strcpyi_l1
	endif
	ret

strcmpi:
	test ecx, ecx
	if nz
	strcmpi_l1:
		lodsb
		mov bl, [edi]
		lea edi, [edi + 1]
		to_lower al
		to_lower bl
		cmp bl, al
		if z
			loop strcmpi_l1
		endif
	endif
	ret

;;;;;;;;;;;;;;;
; hash function
;;;;;;;;;;;;;;;

strhashi:
	mov ebx, 5381
	test ecx, ecx
	if nz
		mov edx, 33
	strhashi_l1:
		lodsb
		movzx eax, al
		to_lower eax
		imul ebx, edx
		add ebx, eax
		loop strhashi_l1
	endif
	ret

;;;;;;;;;;;;;;;;;;;
; syscall functions
;;;;;;;;;;;;;;;;;;;

_syscall:
	int 0x80
	if c
		neg eax
	endif
	ret

_lsyscall:
	int 0x80
	if c
		not eax
		not edx
		add eax, 1
		adc edx, 0
	endif
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; built in variables
; STATE		Is the interpreter executing code (0) or compiling a word (non-zero)?
; LATEST	Points to the latest (most recently defined) word in the dictionary.
; DP		Points to the next free byte of memory. When compiling, compiled words go here.
; S0		Stores the address of the top of the parameter stack.
; R0		The address of the top of the return stack.
; BASE		The current base for printing and reading numbers.
; #IN		The current input buffer descriptor.
; >IN		The current input offset.
; SOURCEFD	The current input source file descriptor.
; BLK		The current block number.
; CHARBUF	Single char buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defvar "state", 0, WORD_STATE, 0
	defvar "dp", 0, WORD_DP, dictionary_start
	defvar "latest", 0, WORD_LATEST, 0
	defvar "s0", 0, WORD_SZ, 0
	defvar "r0", 0, WORD_RZ, 0
	defvar "base", 0, WORD_BASE, 10
	defvar2 "#IN", 0, WORD_INHASH, 0, 0
	defvar ">in", 0, WORD_TOIN, 0
	defvar "sourcefd", 0, WORD_SOURCEFD, 0
	defvar "blk", 0, WORD_BLK, 0
	defvar "charbuf", 0, WORD_CHARBUF, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; built in constants
; VERSION		The current version of this FORTH.
; WORDBUF		The address of the buffer WORD uses.
; LINESIZE		The line buffer size.
; F_IMMED		The IMMEDIATE flag's actual value.
; F_HIDDEN		The HIDDEN flag's actual value.
; F_LENMASK		The length mask in the flags/len byte.
; H_NSIZE		The flags/len field offset.
; H_NAME		The name field offset.
; XT_BODY		The xt body pointer.
; XT_LENGTH		The xt length field offset.
; XT_COMPILE	The xt compile field offset.
; XT_SIZE		The xt size offset.
; SYS_*			The numeric codes of various syscalls.
; O_*			Various sycall flags/modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defconst "version", 0, WORD_VERSION, VERSION_NUM
	defconst "wordbuf", 0, WORD_WORDBUF, word_buf
	defconst "linesize", 0, WORD_LINESIZE, MAX_LINE_SIZE
	defconst "f_immed", 0, WORD__F_IMMED, F_IMMED
	defconst "f_hidden", 0, WORD__F_HIDDEN, F_HIDDEN
	defconst "f_lenmask", 0, WORD__F_LENMASK, F_LENMASK
	defconst "h_nsize", 0, WORD__H_NSIZE, H_NSIZE
	defconst "h_name", 0, WORD__H_NAME, H_NAME
	defconst "xt_body", 0, WORD__XT_BODY, XT_BODY
	defconst "xt_length", 0, WORD__XT_LENGTH, XT_LENGTH
	defconst "xt_compile", 0, WORD__XT_COMPILE, XT_COMPILE
	defconst "xt_size", 0, WORD__XT_SIZE, XT_SIZE

	defconst "sys_exit", 0, WORD_SYS_EXIT, SYS_exit
	defconst "sys_open", 0, WORD_SYS_OPEN, SYS_open
	defconst "sys_close", 0, WORD_SYS_CLOSE, SYS_close
	defconst "sys_read", 0, WORD_SYS_READ, SYS_read
	defconst "sys_write", 0, WORD_SYS_WRITE, SYS_write
	defconst "sys_unlink", 0, WORD_SYS_UNLINK, SYS_unlink
	defconst "sys_rename", 0, WORD_SYS_RENAME, SYS_rename
	defconst "sys_ftruncate", 0, WORD_SYS_FTRUNCATE, SYS_ftruncate
	defconst "sys_fsync", 0, WORD_SYS_FSYNC, SYS_fsync
	defconst "sys_lseek", 0, WORD_SYS_LSEEK, SYS_lseek
	defconst "sys_fstat", 0, WORD_SYS_FSTAT, SYS_fstat
	defconst "sys_stat", 0, WORD_SYS_STAT, SYS_stat

	defconst "o_rdonly", 0, WORD_O_RDONLY, 0x0
	defconst "o_wronly", 0, WORD_O_WRONLY, 0x1
	defconst "o_rdwr", 0, WORD_O_RDWR, 0x2
	defconst "o_creat", 0, WORD_O_CREAT, 0x100
	defconst "o_excl", 0, WORD_O_EXCL, 0x200
	defconst "o_trunc", 0, WORD_O_TRUNC, 0x1000
	defconst "o_append", 0, WORD_O_APPEND, 0x2000
	defconst "o_nonblock", 0, WORD_O_NONBLOCK, 0x4000

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data stack ordering words
;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "dsp@", 0, WORD_DSPFETCH, WORD_INLINE_COMMA
	PUSHDSP ebx
	GETDSP ebx
	ret
	defword_end

	defword "dsp!", 0, WORD_DSPSTORE, WORD_INLINE_COMMA
	SETDSP ebx
	POPDSP ebx
	ret
	defword_end

	defword "drop", 0, WORD_DROP, WORD_INLINE_COMMA
	POPDSP ebx
	ret
	defword_end

	defword "swap", 0, WORD_SWAP, WORD_INLINE_COMMA
	xchg ebx, [ebp]
	ret
	defword_end

	defword "dup", 0, WORD_DUP, WORD_INLINE_COMMA
	PUSHDSP ebx
	ret
	defword_end

	defword "over", 0, WORD_OVER, WORD_INLINE_COMMA
	PUSHDSP ebx
	PICKDSP ebx, 4
	ret
	defword_end

	defword "rot", 0, WORD_ROT, WORD_INLINE_COMMA
	mov eax, ebx
	PICKDSP ecx, 0
	PICKDSP ebx, 4
	PUTDSP eax, 0
	PUTDSP ecx, 4
	ret
	defword_end

	defword "-rot", 0, WORD_NROT, WORD_INLINE_COMMA
	mov eax, ebx
	PICKDSP ebx, 0
	PICKDSP ecx, 4
	PUTDSP ecx, 0
	PUTDSP eax, 4
	ret
	defword_end

	defword "2drop", 0, WORD_DROP2, WORD_INLINE_COMMA
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "2dup", 0, WORD_DUP2, WORD_INLINE_COMMA
	PICKDSP eax, 0
	ADDDSP -8
	PUTDSP eax, 0
	PUTDSP ebx, 4
	ret
	defword_end

	defword "2swap", 0, WORD_SWAP2, WORD_INLINE_COMMA
	mov eax, ebx
	PICKDSP ecx, 0
	PICKDSP ebx, 4
	PICKDSP edx, 8
	PUTDSP edx, 0
	PUTDSP eax, 4
	PUTDSP ecx, 8
	ret
	defword_end

	defword "2rot", 0, WORD_ROT2, WORD_INLINE_COMMA
	mov eax, ebx
	PICKDSP ecx, 16
	PICKDSP ebx, 12
	PICKDSP edx, 8
	PICKDSP edi, 4
	PICKDSP esi, 0
	PUTDSP edx, 16
	PUTDSP edi, 12
	PUTDSP esi, 8
	PUTDSP eax, 4
	PUTDSP ecx, 0
	ret
	defword_end

	defword "?dup", 0, WORD_QDUP, WORD_INLINE_COMMA
	test ebx, ebx
	if nz
		PUSHDSP ebx
	endif
	ret
	defword_end

	defword "!?dup", 0, WORD_NQDUP, WORD_INLINE_COMMA
	test ebx, ebx
	if z
		PUSHDSP ebx
	endif
	ret
	defword_end

	defword "nip", 0, WORD_NIP, WORD_INLINE_COMMA
	ADDDSP 4
	ret
	defword_end

	defword "tuck", 0, WORD_TUCK, WORD_INLINE_COMMA
	PICKDSP eax, 0
	PUTDSP ebx, 0
	PUSHDSP eax
	ret
	defword_end

	defword "pick", 0, WORD_PICK, WORD_INLINE_COMMA
	mov ebx, [ebp + (ebx * 4)]
	ret
	defword_end

	defword "2tuck", 0, WORD_TUCK2, WORD_INLINE_COMMA
	PICKDSP eax, 0
	PICKDSP ecx, 4
	PICKDSP edx, 8
	ADDDSP -8
	PUTDSP eax, 0
	PUTDSP ecx, 4
	PUTDSP edx, 8
	PUTDSP ebx, 12
	PUTDSP eax, 16
	ret
	defword_end

	defword "2nip", 0, WORD_NIP2, WORD_INLINE_COMMA
	PICKDSP eax, 0
	ADDDSP 8
	PUTDSP eax, 0
	ret
	defword_end

	defword "2over", 0, WORD_OVER2, WORD_INLINE_COMMA
	ADDDSP -8
	PUTDSP ebx, 4
	PICKDSP ebx, 16
	PUTDSP ebx, 0
	PICKDSP ebx, 12
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; return stack ordering words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword ">r", 0, WORD_TOR, WORD_INLINE_COMMA
	TORSP
	ret
	defword_end

	defword "r>", 0, WORD_FROMR, WORD_INLINE_COMMA
	FROMRSP
	ret
	defword_end

	defword "2>r", 0, WORD_TOR2, WORD_INLINE_COMMA
	ADDRSP -8
	PICKDSP ecx, 0
	PUTRSP ebx, 0
	PUTRSP ecx, 4
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "2r>", 0, WORD_FROMR2, WORD_INLINE_COMMA
	ADDDSP -8
	PUTDSP ebx, 4
	PICKRSP ebx, 0
	PICKRSP ecx, 4
	PUTDSP ecx, 0
	ADDRSP 8
	ret
	defword_end

	defword "rsp@", 0, WORD_RSPFETCH, WORD_INLINE_COMMA
	PUSHDSP ebx
	GETRSP ebx
	ret
	defword_end

	defword "r@", 0, WORD_RFETCH, WORD_INLINE_COMMA
	PUSHDSP ebx
	PICKRSP ebx, 0
	ret
	defword_end

	defword "r!", 0, WORD_RSTORE, WORD_INLINE_COMMA
	PUTRSP ebx, 0
	POPDSP ebx
	ret
	defword_end

	defword "2r@", 0, WORD_RFETCH2, WORD_INLINE_COMMA
	ADDDSP -8
	PUTDSP ebx, 4
	PICKRSP ebx, 4
	PICKRSP ecx, 0
	PUTDSP ecx, 0
	ret
	defword_end

	defword "rsp!", 0, WORD_RSPSTORE, WORD_INLINE_COMMA
	SETRSP ebx
	POPDSP ebx
	ret
	defword_end

	defword "rdrop", 0, WORD_RDROP, WORD_INLINE_COMMA
	ADDRSP 4
	ret
	defword_end

	defword "2rdrop", 0, WORD_RDROP2, WORD_INLINE_COMMA
	ADDRSP 8
	ret
	defword_end

	defword "n>r", 0, WORD_NTOR, WORD_CALL_COMMA
	PUSHDSP ebx
	PICKRSP eax, 0
	mov ecx, ebx
	inc ecx
	neg ebx
	lea esp, [esp + (ebx * 4)]
	mov esi, ebp
	mov edi, esp
	rep movsd
	mov ebp, esi
	POPDSP ebx
	jmp eax
	defword_end

	defword "nr>", 0, WORD_NFROMR, WORD_CALL_COMMA
	PUSHDSP ebx
	POPRSP eax
	PICKRSP ebx, 0
	inc ebx
	mov ecx, ebx
	neg ebx
	lea ebp, [ebp + (ebx * 4)]
	mov esi, esp
	mov edi, ebp
	rep movsd
	mov esp, esi
	POPDSP ebx
	jmp eax
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; memory fetch and store words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "!", 0, WORD_STORE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	mov [ebx], eax
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "@", 0, WORD_FETCH, WORD_INLINE_COMMA
	mov ebx, [ebx]
	ret
	defword_end

	defword "+!", 0, WORD_ADDSTORE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	add [ebx], eax
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "-!", 0, WORD_SUBSTORE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	sub [ebx], eax
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "c!", 0, WORD_STOREBYTE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	mov [ebx], al
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "c+!", 0, WORD_ADDBYTE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	add [ebx], al
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "c@", 0, WORD_FETCHBYTE, WORD_INLINE_COMMA
	mov eax, ebx
	xor ebx, ebx
	mov bl, [eax]
	ret
	defword_end

	defword "w!", 0, WORD_STORESHORT, WORD_INLINE_COMMA
	PICKDSP eax, 0
	mov [ebx], ax
	PICKDSP ebx, 4
	ADDDSP 8
	ret
	defword_end

	defword "w@", 0, WORD_FETCHSHORT, WORD_INLINE_COMMA
	mov eax, ebx
	xor ebx, ebx
	mov bx, [eax]
	ret
	defword_end

	defword "2!", 0, WORD_STORE2, WORD_INLINE_COMMA
	PICKDSP ecx, 4
	PICKDSP edx, 0
	mov [ebx + 4], ecx
	mov [ebx], edx
	PICKDSP ebx, 8
	ADDDSP 12
	ret
	defword_end

	defword "2@", 0, WORD_FETCH2, WORD_INLINE_COMMA
	ADDDSP -4
	mov ecx, [ebx +4]
	mov ebx, [ebx]
	PUTDSP ecx, 0
	ret
	defword_end

	defword "blank", 0, WORD_BLANK, WORD_CALL_COMMA
	mov ecx, ebx
	PICKDSP ebx, 4
	PICKDSP edi, 0
	ADDDSP 8
	mov eax, 0x20
	rep stosb
	ret
	defword_end

	defword "erase", 0, WORD_ERASE, WORD_CALL_COMMA
	mov ecx, ebx
	PICKDSP ebx, 4
	PICKDSP edi, 0
	ADDDSP 8
	xor eax, eax
	rep stosb
	ret
	defword_end

	defword "fill", 0, WORD_FILL, WORD_CALL_COMMA
	mov eax, ebx
	PICKDSP ebx, 8
	PICKDSP edi, 4
	PICKDSP ecx, 0
	ADDDSP 12
	rep stosb
	ret
	defword_end

	defword "cmove>", 0, WORD_CMOVEB, WORD_CALL_COMMA
	mov ecx, ebx
	PICKDSP ebx, 8
	PICKDSP esi, 4
	PICKDSP edi, 0
	ADDDSP 12
	lea esi, [esi + ecx - 1]
	lea edi, [edi + ecx - 1]
	std
	rep movsb
	cld
	ret
	defword_end

	defword "cmove", 0, WORD_CMOVE, WORD_CALL_COMMA
	mov ecx, ebx
	PICKDSP ebx, 8
	PICKDSP esi, 4
	PICKDSP edi, 0
	ADDDSP 12
	rep movsb
	ret
	defword_end

	defword "move", 0, WORD_MOVE, WORD_CALL_COMMA
	mov ecx, ebx
	PICKDSP ebx, 8
	PICKDSP esi, 4
	PICKDSP edi, 0
	ADDDSP 12
	cmp esi, edi
	if a
		rep movsb
	else
		lea esi, [esi + ecx -1]
		lea edi, [edi + ecx -1]
		std
		rep movsb
		cld
	endif
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; single precision alu words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "+", 0, WORD_ADD, WORD_INLINE_COMMA
	add ebx, [ebp]
	ADDDSP 4
	ret
	defword_end

	defword "-", 0, WORD_SUB, WORD_INLINE_COMMA
	mov eax, ebx
	POPDSP ebx
	sub ebx, eax
	ret
	defword_end

	defword "*", 0, WORD_MULL, WORD_INLINE_COMMA
	imul ebx, [ebp]
	ADDDSP 4
	ret
	defword_end

	defword "/", 0, WORD_DIV, WORD_INLINE_COMMA
	POPDSP eax
	cdq
	idiv ebx
	mov ebx, eax
	ret
	defword_end

	defword "mod", 0, WORD_MOD, WORD_INLINE_COMMA
	POPDSP eax
	cdq
	idiv ebx
	mov ebx, edx
	ret
	defword_end

	defword "1+", 0, WORD_INCR, WORD_INLINE_COMMA
	add ebx, byte 1
	ret
	defword_end

	defword "1-", 0, WORD_DECR, WORD_INLINE_COMMA
	sub ebx, byte 1
	ret
	defword_end

	defword "4+", 0, WORD_INCR4, WORD_INLINE_COMMA
	add ebx, byte 4
	ret
	defword_end

	defword "4-", 0, WORD_DECR4, WORD_INLINE_COMMA
	sub ebx, byte 4
	ret
	defword_end

	defword "2+", 0, WORD_INCR2, WORD_INLINE_COMMA
	add ebx, byte 2
	ret
	defword_end

	defword "2-", 0, WORD_DECR2, WORD_INLINE_COMMA
	sub ebx, byte 2
	ret
	defword_end

	defword "2*", 0, WORD_TWOMUL, WORD_INLINE_COMMA
	shl ebx, byte 1
	ret
	defword_end

	defword "2/", 0, WORD_TWODIV, WORD_INLINE_COMMA
	sar ebx, byte 1
	ret
	defword_end

	defword "abs", 0, WORD_ABS, WORD_INLINE_COMMA
	mov eax, ebx
	sar eax, byte 31
	add ebx, eax
	xor ebx, eax
	ret
	defword_end

	defword "min", 0, WORD_MIN, WORD_INLINE_COMMA
	POPDSP eax
	cmp ebx, eax
	if g
		mov ebx, eax
	endif
	ret
	defword_end

	defword "max", 0, WORD_MAX, WORD_INLINE_COMMA
	POPDSP eax
	cmp ebx, eax
	if l
		mov ebx, eax
	endif
	ret
	defword_end

	defword "lshift", 0, WORD_LSHIFT, WORD_INLINE_COMMA
	mov ecx, ebx
	POPDSP ebx
	shl ebx, cl
	ret
	defword_end

	defword "rshift", 0, WORD_RSHIFT, WORD_INLINE_COMMA
	mov ecx, ebx
	POPDSP ebx
	shr ebx, cl
	ret
	defword_end

	defword "and", 0, WORD_AND, WORD_INLINE_COMMA
	and ebx, [ebp]
	ADDDSP 4
	ret
	defword_end

	defword "or", 0, WORD_OR, WORD_INLINE_COMMA
	or ebx, [ebp]
	ADDDSP 4
	ret
	defword_end

	defword "xor", 0, WORD_XOR, WORD_INLINE_COMMA
	xor ebx, [ebp]
	ADDDSP 4
	ret
	defword_end

	defword "negate", 0, WORD_NEGATE, WORD_INLINE_COMMA
	neg ebx
	ret
	defword_end

	defword "invert", 0, WORD_INVERT, WORD_INLINE_COMMA
	not ebx
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; single precision comparision words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "=", 0, WORD_EQ, WORD_INLINE_COMMA
	cmp [ebp], ebx
	sete bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "<>", 0, WORD_NE, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setne bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "<", 0, WORD_LT, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setl bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword ">", 0, WORD_GT, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setg bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "u<", 0, WORD_ULT, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setb bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "u>", 0, WORD_UGT, WORD_INLINE_COMMA
	cmp [ebp], ebx
	seta bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "u<=", 0, WORD_ULTEQ, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setbe bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "u>=", 0, WORD_UGTEQ, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setae bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "<=", 0, WORD_LTEQ, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setle bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword ">=", 0, WORD_GTEQ, WORD_INLINE_COMMA
	cmp [ebp], ebx
	setge bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0=", 0, WORD_ZEQ, WORD_INLINE_COMMA
	test ebx, ebx
	setz bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0<>", 0, WORD_ZNE, WORD_INLINE_COMMA
	test ebx, ebx
	setnz bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0<", 0, WORD_ZLT, WORD_INLINE_COMMA
	test ebx, ebx
	setl bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0>", 0, WORD_ZGT, WORD_INLINE_COMMA
	test ebx, ebx
	setg bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0<=", 0, WORD_ZLTEQ, WORD_INLINE_COMMA
	test ebx, ebx
	setle bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "0>=", 0, WORD_ZGTEQ, WORD_INLINE_COMMA
	test ebx, ebx
	setge bl
	movzx ebx, bl
	neg ebx
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; double precision ALU words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "s>d", 0, WORD_STOD, WORD_INLINE_COMMA
	mov eax, ebx
	cdq
	PUSHDSP eax
	mov ebx, edx
	ret
	defword_end

	defword "d>s", 0, WORD_DTOS, WORD_INLINE_COMMA
	POPDSP ebx
	ret
	defword_end

	defword "d+", 0, WORD_DPLUS, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP edx, 4
	PICKDSP eax, 0
	add eax, ecx
	adc ebx, edx
	PUTDSP eax, 8
	ADDDSP 8
	ret
	defword_end

	defword "d-", 0, WORD_DMINUS, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP edx, 4
	PICKDSP eax, 0
	sub ecx, eax
	sbb edx, ebx
	PUTDSP ecx, 8
	mov ebx, edx
	ADDDSP 8
	ret
	defword_end

	defword "d2*", 0, WORD_D2STAR, WORD_INLINE_COMMA
	PICKDSP eax, 0
	shl eax, 1
	rcl ebx, 1
	PUTDSP eax, 0
	ret
	defword_end

	defword "d2/", 0, WORD_D2SLASH, WORD_INLINE_COMMA
	PICKDSP eax, 0
	sar ebx, 1
	rcr eax, 1
	PUTDSP eax, 0
	ret
	defword_end

	defword "*/", 0, WORD_MULDIV, WORD_INLINE_COMMA
	PICKDSP edx, 4
	PICKDSP eax, 0
	imul edx
	idiv ebx
	mov ebx, eax
	ADDDSP 8
	ret
	defword_end

	defword "*/mod", 0, WORD_STARSMOD, WORD_INLINE_COMMA
	PICKDSP edx, 4
	PICKDSP eax, 0
	imul edx
	idiv ebx
	PUTDSP edx, 4
	ADDDSP 4
	mov ebx, eax
	ret
	defword_end

	defword "/mod", 0, WORD_DIVMOD, WORD_INLINE_COMMA
	PICKDSP eax, 0
	cdq
	idiv ebx
	PUTDSP edx, 0
	mov ebx, eax
	ret
	defword_end

	defword "dnegate", 0, WORD_DNEGATE, WORD_INLINE_COMMA
	PICKDSP eax, 0
	not eax
	not ebx
	add eax, 1
	adc ebx, 0
	PUTDSP eax, 0
	ret
	defword_end

	defword "dabs", 0, WORD_DABS, WORD_INLINE_COMMA
	test ebx, ebx
	if l
		PICKDSP eax, 0
		not eax
		not ebx
		add eax, 1
		adc ebx, 0
		PUTDSP eax, 0
	endif
	ret
	defword_end

	defword "dmax", 0, WORD_DMAX, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP edx, 4
	PICKDSP eax, 0
	ADDDSP 8
	mov esi, ecx
	mov edi, edx
	sub esi, eax
	sbb edi, ebx
	if l
		PUTDSP eax, 0
	else
		mov ebx, edx
	endif
	ret
	defword_end

	defword "dmin", 0, WORD_DMIN, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP edx, 4
	PICKDSP eax, 0
	ADDDSP 8
	mov esi, ecx
	mov edi, edx
	sub esi, eax
	sbb edi, ebx
	if ge
		PUTDSP eax, 0
	else
		mov ebx, edx
	endif
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; double precision comparision words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "d0=", 0, WORD_DZEQ, WORD_INLINE_COMMA
	or ebx, [ebp]
	setz bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "d0<>", 0, WORD_DZNEQ, WORD_INLINE_COMMA
	or ebx, [ebp]
	setnz bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "d0<", 0, WORD_DZLT, WORD_INLINE_COMMA
	test ebx, ebx
	setl bl
	ADDDSP 4
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "d=", 0, WORD_DEQ, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP eax, 4
	sub ecx, [ebp]
	sbb eax, ebx
	setz bl
	ADDDSP 12
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "d<>", 0, WORD_DNEQ, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP eax, 4
	sub ecx, [ebp]
	sbb eax, ebx
	setnz bl
	ADDDSP 12
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "d<", 0, WORD_DLT, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP eax, 4
	sub ecx, [ebp]
	sbb eax, ebx
	setl bl
	ADDDSP 12
	movzx ebx, bl
	neg ebx
	ret
	defword_end

	defword "du<", 0, WORD_DULT, WORD_INLINE_COMMA
	PICKDSP ecx, 8
	PICKDSP eax, 4
	sub ecx, [ebp]
	sbb eax, ebx
	setb bl
	ADDDSP 12
	movzx ebx, bl
	neg ebx
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;
; mixed precision words
;;;;;;;;;;;;;;;;;;;;;;;

	defword "m+", 0, WORD_MPLUS, WORD_INLINE_COMMA
	PICKDSP eax, 4
	PICKDSP edx, 0
	add eax, ebx
	adc edx, 0
	PUTDSP eax, 4
	mov ebx, edx
	ADDDSP 4
	ret
	defword_end

	defword "m-", 0, WORD_MMINUS, WORD_INLINE_COMMA
	PICKDSP eax, 4
	PICKDSP edx, 0
	sub eax, ebx
	sbb edx, 0
	PUTDSP eax, 4
	mov ebx, edx
	ADDDSP 4
	ret
	defword_end

	defword "m*", 0, WORD_MULSTAR, WORD_INLINE_COMMA
	PICKDSP eax, 0
	imul ebx
	PUTDSP eax, 0
	mov ebx, edx
	ret
	defword_end

	defword "m/", 0, WORD_MSLASH, WORD_INLINE_COMMA
	PICKDSP eax, 4
	PICKDSP edx, 0
	idiv ebx
	mov ebx, eax
	ADDDSP 8
	ret
	defword_end

	defword "um*", 0, WORD_UMULSTAR, WORD_INLINE_COMMA
	PICKDSP eax, 0
	mul ebx
	PUTDSP eax, 0
	mov ebx, edx
	ret
	defword_end

	defword "um/mod", 0, WORD_UMDIVMOD, WORD_INLINE_COMMA
	PICKDSP eax, 4
	PICKDSP edx, 0
	div ebx
	PUTDSP edx, 4
	mov ebx, eax
	ADDDSP 4
	ret
	defword_end

	defword "fm/mod", 0, WORD_FMDIVMOD, WORD_INLINE_COMMA
	PICKDSP edx, 0
	PICKDSP eax, 4
	mov ecx, ebx
	ADDDSP 4
	xor ecx, edx
	idiv ebx
	test ecx, ecx
	if s
		test edx, edx
		if nz
			dec eax
			add edx, ebx
		endif
	endif
	PUTDSP edx, 0
	mov ebx, eax
	ret
	defword_end

	defword "sm/rem", 0, WORD_SMDIVREM, WORD_INLINE_COMMA
	PICKDSP eax, 4
	PICKDSP edx, 0
	idiv ebx
	PUTDSP edx, 4
	mov ebx, eax
	ADDDSP 4
	ret
	defword_end

	defword "u/mod", 0, WORD_UDIVMOD, WORD_INLINE_COMMA
	xor edx, edx
	PICKDSP eax, 0
	div ebx
	PUTDSP edx, 0
	mov ebx, eax
	ret
	defword_end

	defword "dm*", 0, WORD_DMULSTAR, WORD_CALL_COMMA
	call WORD_TUCK
	call WORD_MULL
	TORSP
	call WORD_UMULSTAR
	FROMRSP
	call WORD_ADD
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;
; control flow words
;;;;;;;;;;;;;;;;;;;;

	defword "branch", 0, WORD_BRANCH, WORD_INLINE_COMMA
i_jmp:
	jmp strict near i_ret
	ret
	defword_end

	defword "0branch", 0, WORD_ZBRANCH, WORD_INLINE_COMMA
	mov eax, ebx
	POPDSP ebx
	test eax, eax
	jz strict near i_jmp
	ret
	defword_end

	defword "exit", 0, WORD_EXIT, WORD_EXIT_COMMA
i_ret:
	ret
	defword_end

	defword "exit,", 0, WORD_EXIT_COMMA, WORD_CALL_COMMA
	mov edi, [var_WORD_DP]
	sub edi, 5
	cmp edi, [lastcall]	; are we just after a call instruction ?
	if z
		mov al, [i_jmp]
		mov [edi], al	; change it to a jmp
	endif
	mov edi, [var_WORD_DP]
	mov al, [i_ret]
	stosb
	mov [var_WORD_DP], edi
	POPDSP ebx
	ret
	defword_end

	defword "execute", 0, WORD_EXECUTE, WORD_CALL_COMMA
	mov eax, ebx	; Get xt into eax
	POPDSP ebx		; After xt runs its ret will continue executing the current word.
	jmp eax			; and jump to it.
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;
; terminal input words
;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "read-char", 0, WORD_READCHAR, WORD_CALL_COMMA
	mov ecx, var_WORD_CHARBUF	; 2nd param: buffer
	mov edx, 1					; 3rd param: max length
	push edx
	push ecx
	push ebx
	mov eax, SYS_read			; syscall: read
	call _syscall
	add esp, 12
	xor ebx, ebx
	test eax, eax
	if be
		mov ebx, -1
	endif
	ret
	defword_end

	defword "read-line", 0, WORD_READLINE, WORD_CALL_COMMA
	call WORD_NROT
	call WORD_OVER
	call WORD_ADD
	call WORD_OVER		; ( fd start end cur )
readline_l1:
	PICKDSP eax, 0
	cmp ebx, eax
	jz readline_l4
	PUSHDSP ebx
	PICKDSP ebx, 12
	call WORD_READCHAR
	test ebx, ebx
	jz readline_l2
	call WORD_DROP
	call WORD_DROP2
	call WORD_DROP2
	LOADTOS 0
	LOADTOS 0
	LOADTOS -1
	jmp readline_l5
readline_l2:
	mov ebx, [var_WORD_CHARBUF]
	cmp ebx, 10			; LF
	jz readline_l3
	call WORD_OVER
	call WORD_STOREBYTE
	call WORD_INCR
	jmp readline_l1
readline_l3:
	call WORD_DROP
readline_l4:
	call WORD_NIP
	call WORD_SWAP
	call WORD_SUB
	call WORD_NIP
	LOADTOS -1
	LOADTOS 0
readline_l5:
	ret
	defword_end

	defword "key", 0, WORD_KEY, WORD_CALL_COMMA
	PUSHDSP ebx
	xor ebx, ebx		; stdin
	call WORD_READCHAR
	mov ebx, [var_WORD_CHARBUF]
	ret
	defword_end

	defword "accept", 0, WORD_ACCEPT, WORD_CALL_COMMA
	call WORD_OVER
	call WORD_ADD
	call WORD_OVER	; ( start end cur )
accept_l1:
	call WORD_KEY
	cmp ebx, 127	; BS
	jz accept_l2
	cmp ebx, 10		; LF
	jz accept_l3
	call WORD_OVER	; ( start end cur key cur )
	call WORD_STOREBYTE
	call WORD_INCR	; ( start end cur' )
	PICKDSP eax, 0
	cmp ebx, eax
	jz accept_l4
	jmp accept_l1
accept_l2:
	PICKDSP eax, 4	; ( start end cur' )
	cmp ebx, eax
	jz accept_l1
	call WORD_DECR
	jmp accept_l1
accept_l3:
	call WORD_DROP	; ( start end cur' )
accept_l4:
	call WORD_NIP
	call WORD_SWAP
	call WORD_SUB
	ret
	defword_end

	defword "tabs>spaces", 0, WORD_TABSTOSPACES, WORD_CALL_COMMA
	mov ecx, ebx
	POPDSP esi
	test ecx, ecx
	if nz
		repeat
			lodsb
			cmp al, 9	;TAB
			if z
				mov byte [esi - 1], ' '
			endif
			dec ecx
		until z
	endif
	POPDSP ebx
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;
; terminal output words
;;;;;;;;;;;;;;;;;;;;;;;

	defword "type-fd", 0, WORD_TYPE_FD, WORD_CALL_COMMA
	PICKDSP edx, 0			; 3rd param: length of string
	PICKDSP ecx, 4			; 2nd param: address of string
	ADDDSP 8				; 1st param: FD in ebx
	mov eax, SYS_write		; write syscall
	push edx
	push ecx
	push ebx
	call _syscall
	add esp, 12
	POPDSP ebx
	ret
	defword_end

	defword "type", 0, WORD_TYPE, WORD_CALL_COMMA
	LOADTOS 1				; stdout
	call WORD_TYPE_FD
	ret
	defword_end

	defword "emit", 0, WORD_EMIT, WORD_CALL_COMMA
	mov [emit_scratch], bl	; write needs the address of the byte to write
	mov ebx, emit_scratch
	LOADTOS 1
	call WORD_TYPE
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;
; system call words
;;;;;;;;;;;;;;;;;;;

	defword "syscall", 0, WORD_SYSCALL, WORD_CALL_COMMA
	pop eax
	mov [syscallret], eax	; save return address
	mov eax, ebx			; System call number (see <asm/unistd.h>)
	call _syscall
	mov ebx, eax			; Result (negative for -errno)
	jmp [syscallret]		; return to caller
	defword_end

	defword "lsyscall", 0, WORD_LSYSCALL, WORD_CALL_COMMA
	pop eax
	mov [syscallret], eax	; save return address
	mov eax, ebx			; System call number (see <asm/unistd.h>)
	call _lsyscall
	PUSHDSP eax
	mov ebx, edx			; Result (negative for -errno)
	jmp [syscallret]		; return to caller
	defword_end

;;;;;;;;;;;;;;
; string words
;;;;;;;;;;;;;;

	defword "count", 0, WORD_COUNT, WORD_CALL_COMMA
	xor eax, eax
	mov al, [ebx]
	inc ebx
	LOADTOS eax
	ret
	defword_end

	defword "-trailing", 0, WORD_TRAILING, WORD_CALL_COMMA
	test ebx, ebx
	if nz
		PICKDSP esi, 0
		mov ecx, ebx
		add esi, ebx
		dec esi
		std
	trailing_l1:
		lodsb
		cmp al, ' '
		if be
			loop trailing_l1
		endif
		mov ebx, ecx
		cld
	endif
	ret
	defword_end

	defword "/string", 0, WORD_SSTRING, WORD_CALL_COMMA
	mov eax, ebx
	POPDSP ebx
	PICKDSP ecx, 0
	sub ebx, eax
	add ecx, eax
	PUTDSP ecx, 0
	ret
	defword_end

	defword "compare", 0, WORD_COMPARE, WORD_CALL_COMMA
	PICKDSP esi, 8
	PICKDSP edx, 4
	PICKDSP edi, 0
	ADDDSP 12
	mov ecx, ebx
	cmp edx, ebx
	if be
		mov ecx, edx
	endif
	test ecx, ecx		; ecx lowest length
	jnz compare_l2
	cmp edx, ebx
	jz compare_l3		; both are 0 length
	jmp compare_l4		; otherwise the longest wins
compare_l2:
	cmpsb
	jnz compare_l4		; chars not same
	loop compare_l2
	cmp edx, ebx		; all chars same
	jnz compare_l4		; strings not same size
compare_l3:
	xor ebx, ebx		; same
	jmp compare_l7
compare_l4:
	ja compare_l6
compare_l5:
	mov ebx, -1
	jmp compare_l7
compare_l6:
	mov ebx, 1
compare_l7:
	ret
	defword_end

	defword "icompare", 0, WORD_COMPAREI, WORD_CALL_COMMA
	PICKDSP esi, 8
	PICKDSP edx, 4
	PICKDSP edi, 0
	ADDDSP 12
	mov ecx, ebx
	cmp edx, ebx
	if be
		mov ecx, edx
	endif
	test ecx, ecx		; ecx lowest length
	jnz comparei_l2
	cmp edx, ebx
	jz comparei_l3		; both are 0 length
	jmp comparei_l4		; otherwise the longest wins
comparei_l2:
	mov al, [esi]
	mov ah, [edi]
	to_lower al
	to_lower ah
	cmp ah, al
	jnz comparei_l4		; chars not same
	inc edi
	inc esi
	loop comparei_l2
	cmp edx, ebx		; all chars same
	jnz comparei_l4		; strings not same size
comparei_l3:
	xor ebx, ebx		; same
	jmp comparei_l7
comparei_l4:
	ja comparei_l6
comparei_l5:
	mov ebx, -1
	jmp comparei_l7
comparei_l6:
	mov ebx, 1
comparei_l7:
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary searching words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "find", 0, WORD_FIND, WORD_CALL_COMMA
	call WORD_DUP
	call WORD_COUNT
	call WORD_FIND_DICT
	test ebx, ebx
	if nz
		mov dl, [ebx + H_NSIZE]
		call WORD_TCFA
		LOADTOS 1
		and edx, F_IMMED
		if z
			neg ebx
		endif
		call WORD_ROT
		call WORD_DROP
	endif
	ret
	defword_end

	defword "(find)", 0, WORD_FIND_DICT, WORD_CALL_COMMA
	mov ecx, ebx			; ecx = length
	POPDSP edi				; edi = address
	PUSHRSP ecx
	mov esi, edi
	call strhashi
	and ebx, NUM_HASH_CHAINS-1
	mov esi, hash_buckets
	mov edx, [esi + (ebx * 4)]
	POPRSP ecx				; edx can now scan back through this hash chain
findd_l1:
	test edx, edx			; NULL pointer?  (end of the linked list)
	je findd_l4
	xor eax, eax
	mov al, [edx + H_NSIZE]	; al = flags+length field
	and al, (F_HIDDEN|F_LENMASK)	; al = name length
	cmp al, cl				; Length is the same?
	jne findd_l2
	PUSHRSP ecx				; Save the length
	PUSHRSP edi				; Save the address (repe cmpsb will move this pointer)
	lea esi, [edx + H_NAME]	; Dictionary string we are checking against.
	call strcmpi
	POPRSP edi
	POPRSP ecx
	jne findd_l2			; Not the same.
	mov ebx, edx
	ret
findd_l2:
	mov edx, [edx + H_HLINK]	; Move back through the link field to the previous word
	jmp findd_l1			; .. and loop.
findd_l4:
	xor ebx, ebx			; Return zero to indicate not found.
	ret
	defword_end

	defword ">cfa", 0, WORD_TCFA, WORD_CALL_COMMA
	add ebx, H_NSIZE
	mov al, [ebx]			; Load flags+len into al.
	inc ebx					; skip flags+len byte.
	and eax, F_LENMASK		; Just the length, not the flags.
	add ebx, eax			; skip the name
	add ebx, XT_SIZE		; skip to the xt
	ret
	defword_end

	defword "(bucket)", 0, WORD_BUCKET, WORD_CALL_COMMA
	mov ecx, ebx		; ecx = length
	POPDSP ebx			; ebx = address of name
	PUSHRSP esi
	mov esi, ebx
	call strhashi
	and ebx, NUM_HASH_CHAINS-1
	mov esi, hash_buckets
	lea ebx, [esi + (ebx * 4)]
	POPRSP esi
	ret
	defword_end

	defword "unused", 0, WORD_UNUSED, WORD_CALL_COMMA
	LOADTOS forth_end
	LOADTOS [var_WORD_DP]
	call WORD_SUB
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary building words
;;;;;;;;;;;;;;;;;;;;;;;;;;;

	defword "align", 0, WORD_ALIGNDP, WORD_CALL_COMMA
	mov eax, [var_WORD_DP]
	ALIGNREG eax
	mov [var_WORD_DP], eax
	ret
	defword_end

	defword "header,", 0, WORD_HEADER_COMMA, WORD_CALL_COMMA
	mov ecx, ebx				; ecx = length
	POPDSP ebx					; ebx = address of name
	call WORD_ALIGNDP			; align header
	mov edi, [var_WORD_DP]		; edi is the address of the header
	mov eax, [var_WORD_LATEST]	; Get link pointer
	mov [edi + H_LLINK], eax	; and store it in the header.
	mov [var_WORD_LATEST], edi
	PUSHRSP ebx					; hash chain
	PUSHRSP ecx
	mov esi, ebx
	call strhashi
	and ebx, NUM_HASH_CHAINS-1
	mov esi, hash_buckets
	mov eax, [esi + (ebx * 4)]
	mov [esi + (ebx * 4)], edi
	mov [edi + H_HLINK], eax	; and store it in the header.
	POPRSP ecx
	POPRSP esi
	mov [edi + H_NSIZE], cl		; Store the length/flags byte.
	add edi, H_NAME
	call strcpyi
	mov ecx, XT_SIZE
	xor eax, eax
	rep stosb					; clear the gap till the xt
	mov [var_WORD_DP], edi
	mov long [edi + XT_COMPILE], WORD_CALL_COMMA	;compile action
	POPDSP ebx
	ret
	defword_end

	defword "lit,", 0, WORD_LIT_COMMA, WORD_CALL_COMMA
	mov esi, litc_l1
	mov edi, [var_WORD_DP]
	mov ecx, litc_l2 - litc_l1 - 4
	rep movsb
	mov [var_WORD_DP], edi
	ret
	defword_end
litc_l1:
	LOADTOS 0xBAADF00D
litc_l2:

	defword "slits", 0, WORD_SLITS, WORD_CALL_COMMA
	PUSHDSP ebx
	POPRSP esi
	xor eax, eax
	lodsb				; get the length of the string
	PUSHDSP esi			; push the address of the start of the string
	mov ebx, eax		; push length on the stack
	add esi, eax		; skip past the string
 	jmp esi
	defword_end

	defword "clits", 0, WORD_CLITS, WORD_CALL_COMMA
	FROMRSP
	xor eax, eax
	mov al, [ebx]
	lea eax, [ebx + eax + 1]
 	jmp eax
	defword_end

	defword ",", 0, WORD_COMMA, WORD_CALL_COMMA
	mov edi, [var_WORD_DP]	; DP
	mov eax, ebx
	stosd					; Store it.
	mov [var_WORD_DP], edi	; Update DP (incremented)
	POPDSP ebx
	ret
	defword_end

	defword "c,", 0, WORD_CHAR_COMMA, WORD_CALL_COMMA
	mov eax, ebx
	mov edi, [var_WORD_DP]	; DP
	stosb					; Store it.
	mov [var_WORD_DP], edi	; Update DP (incremented)
	POPDSP ebx
	ret
	defword_end

	defword ":", 0, WORD_COLON, WORD_CALL_COMMA
	call WORD_PARSENAME
	call WORD_HEADER_COMMA	; Create the dictionary entry / header
	mov eax, [var_WORD_DP]
	mov [eax + XT_BODY], eax
	call WORD_LATEST
	call WORD_FETCH
	call WORD_HIDDEN		; Make the word hidden
	call WORD_RBRAC			; Go into compile mode.
	ret
	defword_end

	defword "create", 0, WORD_CREATE, WORD_CALL_COMMA
	call WORD_PARSENAME
	call WORD_HEADER_COMMA
	mov esi, create_l1
	mov edi, [var_WORD_DP]
	PUSHRSP edi
	mov ecx, create_l4 - create_l1
	rep movsb
	mov [var_WORD_DP], edi
	mov edx, edi
	call WORD_ALIGNDP
	POPRSP eax
	mov edi, [var_WORD_DP]
	sub edx, eax
	mov [eax + create_l2 - create_l1 - 4], edi
	mov [eax + XT_BODY], edi
	mov [eax + XT_LENGTH], edx
	ret
	defword_end
create_l1:
	LOADTOS 0xBAADF00D
create_l2:
	call strict near create_l3
create_l3:
	ret
create_l4:

	defword "dodoes", 0, WORD_DODOES, WORD_CALL_COMMA
	call WORD_LATEST
	call WORD_FETCH
	call WORD_TCFA
	add ebx, create_l3 - create_l1 - 4
	POPDSP eax
	sub eax, ebx
	sub eax, 4
	mov [ebx], eax
	POPDSP ebx
	ret
	defword_end

	defword "does>", F_IMMED, WORD_DOES, WORD_CALL_COMMA
	call WORD_LIT_COMMA
	LOADTOS [var_WORD_DP]
	add ebx, 10
	call WORD_COMMA
	LOADTOS WORD_DODOES
	call WORD_COMPILE_COMMA
	LOADTOS 0
	mov bl, [does_l1]
	call WORD_CHAR_COMMA
does_l1:
	ret
	defword_end

	defword "postpone", F_IMMED, WORD_POSTPONE, WORD_CALL_COMMA
	call WORD_PARSENAME
	call WORD_FIND_DICT
	mov dl, [ebx + H_NSIZE]
	call WORD_TCFA
	and edx, F_IMMED
	if z
		call WORD_LIT_COMMA
		call WORD_COMMA
		LOADTOS WORD_COMPILE_COMMA
	endif
	jmp WORD_COMPILE_COMMA
	ret
	defword_end

	defword "call,", 0, WORD_CALL_COMMA, WORD_CALL_COMMA
	mov edi, [var_WORD_DP]
	mov [lastcall], edi	; record last location of last call
	mov esi, i_call
	movsb
	mov eax, ebx
	sub eax, 4
	sub eax, edi
	stosd
	mov [var_WORD_DP], edi
	POPDSP ebx
	ret
	defword_end

	defword "inline,", 0, WORD_INLINE_COMMA, WORD_CALL_COMMA
	mov ecx, [ebx + XT_LENGTH]
	dec ecx					; actual code length minus ret
	mov esi, ebx
	mov edi, [var_WORD_DP]
	rep movsb				; inline copy the code
	mov [var_WORD_DP], edi	; update DP
	POPDSP ebx
	ret
	defword_end

	defword "compile,", 0, WORD_COMPILE_COMMA, WORD_INLINE_COMMA
	call [ebx + XT_COMPILE]
	ret
	defword_end

	defword ";", F_IMMED, WORD_SEMICOLON, WORD_CALL_COMMA
	LOADTOS WORD_EXIT
i_call:
	call strict near WORD_COMPILE_COMMA
	call WORD_LATEST
	call WORD_FETCH
	call WORD_HIDDEN			; toggle hidden flag -- unhide the word (see below for definition).
	call WORD_LBRAC				; go back to IMMEDIATE mode.
	mov edx, ebx
	mov ebx, [var_WORD_LATEST]
	call WORD_TCFA
	mov ecx, [var_WORD_DP]
	sub ecx, ebx
	mov [ebx + XT_LENGTH], ecx	; set code size of word
	mov ebx, edx
	ret
	defword_end

	defword "immediate", 0, WORD_IMMEDIATE, WORD_CALL_COMMA
	mov edi, [var_WORD_LATEST]	; LATEST word.
	add edi, H_NSIZE			; Point to name/flags byte.
	xor byte [edi], F_IMMED		; Toggle the IMMED bit.
	ret
	defword_end

	defword "hidden", 0, WORD_HIDDEN, WORD_CALL_COMMA
	add ebx, H_NSIZE			; Point to name/flags byte.
	xor byte [ebx], F_HIDDEN	; Toggle the HIDDEN bit.
	POPDSP ebx
	ret
	defword_end

	defword "[", F_IMMED, WORD_LBRAC, WORD_CALL_COMMA
	mov long [var_WORD_STATE], 0	; Set STATE to 0.
	ret
	defword_end

	defword "]", 0, WORD_RBRAC, WORD_CALL_COMMA
	mov long [var_WORD_STATE], 1	; Set STATE to 1.
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;
; source buffer words
;;;;;;;;;;;;;;;;;;;;;

	defword "source", 0, WORD_SOURCE, WORD_CALL_COMMA
	call WORD_INHASH
	call WORD_FETCH2
	ret
	defword_end

	defword "refill", 0, WORD_REFILL, WORD_CALL_COMMA
	LOADTOS tib_buffer
	call WORD_LINESIZE	; ( tib len )
	call WORD_OVER
	call WORD_SWAP		; ( tib tib len )
	call WORD_ACCEPT	; read line into TIB
	call WORD_DUP2
	call WORD_TABSTOSPACES
	call WORD_INHASH
	call WORD_STORE2	; set as current WORD_SOURCE
	LOADTOS 0
	call WORD_TOIN
	call WORD_STORE		; set to start of buffer
	LOADTOS -1
	ret
	defword_end

	defword "isspace?", 0, WORD_ISSPACE, WORD_CALL_COMMA
	LOADTOS ' '
	call WORD_ULTEQ
	ret
	defword_end

	defword "isnotspace?", 0, WORD_ISNOTSPACE, WORD_CALL_COMMA
	call WORD_ISSPACE
	call WORD_ZEQ
	ret
	defword_end

	defword "xt-skip", 0, WORD_XTSKIP, WORD_CALL_COMMA
	TORSP
xtskip_l1:
	test ebx, ebx
	jz xtskip_l3
	call WORD_OVER
	call WORD_FETCHBYTE
	FETCHRSP
	call WORD_EXECUTE
	test ebx, ebx
	jz xtskip_l2
	mov ebx, 1
	call WORD_SSTRING
	jmp xtskip_l1
xtskip_l2:
	call WORD_DROP
xtskip_l3:
	ADDRSP 4
	ret
	defword_end

;;;;;;;;;;;;;;;;;;;;;;
; input parseing words
;;;;;;;;;;;;;;;;;;;;;;

	defword "parse-name", 0, WORD_PARSENAME, WORD_CALL_COMMA
	call WORD_SOURCE
	call WORD_TOIN
	call WORD_FETCH
	call WORD_SSTRING
	LOADTOS WORD_ISSPACE
	call WORD_XTSKIP
	call WORD_OVER
	TORSP
	LOADTOS WORD_ISNOTSPACE
	call WORD_XTSKIP
	call WORD_DUP2
	LOADTOS 1
	call WORD_MIN
	call WORD_ADD
	call WORD_SOURCE
	call WORD_DROP
	call WORD_SUB
	call WORD_TOIN
	call WORD_STORE
	call WORD_DROP
	FROMRSP
	call WORD_TUCK
	call WORD_SUB
; code to print out "P <word> CR"
;LOADTOS 80
;call WORD_EMIT
;LOADTOS 32
;call WORD_EMIT
;call WORD_DUP2
;call WORD_TYPE
;LOADTOS 10
;call WORD_EMIT
	ret
	defword_end

	defword "word-name", 0, WORD_WORDNAME, WORD_CALL_COMMA
	call WORD_PARSENAME		; ( start len )
	LOADTOS word_buf		; ( string size buf )
	call WORD_DUP2			; ( string size buf size buf )
	call WORD_STOREBYTE		; ( string size buf )
	call WORD_INCR			; ( string size buf+1 )
	call WORD_SWAP			; ( string buf+1 size )
	call WORD_CMOVE
	LOADTOS word_buf		; ( cstring )
; debug code to print out "N <word> CR"
;LOADTOS 78
;call WORD_EMIT
;LOADTOS 32
;call WORD_EMIT
;call WORD_DUP2
;call WORD_TYPE
;LOADTOS 10
;call WORD_EMIT
	ret
	defword_end

	defword "interp-name", 0, WORD_INTERPNAME, WORD_CALL_COMMA
	call WORD_PARSENAME			; ( start len )
	LOADTOS intep_name_buf		; ( string size buf )
	call WORD_DUP2				; ( string size buf size buf )
	call WORD_STOREBYTE			; ( string size buf )
	call WORD_INCR				; ( string size buf+1 )
	call WORD_SWAP				; ( string buf+1 size )
	call WORD_CMOVE
	LOADTOS intep_name_buf		;( cstring )
	ret
	defword_end

	defword "interpret", 0, WORD_INTERPRET, WORD_CALL_COMMA
	loopstart
		call WORD_INTERPNAME
		mov al, [ebx]
		test al, al
		breakif z
		; debug code to print out "I <word> CR"
		;LOADTOS 73
		;call WORD_EMIT
		;LOADTOS 32
		;call WORD_EMIT
		;call WORD_DUP
		;call WORD_COUNT
		;call WORD_TYPE
		;LOADTOS 10
		;call WORD_EMIT
		call WORD_INTERP
	loopend
	call WORD_DROP
	ret
	defword_end

	defword "interp", 0, WORD_INTERP, WORD_CALL_COMMA
	call WORD_FIND				; ( cstring 0 | xt 1 | xt | -1 )
	mov eax, ebx
	POPDSP ebx
	test eax, eax
	jz tryasnumber
	jle nonimediate
executeword:
	mov eax, ebx
	POPDSP ebx
	jmp eax
nonimediate:
	mov eax, [var_WORD_STATE]
	test eax, eax				; are we in imedeate mode ?
	jz executeword
	jmp WORD_COMPILE_COMMA		; compile xt
tryasnumber:
	call WORD_COUNT				; ( adr len )
	LOADTOS 0
	LOADTOS 0
	call WORD_SWAP2				; ( 0d addr len )
	call WORD_TOSNUMBER			; ( d addr len )
	test ebx, ebx
	jnz parseproblem
	call WORD_DROP2
	call WORD_DROP				; ( num )
	mov eax, [var_WORD_STATE]
	test eax, eax
	if nz
		call WORD_LIT_COMMA		; compile LIT
		call WORD_COMMA			; compile value
	endif
	ret
parseproblem:
	LOADTOS errmsg
	LOADTOS errmsgend - errmsg
	LOADTOS 2
	call WORD_TYPE_FD
	LOADTOS errmsgnl
	LOADTOS 1
	LOADTOS 2
	call WORD_TYPE_FD
	LOADTOS tib_buffer
	LOADTOS [var_WORD_TOIN]
	LOADTOS 2
	call WORD_TYPE_FD
	LOADTOS errmsgnl
	LOADTOS 1
	LOADTOS 2
	call WORD_TYPE_FD
	call WORD_DROP2
	call WORD_DROP2
	ret
	defword_end

	defword ">number", 0, WORD_TONUMBER, WORD_CALL_COMMA
	call WORD_OVER
	call WORD_ADD
	call WORD_SWAP			; ( ud end cur )
tonumber_l1:
	PICKDSP eax, 0
	cmp ebx, eax
	jz near tonumber_l4
	call WORD_DUP
	call WORD_FETCHBYTE		; ( ud end cur char )
	to_lower ebx
	sub ebx, byte '0'
	jb tonumber_l3			; < '0'?
	cmp ebx, byte 10
	jb tonumber_l2			; <= '9' ?
	sub ebx, byte 'a' - '0'
	jb tonumber_l3			; < 'a' ?
	add ebx, byte 10
tonumber_l2:
	cmp ebx, [var_WORD_BASE]
	jge tonumber_l3			; >= WORD_BASE ?
	TORSP
	call WORD_SWAP2			; ( end cur ud )
	LOADTOS [var_WORD_BASE]
	call WORD_DMULSTAR
	FROMRSP
	call WORD_MPLUS			; ( end cur ud' )
	call WORD_SWAP2
	call WORD_INCR			; ( ud' end cur' )
	jmp tonumber_l1
tonumber_l3:
	call WORD_DROP
tonumber_l4:
	call WORD_SWAP
	call WORD_OVER
	call WORD_SUB			; ( ud' c-addr u2 )
	ret
	defword_end

	defword ">snumber", 0, WORD_TOSNUMBER, WORD_CALL_COMMA
	test ebx, ebx
	if nz
		PICKDSP eax, 0
		mov cl, [eax]
		cmp cl, '-'
		jnz WORD_TONUMBER	; not '-'
		inc eax
		PUTDSP eax, 0
		dec ebx
		call WORD_TONUMBER
		call WORD_SWAP2
		call WORD_DNEGATE
		call WORD_SWAP2
	endif
	ret
	defword_end

;;;;;;;;;;;
; tick word
;;;;;;;;;;;

	defword "ticks", 0, WORD_TICKS, WORD_CALL_COMMA
	sub ebp, byte 8
	rdtsc
	mov [byte ebp -4], ebx
	mov [ebp], eax
	mov ebx, edx
	ret
	defword_end

;;;;;;;;;;;
; test word
;;;;;;;;;;;

	defword "test", 0, WORD_TEST, WORD_CALL_COMMA
	ret
	defword_end

;;;;;;;;;;;;;;;;;
; read/write data
;;;;;;;;;;;;;;;;;

	align 4
syscallret:
	; return address saved by syscall
	dd 0
lastcall:
	; last call layed down by compiler
	dd 0

tib_buffer:
	; keyboard input buffer
	times MAX_LINE_SIZE db 0
word_buf:
	; static buffer where WORD returns. Subsequent calls
	; overwrite this buffer.
	times MAX_LINE_SIZE db 0
intep_name_buf:
	; static buffer where INTERPNAME returns. Subsequent calls
	; overwrite this buffer.
	times MAX_LINE_SIZE db 0
emit_scratch:
	; scratch used by EMIT
	db 0
errmsg:
	db "PARSE ERROR:"
errmsgend:
errmsgnl:
	db 10
bootfile:
	db "forth.f"
	db 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dictionary hash table (64)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	align 4
hash_buckets:
	dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	dd 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; addresses of all built in dictionary words.
; this ends up as part of the user space after booting !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	align 4
dictionary_start:
	dd dic_WORD_ABS
	dd dic_WORD_ACCEPT
	dd dic_WORD_ADD
	dd dic_WORD_ADDBYTE
	dd dic_WORD_ADDSTORE
	dd dic_WORD_ALIGNDP
	dd dic_WORD_AND
	dd dic_WORD_BASE
	dd dic_WORD_BLANK
	dd dic_WORD_BLK
	dd dic_WORD_BRANCH
	dd dic_WORD_BUCKET
	dd dic_WORD_CALL_COMMA
	dd dic_WORD_CHARBUF
	dd dic_WORD_CHAR_COMMA
	dd dic_WORD_CLITS
	dd dic_WORD_CMOVE
	dd dic_WORD_CMOVEB
	dd dic_WORD_COLON
	dd dic_WORD_COMMA
	dd dic_WORD_COMPARE
	dd dic_WORD_COMPAREI
	dd dic_WORD_COMPILE_COMMA
	dd dic_WORD_COUNT
	dd dic_WORD_CREATE
	dd dic_WORD_D2SLASH
	dd dic_WORD_D2STAR
	dd dic_WORD_DABS
	dd dic_WORD_DECR
	dd dic_WORD_DECR2
	dd dic_WORD_DECR4
	dd dic_WORD_DEQ
	dd dic_WORD_DIV
	dd dic_WORD_DIVMOD
	dd dic_WORD_DLT
	dd dic_WORD_DMAX
	dd dic_WORD_DMIN
	dd dic_WORD_DMINUS
	dd dic_WORD_DMULSTAR
	dd dic_WORD_DNEGATE
	dd dic_WORD_DNEQ
	dd dic_WORD_DODOES
	dd dic_WORD_DOES
	dd dic_WORD_DP
	dd dic_WORD_DPLUS
	dd dic_WORD_DROP
	dd dic_WORD_DROP2
	dd dic_WORD_DSPFETCH
	dd dic_WORD_DSPSTORE
	dd dic_WORD_DTOS
	dd dic_WORD_DULT
	dd dic_WORD_DUP
	dd dic_WORD_DUP2
	dd dic_WORD_DZEQ
	dd dic_WORD_DZLT
	dd dic_WORD_DZNEQ
	dd dic_WORD_EMIT
	dd dic_WORD_EQ
	dd dic_WORD_ERASE
	dd dic_WORD_EXECUTE
	dd dic_WORD_EXIT
	dd dic_WORD_FETCH
	dd dic_WORD_FETCH2
	dd dic_WORD_FETCHBYTE
	dd dic_WORD_FETCHSHORT
	dd dic_WORD_FILL
	dd dic_WORD_FIND
	dd dic_WORD_FIND_DICT
	dd dic_WORD_FMDIVMOD
	dd dic_WORD_FROMR
	dd dic_WORD_FROMR2
	dd dic_WORD_GT
	dd dic_WORD_GTEQ
	dd dic_WORD_HEADER_COMMA
	dd dic_WORD_HIDDEN
	dd dic_WORD_IMMEDIATE
	dd dic_WORD_INCR
	dd dic_WORD_INCR2
	dd dic_WORD_INCR4
	dd dic_WORD_INHASH
	dd dic_WORD_INLINE_COMMA
	dd dic_WORD_INTERP
	dd dic_WORD_INTERPNAME
	dd dic_WORD_INTERPRET
	dd dic_WORD_INVERT
	dd dic_WORD_ISNOTSPACE
	dd dic_WORD_ISSPACE
	dd dic_WORD_KEY
	dd dic_WORD_LATEST
	dd dic_WORD_LBRAC
	dd dic_WORD_LINESIZE
	dd dic_WORD_LIT_COMMA
	dd dic_WORD_LSHIFT
	dd dic_WORD_LSYSCALL
	dd dic_WORD_LT
	dd dic_WORD_LTEQ
	dd dic_WORD_MAX
	dd dic_WORD_MIN
	dd dic_WORD_MMINUS
	dd dic_WORD_MOD
	dd dic_WORD_MOVE
	dd dic_WORD_MPLUS
	dd dic_WORD_MSLASH
	dd dic_WORD_MULDIV
	dd dic_WORD_MULL
	dd dic_WORD_MULSTAR
	dd dic_WORD_NE
	dd dic_WORD_NEGATE
	dd dic_WORD_NFROMR
	dd dic_WORD_NIP
	dd dic_WORD_NIP2
	dd dic_WORD_NQDUP
	dd dic_WORD_NROT
	dd dic_WORD_NTOR
	dd dic_WORD_OR
	dd dic_WORD_OVER
	dd dic_WORD_OVER2
	dd dic_WORD_O_APPEND
	dd dic_WORD_O_CREAT
	dd dic_WORD_O_EXCL
	dd dic_WORD_O_NONBLOCK
	dd dic_WORD_O_RDONLY
	dd dic_WORD_O_RDWR
	dd dic_WORD_O_TRUNC
	dd dic_WORD_O_WRONLY
	dd dic_WORD_PARSENAME
	dd dic_WORD_PICK
	dd dic_WORD_POSTPONE
	dd dic_WORD_QDUP
	dd dic_WORD_RBRAC
	dd dic_WORD_RDROP
	dd dic_WORD_RDROP2
	dd dic_WORD_READCHAR
	dd dic_WORD_READLINE
	dd dic_WORD_REFILL
	dd dic_WORD_RFETCH
	dd dic_WORD_RFETCH2
	dd dic_WORD_ROT
	dd dic_WORD_ROT2
	dd dic_WORD_RSHIFT
	dd dic_WORD_RSPFETCH
	dd dic_WORD_RSPSTORE
	dd dic_WORD_RSTORE
	dd dic_WORD_RZ
	dd dic_WORD_SEMICOLON
	dd dic_WORD_SLITS
	dd dic_WORD_SMDIVREM
	dd dic_WORD_SOURCE
	dd dic_WORD_SOURCEFD
	dd dic_WORD_SSTRING
	dd dic_WORD_STARSMOD
	dd dic_WORD_STATE
	dd dic_WORD_STOD
	dd dic_WORD_STORE
	dd dic_WORD_STORE2
	dd dic_WORD_STOREBYTE
	dd dic_WORD_STORESHORT
	dd dic_WORD_SUB
	dd dic_WORD_SUBSTORE
	dd dic_WORD_SWAP
	dd dic_WORD_SWAP2
	dd dic_WORD_SYSCALL
	dd dic_WORD_SYS_CLOSE
	dd dic_WORD_SYS_EXIT
	dd dic_WORD_SYS_FSTAT
	dd dic_WORD_SYS_FSYNC
	dd dic_WORD_SYS_FTRUNCATE
	dd dic_WORD_SYS_LSEEK
	dd dic_WORD_SYS_OPEN
	dd dic_WORD_SYS_READ
	dd dic_WORD_SYS_RENAME
	dd dic_WORD_SYS_STAT
	dd dic_WORD_SYS_UNLINK
	dd dic_WORD_SYS_WRITE
	dd dic_WORD_SZ
	dd dic_WORD_TABSTOSPACES
	dd dic_WORD_TCFA
	dd dic_WORD_TICKS
	dd dic_WORD_TOIN
	dd dic_WORD_TONUMBER
	dd dic_WORD_TOR
	dd dic_WORD_TOR2
	dd dic_WORD_TOSNUMBER
	dd dic_WORD_TRAILING
	dd dic_WORD_TUCK
	dd dic_WORD_TUCK2
	dd dic_WORD_TWODIV
	dd dic_WORD_TWOMUL
	dd dic_WORD_TYPE
	dd dic_WORD_TYPE_FD
	dd dic_WORD_UDIVMOD
	dd dic_WORD_UGT
	dd dic_WORD_UGTEQ
	dd dic_WORD_ULT
	dd dic_WORD_ULTEQ
	dd dic_WORD_UMDIVMOD
	dd dic_WORD_UMULSTAR
	dd dic_WORD_UNUSED
	dd dic_WORD_VERSION
	dd dic_WORD_WORDBUF
	dd dic_WORD_WORDNAME
	dd dic_WORD_XOR
	dd dic_WORD_XTSKIP
	dd dic_WORD_ZBRANCH
	dd dic_WORD_ZEQ
	dd dic_WORD_ZGT
	dd dic_WORD_ZGTEQ
	dd dic_WORD_ZLT
	dd dic_WORD_ZLTEQ
	dd dic_WORD_ZNE
	dd dic_WORD__F_HIDDEN
	dd dic_WORD__F_IMMED
	dd dic_WORD__F_LENMASK
	dd dic_WORD__H_NAME
	dd dic_WORD__H_NSIZE
	dd dic_WORD__XT_BODY
	dd dic_WORD__XT_COMPILE
	dd dic_WORD__XT_LENGTH
	dd dic_WORD__XT_SIZE
	dd dic_WORD_TEST
dictionary_end:

;;;;;;;;;;;;;;;;;;;;;;;;;;
; room for user dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;

	times USER_DEFS_SIZE db 0
forth_end:
