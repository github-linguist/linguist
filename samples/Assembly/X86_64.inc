
; flat assembler core
; Copyright (c) 1999-2014, Tomasz Grysztar.
; All rights reserved.

simple_instruction_except64:
	cmp	[code_type],64
	je	illegal_instruction
simple_instruction:
	stos	byte [edi]
	jmp	instruction_assembled
simple_instruction_only64:
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	simple_instruction
simple_instruction_16bit_except64:
	cmp	[code_type],64
	je	illegal_instruction
simple_instruction_16bit:
	cmp	[code_type],16
	jne	size_prefix
	stos	byte [edi]
	jmp	instruction_assembled
      size_prefix:
	mov	ah,al
	mov	al,66h
	stos	word [edi]
	jmp	instruction_assembled
simple_instruction_32bit_except64:
	cmp	[code_type],64
	je	illegal_instruction
simple_instruction_32bit:
	cmp	[code_type],16
	je	size_prefix
	stos	byte [edi]
	jmp	instruction_assembled
iret_instruction:
	cmp	[code_type],64
	jne	simple_instruction
simple_instruction_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	mov	ah,al
	mov	al,48h
	stos	word [edi]
	jmp	instruction_assembled
simple_extended_instruction_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	mov	byte [edi],48h
	inc	edi
simple_extended_instruction:
	mov	ah,al
	mov	al,0Fh
	stos	word [edi]
	jmp	instruction_assembled
prefix_instruction:
	stos	byte [edi]
	or	[prefixed_instruction],-1
	jmp	continue_line
segment_prefix:
	mov	ah,al
	shr	ah,4
	cmp	ah,6
	jne	illegal_instruction
	and	al,1111b
	mov	[segment_register],al
	call	store_segment_prefix
	or	[prefixed_instruction],-1
	jmp	continue_line
int_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	ah,1
	ja	invalid_operand_size
	cmp	al,'('
	jne	invalid_operand
	call	get_byte_value
	test	eax,eax
	jns	int_imm_ok
	call	recoverable_overflow
      int_imm_ok:
	mov	ah,al
	mov	al,0CDh
	stos	word [edi]
	jmp	instruction_assembled
aa_instruction:
	cmp	[code_type],64
	je	illegal_instruction
	push	eax
	mov	bl,10
	cmp	byte [esi],'('
	jne	aa_store
	inc	esi
	xor	al,al
	xchg	al,[operand_size]
	cmp	al,1
	ja	invalid_operand_size
	call	get_byte_value
	mov	bl,al
      aa_store:
	cmp	[operand_size],0
	jne	invalid_operand
	pop	eax
	mov	ah,bl
	stos	word [edi]
	jmp	instruction_assembled

basic_instruction:
	mov	[base_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	basic_reg
	cmp	al,'['
	jne	invalid_operand
      basic_mem:
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	basic_mem_imm
	cmp	al,10h
	jne	invalid_operand
      basic_mem_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	al,ah
	cmp	al,1
	je	instruction_ready
	call	operand_autodetect
	inc	[base_code]
      instruction_ready:
	call	store_instruction
	jmp	instruction_assembled
      basic_mem_imm:
	mov	al,[operand_size]
	cmp	al,1
	jb	basic_mem_imm_nosize
	je	basic_mem_imm_8bit
	cmp	al,2
	je	basic_mem_imm_16bit
	cmp	al,4
	je	basic_mem_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      basic_mem_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	basic_mem_imm_32bit_ok
      basic_mem_imm_nosize:
	call	recoverable_unknown_size
      basic_mem_imm_8bit:
	call	get_byte_value
	mov	byte [value],al
	mov	al,[base_code]
	shr	al,3
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	[base_code],80h
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      basic_mem_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	word [value],ax
	mov	al,[base_code]
	shr	al,3
	mov	[postbyte_register],al
	pop	ecx ebx edx
	cmp	[value_type],0
	jne	basic_mem_imm_16bit_store
	cmp	[size_declared],0
	jne	basic_mem_imm_16bit_store
	cmp	word [value],80h
	jb	basic_mem_simm_8bit
	cmp	word [value],-80h
	jae	basic_mem_simm_8bit
      basic_mem_imm_16bit_store:
	mov	[base_code],81h
	call	store_instruction_with_imm16
	jmp	instruction_assembled
      basic_mem_simm_8bit:
	mov	[base_code],83h
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      basic_mem_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      basic_mem_imm_32bit_ok:
	mov	dword [value],eax
	mov	al,[base_code]
	shr	al,3
	mov	[postbyte_register],al
	pop	ecx ebx edx
	cmp	[value_type],0
	jne	basic_mem_imm_32bit_store
	cmp	[size_declared],0
	jne	basic_mem_imm_32bit_store
	cmp	dword [value],80h
	jb	basic_mem_simm_8bit
	cmp	dword [value],-80h
	jae	basic_mem_simm_8bit
      basic_mem_imm_32bit_store:
	mov	[base_code],81h
	call	store_instruction_with_imm32
	jmp	instruction_assembled
      get_simm32:
	call	get_qword_value
	mov	ecx,edx
	cdq
	cmp	ecx,edx
	jne	value_out_of_range
	cmp	[value_type],4
	jne	get_simm32_ok
	mov	[value_type],2
      get_simm32_ok:
	ret
      basic_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	basic_reg_reg
	cmp	al,'('
	je	basic_reg_imm
	cmp	al,'['
	jne	invalid_operand
      basic_reg_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	basic_reg_mem_8bit
	call	operand_autodetect
	add	[base_code],3
	jmp	instruction_ready
      basic_reg_mem_8bit:
	add	[base_code],2
	jmp	instruction_ready
      basic_reg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,[postbyte_register]
	mov	[postbyte_register],al
	mov	al,ah
	cmp	al,1
	je	nomem_instruction_ready
	call	operand_autodetect
	inc	[base_code]
      nomem_instruction_ready:
	call	store_nomem_instruction
	jmp	instruction_assembled
      basic_reg_imm:
	mov	al,[operand_size]
	cmp	al,1
	je	basic_reg_imm_8bit
	cmp	al,2
	je	basic_reg_imm_16bit
	cmp	al,4
	je	basic_reg_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      basic_reg_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	basic_reg_imm_32bit_ok
      basic_reg_imm_8bit:
	call	get_byte_value
	mov	dl,al
	mov	bl,[base_code]
	shr	bl,3
	xchg	bl,[postbyte_register]
	or	bl,bl
	jz	basic_al_imm
	mov	[base_code],80h
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      basic_al_imm:
	mov	al,[base_code]
	add	al,4
	stos	byte [edi]
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      basic_reg_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	dx,ax
	mov	bl,[base_code]
	shr	bl,3
	xchg	bl,[postbyte_register]
	cmp	[value_type],0
	jne	basic_reg_imm_16bit_store
	cmp	[size_declared],0
	jne	basic_reg_imm_16bit_store
	cmp	dx,80h
	jb	basic_reg_simm_8bit
	cmp	dx,-80h
	jae	basic_reg_simm_8bit
      basic_reg_imm_16bit_store:
	or	bl,bl
	jz	basic_ax_imm
	mov	[base_code],81h
	call	store_nomem_instruction
      basic_store_imm_16bit:
	mov	ax,dx
	call	mark_relocation
	stos	word [edi]
	jmp	instruction_assembled
      basic_reg_simm_8bit:
	mov	[base_code],83h
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      basic_ax_imm:
	add	[base_code],5
	call	store_instruction_code
	jmp	basic_store_imm_16bit
      basic_reg_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      basic_reg_imm_32bit_ok:
	mov	edx,eax
	mov	bl,[base_code]
	shr	bl,3
	xchg	bl,[postbyte_register]
	cmp	[value_type],0
	jne	basic_reg_imm_32bit_store
	cmp	[size_declared],0
	jne	basic_reg_imm_32bit_store
	cmp	edx,80h
	jb	basic_reg_simm_8bit
	cmp	edx,-80h
	jae	basic_reg_simm_8bit
      basic_reg_imm_32bit_store:
	or	bl,bl
	jz	basic_eax_imm
	mov	[base_code],81h
	call	store_nomem_instruction
      basic_store_imm_32bit:
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      basic_eax_imm:
	add	[base_code],5
	call	store_instruction_code
	jmp	basic_store_imm_32bit
      recoverable_unknown_size:
	cmp	[error_line],0
	jne	ignore_unknown_size
	push	[current_line]
	pop	[error_line]
	mov	[error],operand_size_not_specified
      ignore_unknown_size:
	ret
single_operand_instruction:
	mov	[base_code],0F6h
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	single_reg
	cmp	al,'['
	jne	invalid_operand
      single_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	single_mem_8bit
	jb	single_mem_nosize
	call	operand_autodetect
	inc	[base_code]
	jmp	instruction_ready
      single_mem_nosize:
	call	recoverable_unknown_size
      single_mem_8bit:
	jmp	instruction_ready
      single_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	cmp	al,1
	je	single_reg_8bit
	call	operand_autodetect
	inc	[base_code]
      single_reg_8bit:
	jmp	nomem_instruction_ready
mov_instruction:
	mov	[base_code],88h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	mov_reg
	cmp	al,'['
	jne	invalid_operand
      mov_mem:
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	mov_mem_imm
	cmp	al,10h
	jne	invalid_operand
      mov_mem_reg:
	lods	byte [esi]
	cmp	al,60h
	jb	mov_mem_general_reg
	cmp	al,70h
	jb	mov_mem_sreg
      mov_mem_general_reg:
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	cmp	ah,1
	je	mov_mem_reg_8bit
	mov	al,ah
	call	operand_autodetect
	mov	al,[postbyte_register]
	or	al,bl
	or	al,bh
	jz	mov_mem_ax
	inc	[base_code]
	jmp	instruction_ready
      mov_mem_reg_8bit:
	or	al,bl
	or	al,bh
	jnz	instruction_ready
      mov_mem_al:
	test	ch,22h
	jnz	mov_mem_address16_al
	test	ch,44h
	jnz	mov_mem_address32_al
	test	ch,88h
	jnz	mov_mem_address64_al
	or	ch,ch
	jnz	invalid_address_size
	cmp	[code_type],64
	je	mov_mem_address64_al
	cmp	[code_type],32
	je	mov_mem_address32_al
	cmp	edx,10000h
	jb	mov_mem_address16_al
      mov_mem_address32_al:
	call	store_segment_prefix_if_necessary
	call	address_32bit_prefix
	mov	[base_code],0A2h
      store_mov_address32:
	call	store_instruction_code
	call	store_address_32bit_value
	jmp	instruction_assembled
      mov_mem_address16_al:
	call	store_segment_prefix_if_necessary
	call	address_16bit_prefix
	mov	[base_code],0A2h
      store_mov_address16:
	cmp	[code_type],64
	je	invalid_address
	call	store_instruction_code
	mov	eax,edx
	stos	word [edi]
	cmp	edx,10000h
	jge	value_out_of_range
	jmp	instruction_assembled
      mov_mem_address64_al:
	call	store_segment_prefix_if_necessary
	mov	[base_code],0A2h
      store_mov_address64:
	call	store_instruction_code
	call	store_address_64bit_value
	jmp	instruction_assembled
      mov_mem_ax:
	test	ch,22h
	jnz	mov_mem_address16_ax
	test	ch,44h
	jnz	mov_mem_address32_ax
	test	ch,88h
	jnz	mov_mem_address64_ax
	or	ch,ch
	jnz	invalid_address_size
	cmp	[code_type],64
	je	mov_mem_address64_ax
	cmp	[code_type],32
	je	mov_mem_address32_ax
	cmp	edx,10000h
	jb	mov_mem_address16_ax
      mov_mem_address32_ax:
	call	store_segment_prefix_if_necessary
	call	address_32bit_prefix
	mov	[base_code],0A3h
	jmp	store_mov_address32
      mov_mem_address16_ax:
	call	store_segment_prefix_if_necessary
	call	address_16bit_prefix
	mov	[base_code],0A3h
	jmp	store_mov_address16
      mov_mem_address64_ax:
	call	store_segment_prefix_if_necessary
	mov	[base_code],0A3h
	jmp	store_mov_address64
      mov_mem_sreg:
	sub	al,61h
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	ah,[operand_size]
	or	ah,ah
	jz	mov_mem_sreg_store
	cmp	ah,2
	jne	invalid_operand_size
      mov_mem_sreg_store:
	mov	[base_code],8Ch
	jmp	instruction_ready
      mov_mem_imm:
	mov	al,[operand_size]
	cmp	al,1
	jb	mov_mem_imm_nosize
	je	mov_mem_imm_8bit
	cmp	al,2
	je	mov_mem_imm_16bit
	cmp	al,4
	je	mov_mem_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      mov_mem_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	mov_mem_imm_32bit_store
      mov_mem_imm_8bit:
	call	get_byte_value
	mov	byte [value],al
	mov	[postbyte_register],0
	mov	[base_code],0C6h
	pop	ecx ebx edx
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      mov_mem_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	word [value],ax
	mov	[postbyte_register],0
	mov	[base_code],0C7h
	pop	ecx ebx edx
	call	store_instruction_with_imm16
	jmp	instruction_assembled
      mov_mem_imm_nosize:
	call	recoverable_unknown_size
      mov_mem_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      mov_mem_imm_32bit_store:
	mov	dword [value],eax
	mov	[postbyte_register],0
	mov	[base_code],0C7h
	pop	ecx ebx edx
	call	store_instruction_with_imm32
	jmp	instruction_assembled
      mov_reg:
	lods	byte [esi]
	mov	ah,al
	sub	ah,10h
	and	ah,al
	test	ah,0F0h
	jnz	mov_sreg
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	je	mov_reg_mem
	cmp	al,'('
	je	mov_reg_imm
	cmp	al,10h
	jne	invalid_operand
      mov_reg_reg:
	lods	byte [esi]
	mov	ah,al
	sub	ah,10h
	and	ah,al
	test	ah,0F0h
	jnz	mov_reg_sreg
	call	convert_register
	mov	bl,[postbyte_register]
	mov	[postbyte_register],al
	mov	al,ah
	cmp	al,1
	je	mov_reg_reg_8bit
	call	operand_autodetect
	inc	[base_code]
      mov_reg_reg_8bit:
	jmp	nomem_instruction_ready
      mov_reg_sreg:
	mov	bl,[postbyte_register]
	mov	ah,al
	and	al,1111b
	mov	[postbyte_register],al
	shr	ah,4
	cmp	ah,5
	je	mov_reg_creg
	cmp	ah,7
	je	mov_reg_dreg
	ja	mov_reg_treg
	dec	[postbyte_register]
	cmp	[operand_size],8
	je	mov_reg_sreg64
	cmp	[operand_size],4
	je	mov_reg_sreg32
	cmp	[operand_size],2
	jne	invalid_operand_size
	call	operand_16bit
	jmp	mov_reg_sreg_store
      mov_reg_sreg64:
	call	operand_64bit
	jmp	mov_reg_sreg_store
      mov_reg_sreg32:
	call	operand_32bit
      mov_reg_sreg_store:
	mov	[base_code],8Ch
	jmp	nomem_instruction_ready
      mov_reg_treg:
	cmp	ah,9
	jne	invalid_operand
	mov	[extended_code],24h
	jmp	mov_reg_xrx
      mov_reg_dreg:
	mov	[extended_code],21h
	jmp	mov_reg_xrx
      mov_reg_creg:
	mov	[extended_code],20h
      mov_reg_xrx:
	mov	[base_code],0Fh
	cmp	[code_type],64
	je	mov_reg_xrx_64bit
	cmp	[operand_size],4
	jne	invalid_operand_size
	cmp	[postbyte_register],8
	jne	mov_reg_xrx_store
	cmp	[extended_code],20h
	jne	mov_reg_xrx_store
	mov	al,0F0h
	stos	byte [edi]
	mov	[postbyte_register],0
      mov_reg_xrx_store:
	jmp	nomem_instruction_ready
      mov_reg_xrx_64bit:
	cmp	[operand_size],8
	jne	invalid_operand_size
	jmp	nomem_instruction_ready
      mov_reg_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	mov_reg_mem_8bit
	call	operand_autodetect
	mov	al,[postbyte_register]
	or	al,bl
	or	al,bh
	jz	mov_ax_mem
	add	[base_code],3
	jmp	instruction_ready
      mov_reg_mem_8bit:
	mov	al,[postbyte_register]
	or	al,bl
	or	al,bh
	jz	mov_al_mem
	add	[base_code],2
	jmp	instruction_ready
      mov_al_mem:
	test	ch,22h
	jnz	mov_al_mem_address16
	test	ch,44h
	jnz	mov_al_mem_address32
	test	ch,88h
	jnz	mov_al_mem_address64
	or	ch,ch
	jnz	invalid_address_size
	cmp	[code_type],64
	je	mov_al_mem_address64
	cmp	[code_type],32
	je	mov_al_mem_address32
	cmp	edx,10000h
	jb	mov_al_mem_address16
      mov_al_mem_address32:
	call	store_segment_prefix_if_necessary
	call	address_32bit_prefix
	mov	[base_code],0A0h
	jmp	store_mov_address32
      mov_al_mem_address16:
	call	store_segment_prefix_if_necessary
	call	address_16bit_prefix
	mov	[base_code],0A0h
	jmp	store_mov_address16
      mov_al_mem_address64:
	call	store_segment_prefix_if_necessary
	mov	[base_code],0A0h
	jmp	store_mov_address64
      mov_ax_mem:
	test	ch,22h
	jnz	mov_ax_mem_address16
	test	ch,44h
	jnz	mov_ax_mem_address32
	test	ch,88h
	jnz	mov_ax_mem_address64
	or	ch,ch
	jnz	invalid_address_size
	cmp	[code_type],64
	je	mov_ax_mem_address64
	cmp	[code_type],32
	je	mov_ax_mem_address32
	cmp	edx,10000h
	jb	mov_ax_mem_address16
      mov_ax_mem_address32:
	call	store_segment_prefix_if_necessary
	call	address_32bit_prefix
	mov	[base_code],0A1h
	jmp	store_mov_address32
      mov_ax_mem_address16:
	call	store_segment_prefix_if_necessary
	call	address_16bit_prefix
	mov	[base_code],0A1h
	jmp	store_mov_address16
      mov_ax_mem_address64:
	call	store_segment_prefix_if_necessary
	mov	[base_code],0A1h
	jmp	store_mov_address64
      mov_reg_imm:
	mov	al,[operand_size]
	cmp	al,1
	je	mov_reg_imm_8bit
	cmp	al,2
	je	mov_reg_imm_16bit
	cmp	al,4
	je	mov_reg_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      mov_reg_imm_64bit:
	call	operand_64bit
	call	get_qword_value
	mov	ecx,edx
	cmp	[size_declared],0
	jne	mov_reg_imm_64bit_store
	cmp	[value_type],4
	jae	mov_reg_imm_64bit_store
	cdq
	cmp	ecx,edx
	je	mov_reg_64bit_imm_32bit
      mov_reg_imm_64bit_store:
	push	eax ecx
	mov	al,0B8h
	call	store_mov_reg_imm_code
	pop	edx eax
	call	mark_relocation
	stos	dword [edi]
	mov	eax,edx
	stos	dword [edi]
	jmp	instruction_assembled
      mov_reg_imm_8bit:
	call	get_byte_value
	mov	dl,al
	mov	al,0B0h
	call	store_mov_reg_imm_code
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      mov_reg_imm_16bit:
	call	get_word_value
	mov	dx,ax
	call	operand_16bit
	mov	al,0B8h
	call	store_mov_reg_imm_code
	mov	ax,dx
	call	mark_relocation
	stos	word [edi]
	jmp	instruction_assembled
      mov_reg_imm_32bit:
	call	operand_32bit
	call	get_dword_value
	mov	edx,eax
	mov	al,0B8h
	call	store_mov_reg_imm_code
      mov_store_imm_32bit:
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      store_mov_reg_imm_code:
	mov	ah,[postbyte_register]
	test	ah,1000b
	jz	mov_reg_imm_prefix_ok
	or	[rex_prefix],41h
      mov_reg_imm_prefix_ok:
	and	ah,111b
	add	al,ah
	mov	[base_code],al
	call	store_instruction_code
	ret
      mov_reg_64bit_imm_32bit:
	mov	edx,eax
	mov	bl,[postbyte_register]
	mov	[postbyte_register],0
	mov	[base_code],0C7h
	call	store_nomem_instruction
	jmp	mov_store_imm_32bit
      mov_sreg:
	mov	ah,al
	and	al,1111b
	mov	[postbyte_register],al
	shr	ah,4
	cmp	ah,5
	je	mov_creg
	cmp	ah,7
	je	mov_dreg
	ja	mov_treg
	cmp	al,2
	je	illegal_instruction
	dec	[postbyte_register]
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	je	mov_sreg_mem
	cmp	al,10h
	jne	invalid_operand
      mov_sreg_reg:
	lods	byte [esi]
	call	convert_register
	or	ah,ah
	jz	mov_sreg_reg_size_ok
	cmp	ah,2
	jne	invalid_operand_size
	mov	bl,al
      mov_sreg_reg_size_ok:
	mov	[base_code],8Eh
	jmp	nomem_instruction_ready
      mov_sreg_mem:
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	mov_sreg_mem_size_ok
	cmp	al,2
	jne	invalid_operand_size
      mov_sreg_mem_size_ok:
	mov	[base_code],8Eh
	jmp	instruction_ready
      mov_treg:
	cmp	ah,9
	jne	invalid_operand
	mov	[extended_code],26h
	jmp	mov_xrx
      mov_dreg:
	mov	[extended_code],23h
	jmp	mov_xrx
      mov_creg:
	mov	[extended_code],22h
      mov_xrx:
	mov	[base_code],0Fh
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	cmp	[code_type],64
	je	mov_xrx_64bit
	cmp	ah,4
	jne	invalid_operand_size
	cmp	[postbyte_register],8
	jne	mov_xrx_store
	cmp	[extended_code],22h
	jne	mov_xrx_store
	mov	al,0F0h
	stos	byte [edi]
	mov	[postbyte_register],0
      mov_xrx_store:
	jmp	nomem_instruction_ready
      mov_xrx_64bit:
	cmp	ah,8
	je	mov_xrx_store
	jmp	invalid_operand_size
test_instruction:
	mov	[base_code],84h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	test_reg
	cmp	al,'['
	jne	invalid_operand
      test_mem:
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	test_mem_imm
	cmp	al,10h
	jne	invalid_operand
      test_mem_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	al,ah
	cmp	al,1
	je	test_mem_reg_8bit
	call	operand_autodetect
	inc	[base_code]
      test_mem_reg_8bit:
	jmp	instruction_ready
      test_mem_imm:
	mov	al,[operand_size]
	cmp	al,1
	jb	test_mem_imm_nosize
	je	test_mem_imm_8bit
	cmp	al,2
	je	test_mem_imm_16bit
	cmp	al,4
	je	test_mem_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      test_mem_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	test_mem_imm_32bit_store
      test_mem_imm_8bit:
	call	get_byte_value
	mov	byte [value],al
	mov	[postbyte_register],0
	mov	[base_code],0F6h
	pop	ecx ebx edx
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      test_mem_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	word [value],ax
	mov	[postbyte_register],0
	mov	[base_code],0F7h
	pop	ecx ebx edx
	call	store_instruction_with_imm16
	jmp	instruction_assembled
      test_mem_imm_nosize:
	call	recoverable_unknown_size
      test_mem_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      test_mem_imm_32bit_store:
	mov	dword [value],eax
	mov	[postbyte_register],0
	mov	[base_code],0F7h
	pop	ecx ebx edx
	call	store_instruction_with_imm32
	jmp	instruction_assembled
      test_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	je	test_reg_mem
	cmp	al,'('
	je	test_reg_imm
	cmp	al,10h
	jne	invalid_operand
      test_reg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,[postbyte_register]
	mov	[postbyte_register],al
	mov	al,ah
	cmp	al,1
	je	test_reg_reg_8bit
	call	operand_autodetect
	inc	[base_code]
      test_reg_reg_8bit:
	jmp	nomem_instruction_ready
      test_reg_imm:
	mov	al,[operand_size]
	cmp	al,1
	je	test_reg_imm_8bit
	cmp	al,2
	je	test_reg_imm_16bit
	cmp	al,4
	je	test_reg_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      test_reg_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	test_reg_imm_32bit_store
      test_reg_imm_8bit:
	call	get_byte_value
	mov	dl,al
	mov	bl,[postbyte_register]
	mov	[postbyte_register],0
	mov	[base_code],0F6h
	or	bl,bl
	jz	test_al_imm
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      test_al_imm:
	mov	[base_code],0A8h
	call	store_instruction_code
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      test_reg_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	dx,ax
	mov	bl,[postbyte_register]
	mov	[postbyte_register],0
	mov	[base_code],0F7h
	or	bl,bl
	jz	test_ax_imm
	call	store_nomem_instruction
	mov	ax,dx
	call	mark_relocation
	stos	word [edi]
	jmp	instruction_assembled
      test_ax_imm:
	mov	[base_code],0A9h
	call	store_instruction_code
	mov	ax,dx
	stos	word [edi]
	jmp	instruction_assembled
      test_reg_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      test_reg_imm_32bit_store:
	mov	edx,eax
	mov	bl,[postbyte_register]
	mov	[postbyte_register],0
	mov	[base_code],0F7h
	or	bl,bl
	jz	test_eax_imm
	call	store_nomem_instruction
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      test_eax_imm:
	mov	[base_code],0A9h
	call	store_instruction_code
	mov	eax,edx
	stos	dword [edi]
	jmp	instruction_assembled
      test_reg_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	test_reg_mem_8bit
	call	operand_autodetect
	inc	[base_code]
      test_reg_mem_8bit:
	jmp	instruction_ready
xchg_instruction:
	mov	[base_code],86h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	xchg_reg
	cmp	al,'['
	jne	invalid_operand
      xchg_mem:
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	test_mem_reg
	jmp	invalid_operand
      xchg_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	je	test_reg_mem
	cmp	al,10h
	jne	invalid_operand
      xchg_reg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	cmp	al,1
	je	xchg_reg_reg_8bit
	call	operand_autodetect
	cmp	[postbyte_register],0
	je	xchg_ax_reg
	or	bl,bl
	jnz	xchg_reg_reg_store
	mov	bl,[postbyte_register]
      xchg_ax_reg:
	cmp	[code_type],64
	jne	xchg_ax_reg_ok
	cmp	ah,4
	jne	xchg_ax_reg_ok
	or	bl,bl
	jz	xchg_reg_reg_store
      xchg_ax_reg_ok:
	test	bl,1000b
	jz	xchg_ax_reg_store
	or	[rex_prefix],41h
	and	bl,111b
      xchg_ax_reg_store:
	add	bl,90h
	mov	[base_code],bl
	call	store_instruction_code
	jmp	instruction_assembled
      xchg_reg_reg_store:
	inc	[base_code]
      xchg_reg_reg_8bit:
	jmp	nomem_instruction_ready
push_instruction:
	mov	[push_size],al
      push_next:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	push_reg
	cmp	al,'('
	je	push_imm
	cmp	al,'['
	jne	invalid_operand
      push_mem:
	call	get_address
	mov	al,[operand_size]
	mov	ah,[push_size]
	cmp	al,2
	je	push_mem_16bit
	cmp	al,4
	je	push_mem_32bit
	cmp	al,8
	je	push_mem_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	ah,2
	je	push_mem_16bit
	cmp	ah,4
	je	push_mem_32bit
	cmp	ah,8
	je	push_mem_64bit
	call	recoverable_unknown_size
	jmp	push_mem_store
      push_mem_16bit:
	test	ah,not 2
	jnz	invalid_operand_size
	call	operand_16bit
	jmp	push_mem_store
      push_mem_32bit:
	test	ah,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	push_mem_store
      push_mem_64bit:
	test	ah,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
      push_mem_store:
	mov	[base_code],0FFh
	mov	[postbyte_register],110b
	call	store_instruction
	jmp	push_done
      push_reg:
	lods	byte [esi]
	mov	ah,al
	sub	ah,10h
	and	ah,al
	test	ah,0F0h
	jnz	push_sreg
	call	convert_register
	test	al,1000b
	jz	push_reg_ok
	or	[rex_prefix],41h
	and	al,111b
      push_reg_ok:
	add	al,50h
	mov	[base_code],al
	mov	al,ah
	mov	ah,[push_size]
	cmp	al,2
	je	push_reg_16bit
	cmp	al,4
	je	push_reg_32bit
	cmp	al,8
	jne	invalid_operand_size
      push_reg_64bit:
	test	ah,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	push_reg_store
      push_reg_32bit:
	test	ah,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	push_reg_store
      push_reg_16bit:
	test	ah,not 2
	jnz	invalid_operand_size
	call	operand_16bit
      push_reg_store:
	call	store_instruction_code
	jmp	push_done
      push_sreg:
	mov	bl,al
	mov	dl,[operand_size]
	mov	dh,[push_size]
	cmp	dl,2
	je	push_sreg16
	cmp	dl,4
	je	push_sreg32
	cmp	dl,8
	je	push_sreg64
	or	dl,dl
	jnz	invalid_operand_size
	cmp	dh,2
	je	push_sreg16
	cmp	dh,4
	je	push_sreg32
	cmp	dh,8
	je	push_sreg64
	jmp	push_sreg_store
      push_sreg16:
	test	dh,not 2
	jnz	invalid_operand_size
	call	operand_16bit
	jmp	push_sreg_store
      push_sreg32:
	test	dh,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	push_sreg_store
      push_sreg64:
	test	dh,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
      push_sreg_store:
	mov	al,bl
	cmp	al,70h
	jae	invalid_operand
	sub	al,61h
	jc	invalid_operand
	cmp	al,4
	jae	push_sreg_386
	shl	al,3
	add	al,6
	mov	[base_code],al
	cmp	[code_type],64
	je	illegal_instruction
	jmp	push_reg_store
      push_sreg_386:
	sub	al,4
	shl	al,3
	add	al,0A0h
	mov	[extended_code],al
	mov	[base_code],0Fh
	jmp	push_reg_store
      push_imm:
	mov	al,[operand_size]
	mov	ah,[push_size]
	or	al,al
	je	push_imm_size_ok
	or	ah,ah
	je	push_imm_size_ok
	cmp	al,ah
	jne	invalid_operand_size
      push_imm_size_ok:
	cmp	al,2
	je	push_imm_16bit
	cmp	al,4
	je	push_imm_32bit
	cmp	al,8
	je	push_imm_64bit
	cmp	ah,2
	je	push_imm_optimized_16bit
	cmp	ah,4
	je	push_imm_optimized_32bit
	cmp	ah,8
	je	push_imm_optimized_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	[code_type],16
	je	push_imm_optimized_16bit
	cmp	[code_type],32
	je	push_imm_optimized_32bit
      push_imm_optimized_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	call	get_simm32
	mov	edx,eax
	cmp	[value_type],0
	jne	push_imm_32bit_store
	cmp	eax,-80h
	jl	push_imm_32bit_store
	cmp	eax,80h
	jge	push_imm_32bit_store
	jmp	push_imm_8bit
      push_imm_optimized_32bit:
	cmp	[code_type],64
	je	illegal_instruction
	call	get_dword_value
	mov	edx,eax
	call	operand_32bit
	cmp	[value_type],0
	jne	push_imm_32bit_store
	cmp	eax,-80h
	jl	push_imm_32bit_store
	cmp	eax,80h
	jge	push_imm_32bit_store
	jmp	push_imm_8bit
      push_imm_optimized_16bit:
	call	get_word_value
	mov	dx,ax
	call	operand_16bit
	cmp	[value_type],0
	jne	push_imm_16bit_store
	cmp	ax,-80h
	jl	push_imm_16bit_store
	cmp	ax,80h
	jge	push_imm_16bit_store
      push_imm_8bit:
	mov	ah,al
	mov	[base_code],6Ah
	call	store_instruction_code
	mov	al,ah
	stos	byte [edi]
	jmp	push_done
      push_imm_16bit:
	call	get_word_value
	mov	dx,ax
	call	operand_16bit
      push_imm_16bit_store:
	mov	[base_code],68h
	call	store_instruction_code
	mov	ax,dx
	call	mark_relocation
	stos	word [edi]
	jmp	push_done
      push_imm_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	call	get_simm32
	mov	edx,eax
	jmp	push_imm_32bit_store
      push_imm_32bit:
	cmp	[code_type],64
	je	illegal_instruction
	call	get_dword_value
	mov	edx,eax
	call	operand_32bit
      push_imm_32bit_store:
	mov	[base_code],68h
	call	store_instruction_code
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
      push_done:
	lods	byte [esi]
	dec	esi
	cmp	al,0Fh
	je	instruction_assembled
	or	al,al
	jz	instruction_assembled
	mov	[operand_size],0
	mov	[size_override],0
	mov	[operand_prefix],0
	mov	[rex_prefix],0
	jmp	push_next
pop_instruction:
	mov	[push_size],al
      pop_next:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pop_reg
	cmp	al,'['
	jne	invalid_operand
      pop_mem:
	call	get_address
	mov	al,[operand_size]
	mov	ah,[push_size]
	cmp	al,2
	je	pop_mem_16bit
	cmp	al,4
	je	pop_mem_32bit
	cmp	al,8
	je	pop_mem_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	ah,2
	je	pop_mem_16bit
	cmp	ah,4
	je	pop_mem_32bit
	cmp	ah,8
	je	pop_mem_64bit
	call	recoverable_unknown_size
	jmp	pop_mem_store
      pop_mem_16bit:
	test	ah,not 2
	jnz	invalid_operand_size
	call	operand_16bit
	jmp	pop_mem_store
      pop_mem_32bit:
	test	ah,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	pop_mem_store
      pop_mem_64bit:
	test	ah,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
      pop_mem_store:
	mov	[base_code],08Fh
	mov	[postbyte_register],0
	call	store_instruction
	jmp	pop_done
      pop_reg:
	lods	byte [esi]
	mov	ah,al
	sub	ah,10h
	and	ah,al
	test	ah,0F0h
	jnz	pop_sreg
	call	convert_register
	test	al,1000b
	jz	pop_reg_ok
	or	[rex_prefix],41h
	and	al,111b
      pop_reg_ok:
	add	al,58h
	mov	[base_code],al
	mov	al,ah
	mov	ah,[push_size]
	cmp	al,2
	je	pop_reg_16bit
	cmp	al,4
	je	pop_reg_32bit
	cmp	al,8
	je	pop_reg_64bit
	jmp	invalid_operand_size
      pop_reg_64bit:
	test	ah,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	pop_reg_store
      pop_reg_32bit:
	test	ah,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	pop_reg_store
      pop_reg_16bit:
	test	ah,not 2
	jnz	invalid_operand_size
	call	operand_16bit
      pop_reg_store:
	call	store_instruction_code
      pop_done:
	lods	byte [esi]
	dec	esi
	cmp	al,0Fh
	je	instruction_assembled
	or	al,al
	jz	instruction_assembled
	mov	[operand_size],0
	mov	[size_override],0
	mov	[operand_prefix],0
	mov	[rex_prefix],0
	jmp	pop_next
      pop_sreg:
	mov	dl,[operand_size]
	mov	dh,[push_size]
	cmp	al,62h
	je	pop_cs
	mov	bl,al
	cmp	dl,2
	je	pop_sreg16
	cmp	dl,4
	je	pop_sreg32
	cmp	dl,8
	je	pop_sreg64
	or	dl,dl
	jnz	invalid_operand_size
	cmp	dh,2
	je	pop_sreg16
	cmp	dh,4
	je	pop_sreg32
	cmp	dh,8
	je	pop_sreg64
	jmp	pop_sreg_store
      pop_sreg16:
	test	dh,not 2
	jnz	invalid_operand_size
	call	operand_16bit
	jmp	pop_sreg_store
      pop_sreg32:
	test	dh,not 4
	jnz	invalid_operand_size
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	pop_sreg_store
      pop_sreg64:
	test	dh,not 8
	jnz	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
      pop_sreg_store:
	mov	al,bl
	cmp	al,70h
	jae	invalid_operand
	sub	al,61h
	jc	invalid_operand
	cmp	al,4
	jae	pop_sreg_386
	shl	al,3
	add	al,7
	mov	[base_code],al
	cmp	[code_type],64
	je	illegal_instruction
	jmp	pop_reg_store
      pop_cs:
	cmp	[code_type],16
	jne	illegal_instruction
	cmp	dl,2
	je	pop_cs_store
	or	dl,dl
	jnz	invalid_operand_size
	cmp	dh,2
	je	pop_cs_store
	or	dh,dh
	jnz	illegal_instruction
      pop_cs_store:
	test	dh,not 2
	jnz	invalid_operand_size
	mov	al,0Fh
	stos	byte [edi]
	jmp	pop_done
      pop_sreg_386:
	sub	al,4
	shl	al,3
	add	al,0A1h
	mov	[extended_code],al
	mov	[base_code],0Fh
	jmp	pop_reg_store
inc_instruction:
	mov	[base_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	inc_reg
	cmp	al,'['
	je	inc_mem
	jne	invalid_operand
      inc_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	inc_mem_8bit
	jb	inc_mem_nosize
	call	operand_autodetect
	mov	al,0FFh
	xchg	al,[base_code]
	mov	[postbyte_register],al
	jmp	instruction_ready
      inc_mem_nosize:
	call	recoverable_unknown_size
      inc_mem_8bit:
	mov	al,0FEh
	xchg	al,[base_code]
	mov	[postbyte_register],al
	jmp	instruction_ready
      inc_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,0FEh
	xchg	al,[base_code]
	mov	[postbyte_register],al
	mov	al,ah
	cmp	al,1
	je	inc_reg_8bit
	call	operand_autodetect
	cmp	[code_type],64
	je	inc_reg_long_form
	mov	al,[postbyte_register]
	shl	al,3
	add	al,bl
	add	al,40h
	mov	[base_code],al
	call	store_instruction_code
	jmp	instruction_assembled
      inc_reg_long_form:
	inc	[base_code]
      inc_reg_8bit:
	jmp	nomem_instruction_ready
set_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	set_reg
	cmp	al,'['
	jne	invalid_operand
      set_mem:
	call	get_address
	cmp	[operand_size],1
	ja	invalid_operand_size
	mov	[postbyte_register],0
	jmp	instruction_ready
      set_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,1
	jne	invalid_operand_size
	mov	bl,al
	mov	[postbyte_register],0
	jmp	nomem_instruction_ready
arpl_instruction:
	cmp	[code_type],64
	je	illegal_instruction
	mov	[base_code],63h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	arpl_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	cmp	ah,2
	jne	invalid_operand_size
	jmp	instruction_ready
      arpl_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,2
	jne	invalid_operand_size
	mov	bl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	jmp	nomem_instruction_ready
bound_instruction:
	cmp	[code_type],64
	je	illegal_instruction
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,2
	je	bound_store
	cmp	al,4
	jne	invalid_operand_size
      bound_store:
	call	operand_autodetect
	mov	[base_code],62h
	jmp	instruction_ready
enter_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	ah,2
	je	enter_imm16_size_ok
	or	ah,ah
	jnz	invalid_operand_size
      enter_imm16_size_ok:
	cmp	al,'('
	jne	invalid_operand
	call	get_word_value
	cmp	[next_pass_needed],0
	jne	enter_imm16_ok
	cmp	[value_type],0
	jne	invalid_use_of_symbol
	test	eax,eax
	js	value_out_of_range
      enter_imm16_ok:
	push	eax
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	ah,1
	je	enter_imm8_size_ok
	or	ah,ah
	jnz	invalid_operand_size
      enter_imm8_size_ok:
	cmp	al,'('
	jne	invalid_operand
	call	get_byte_value
	cmp	[next_pass_needed],0
	jne	enter_imm8_ok
	test	eax,eax
	js	value_out_of_range
      enter_imm8_ok:
	mov	dl,al
	pop	ebx
	mov	al,0C8h
	stos	byte [edi]
	mov	ax,bx
	stos	word [edi]
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
ret_instruction_only64:
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	ret_instruction
ret_instruction_32bit_except64:
	cmp	[code_type],64
	je	illegal_instruction
ret_instruction_32bit:
	call	operand_32bit
	jmp	ret_instruction
ret_instruction_16bit:
	call	operand_16bit
	jmp	ret_instruction
retf_instruction:
	cmp	[code_type],64
	jne	ret_instruction
ret_instruction_64bit:
	call	operand_64bit
ret_instruction:
	mov	[base_code],al
	lods	byte [esi]
	dec	esi
	or	al,al
	jz	simple_ret
	cmp	al,0Fh
	je	simple_ret
	lods	byte [esi]
	call	get_size_operator
	or	ah,ah
	jz	ret_imm
	cmp	ah,2
	je	ret_imm
	jmp	invalid_operand_size
      ret_imm:
	cmp	al,'('
	jne	invalid_operand
	call	get_word_value
	cmp	[next_pass_needed],0
	jne	ret_imm_ok
	cmp	[value_type],0
	jne	invalid_use_of_symbol
	test	eax,eax
	js	value_out_of_range
      ret_imm_ok:
	cmp	[size_declared],0
	jne	ret_imm_store
	or	ax,ax
	jz	simple_ret
      ret_imm_store:
	mov	dx,ax
	call	store_instruction_code
	mov	ax,dx
	stos	word [edi]
	jmp	instruction_assembled
      simple_ret:
	inc	[base_code]
	call	store_instruction_code
	jmp	instruction_assembled
lea_instruction:
	mov	[base_code],8Dh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	xor	al,al
	xchg	al,[operand_size]
	push	eax
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	mov	[size_override],-1
	call	get_address
	pop	eax
	mov	[operand_size],al
	call	operand_autodetect
	jmp	instruction_ready
ls_instruction:
	or	al,al
	jz	les_instruction
	cmp	al,3
	jz	lds_instruction
	add	al,0B0h
	mov	[extended_code],al
	mov	[base_code],0Fh
	jmp	ls_code_ok
      les_instruction:
	mov	[base_code],0C4h
	jmp	ls_short_code
      lds_instruction:
	mov	[base_code],0C5h
      ls_short_code:
	cmp	[code_type],64
	je	illegal_instruction
      ls_code_ok:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	add	[operand_size],2
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,4
	je	ls_16bit
	cmp	al,6
	je	ls_32bit
	cmp	al,10
	je	ls_64bit
	jmp	invalid_operand_size
      ls_16bit:
	call	operand_16bit
	jmp	instruction_ready
      ls_32bit:
	call	operand_32bit
	jmp	instruction_ready
      ls_64bit:
	call	operand_64bit
	jmp	instruction_ready
sh_instruction:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	sh_reg
	cmp	al,'['
	jne	invalid_operand
      sh_mem:
	call	get_address
	push	edx ebx ecx
	mov	al,[operand_size]
	push	eax
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	sh_mem_imm
	cmp	al,10h
	jne	invalid_operand
      sh_mem_reg:
	lods	byte [esi]
	cmp	al,11h
	jne	invalid_operand
	pop	eax ecx ebx edx
	cmp	al,1
	je	sh_mem_cl_8bit
	jb	sh_mem_cl_nosize
	call	operand_autodetect
	mov	[base_code],0D3h
	jmp	instruction_ready
      sh_mem_cl_nosize:
	call	recoverable_unknown_size
      sh_mem_cl_8bit:
	mov	[base_code],0D2h
	jmp	instruction_ready
      sh_mem_imm:
	mov	al,[operand_size]
	or	al,al
	jz	sh_mem_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      sh_mem_imm_size_ok:
	call	get_byte_value
	mov	byte [value],al
	pop	eax ecx ebx edx
	cmp	al,1
	je	sh_mem_imm_8bit
	jb	sh_mem_imm_nosize
	call	operand_autodetect
	cmp	byte [value],1
	je	sh_mem_1
	mov	[base_code],0C1h
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      sh_mem_1:
	mov	[base_code],0D1h
	jmp	instruction_ready
      sh_mem_imm_nosize:
	call	recoverable_unknown_size
      sh_mem_imm_8bit:
	cmp	byte [value],1
	je	sh_mem_1_8bit
	mov	[base_code],0C0h
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      sh_mem_1_8bit:
	mov	[base_code],0D0h
	jmp	instruction_ready
      sh_reg:
	lods	byte [esi]
	call	convert_register
	mov	bx,ax
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	sh_reg_imm
	cmp	al,10h
	jne	invalid_operand
      sh_reg_reg:
	lods	byte [esi]
	cmp	al,11h
	jne	invalid_operand
	mov	al,bh
	cmp	al,1
	je	sh_reg_cl_8bit
	call	operand_autodetect
	mov	[base_code],0D3h
	jmp	nomem_instruction_ready
      sh_reg_cl_8bit:
	mov	[base_code],0D2h
	jmp	nomem_instruction_ready
      sh_reg_imm:
	mov	al,[operand_size]
	or	al,al
	jz	sh_reg_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      sh_reg_imm_size_ok:
	push	ebx
	call	get_byte_value
	mov	dl,al
	pop	ebx
	mov	al,bh
	cmp	al,1
	je	sh_reg_imm_8bit
	call	operand_autodetect
	cmp	dl,1
	je	sh_reg_1
	mov	[base_code],0C1h
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      sh_reg_1:
	mov	[base_code],0D1h
	jmp	nomem_instruction_ready
      sh_reg_imm_8bit:
	cmp	dl,1
	je	sh_reg_1_8bit
	mov	[base_code],0C0h
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      sh_reg_1_8bit:
	mov	[base_code],0D0h
	jmp	nomem_instruction_ready
shd_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	shd_reg
	cmp	al,'['
	jne	invalid_operand
      shd_mem:
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	al,ah
	mov	[operand_size],0
	push	eax
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	shd_mem_reg_imm
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,11h
	jne	invalid_operand
	pop	eax ecx ebx edx
	call	operand_autodetect
	inc	[extended_code]
	jmp	instruction_ready
      shd_mem_reg_imm:
	mov	al,[operand_size]
	or	al,al
	jz	shd_mem_reg_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      shd_mem_reg_imm_size_ok:
	call	get_byte_value
	mov	byte [value],al
	pop	eax ecx ebx edx
	call	operand_autodetect
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      shd_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,[postbyte_register]
	mov	[postbyte_register],al
	mov	al,ah
	push	eax ebx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	shd_reg_reg_imm
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,11h
	jne	invalid_operand
	pop	ebx eax
	call	operand_autodetect
	inc	[extended_code]
	jmp	nomem_instruction_ready
      shd_reg_reg_imm:
	mov	al,[operand_size]
	or	al,al
	jz	shd_reg_reg_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      shd_reg_reg_imm_size_ok:
	call	get_byte_value
	mov	dl,al
	pop	ebx eax
	call	operand_autodetect
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
movx_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	mov	al,ah
	push	eax
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movx_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	pop	eax
	mov	ah,[operand_size]
	or	ah,ah
	jz	movx_unknown_size
	cmp	ah,al
	jae	invalid_operand_size
	cmp	ah,1
	je	movx_mem_store
	cmp	ah,2
	jne	invalid_operand_size
	inc	[extended_code]
      movx_mem_store:
	call	operand_autodetect
	jmp	instruction_ready
      movx_unknown_size:
	call	recoverable_unknown_size
	jmp	movx_mem_store
      movx_reg:
	lods	byte [esi]
	call	convert_register
	pop	ebx
	xchg	bl,al
	cmp	ah,al
	jae	invalid_operand_size
	cmp	ah,1
	je	movx_reg_8bit
	cmp	ah,2
	je	movx_reg_16bit
	jmp	invalid_operand_size
      movx_reg_8bit:
	call	operand_autodetect
	jmp	nomem_instruction_ready
      movx_reg_16bit:
	call	operand_autodetect
	inc	[extended_code]
	jmp	nomem_instruction_ready
movsxd_instruction:
	mov	[base_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	cmp	ah,8
	jne	invalid_operand_size
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movsxd_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],4
	je	movsxd_mem_store
	cmp	[operand_size],0
	jne	invalid_operand_size
      movsxd_mem_store:
	call	operand_64bit
	jmp	instruction_ready
      movsxd_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	jne	invalid_operand_size
	mov	bl,al
	call	operand_64bit
	jmp	nomem_instruction_ready
bt_instruction:
	mov	[postbyte_register],al
	shl	al,3
	add	al,83h
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	bt_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	push	eax ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	cmp	byte [esi],'('
	je	bt_mem_imm
	cmp	byte [esi],11h
	jne	bt_mem_reg
	cmp	byte [esi+2],'('
	je	bt_mem_imm
      bt_mem_reg:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	al,ah
	call	operand_autodetect
	jmp	instruction_ready
      bt_mem_imm:
	xor	al,al
	xchg	al,[operand_size]
	push	eax
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	mov	al,[operand_size]
	or	al,al
	jz	bt_mem_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      bt_mem_imm_size_ok:
	call	get_byte_value
	mov	byte [value],al
	pop	eax
	or	al,al
	jz	bt_mem_imm_nosize
	call	operand_autodetect
      bt_mem_imm_store:
	pop	ecx ebx edx
	mov	[extended_code],0BAh
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      bt_mem_imm_nosize:
	call	recoverable_unknown_size
	jmp	bt_mem_imm_store
      bt_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	cmp	byte [esi],'('
	je	bt_reg_imm
	cmp	byte [esi],11h
	jne	bt_reg_reg
	cmp	byte [esi+2],'('
	je	bt_reg_imm
      bt_reg_reg:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	mov	al,ah
	call	operand_autodetect
	jmp	nomem_instruction_ready
      bt_reg_imm:
	xor	al,al
	xchg	al,[operand_size]
	push	eax ebx
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	mov	al,[operand_size]
	or	al,al
	jz	bt_reg_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      bt_reg_imm_size_ok:
	call	get_byte_value
	mov	byte [value],al
	pop	ebx eax
	call	operand_autodetect
      bt_reg_imm_store:
	mov	[extended_code],0BAh
	call	store_nomem_instruction
	mov	al,byte [value]
	stos	byte [edi]
	jmp	instruction_assembled
bs_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	call	get_reg_mem
	jc	bs_reg_reg
	mov	al,[operand_size]
	call	operand_autodetect
	jmp	instruction_ready
      bs_reg_reg:
	mov	al,ah
	call	operand_autodetect
	jmp	nomem_instruction_ready
      get_reg_mem:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	get_reg_reg
	cmp	al,'['
	jne	invalid_argument
	call	get_address
	clc
	ret
      get_reg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	stc
	ret

imul_instruction:
	mov	[base_code],0F6h
	mov	[postbyte_register],5
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	imul_reg
	cmp	al,'['
	jne	invalid_operand
      imul_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,1
	je	imul_mem_8bit
	jb	imul_mem_nosize
	call	operand_autodetect
	inc	[base_code]
	jmp	instruction_ready
      imul_mem_nosize:
	call	recoverable_unknown_size
      imul_mem_8bit:
	jmp	instruction_ready
      imul_reg:
	lods	byte [esi]
	call	convert_register
	cmp	byte [esi],','
	je	imul_reg_
	mov	bl,al
	mov	al,ah
	cmp	al,1
	je	imul_reg_8bit
	call	operand_autodetect
	inc	[base_code]
      imul_reg_8bit:
	jmp	nomem_instruction_ready
      imul_reg_:
	mov	[postbyte_register],al
	inc	esi
	cmp	byte [esi],'('
	je	imul_reg_imm
	cmp	byte [esi],11h
	jne	imul_reg_noimm
	cmp	byte [esi+2],'('
	je	imul_reg_imm
      imul_reg_noimm:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	imul_reg_reg
	cmp	al,'['
	jne	invalid_operand
      imul_reg_mem:
	call	get_address
	push	edx ebx ecx
	cmp	byte [esi],','
	je	imul_reg_mem_imm
	mov	al,[operand_size]
	call	operand_autodetect
	pop	ecx ebx edx
	mov	[base_code],0Fh
	mov	[extended_code],0AFh
	jmp	instruction_ready
      imul_reg_mem_imm:
	inc	esi
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	mov	al,[operand_size]
	cmp	al,2
	je	imul_reg_mem_imm_16bit
	cmp	al,4
	je	imul_reg_mem_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      imul_reg_mem_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	imul_reg_mem_imm_32bit_ok
      imul_reg_mem_imm_16bit:
	call	operand_16bit
	call	get_word_value
	mov	word [value],ax
	cmp	[value_type],0
	jne	imul_reg_mem_imm_16bit_store
	cmp	[size_declared],0
	jne	imul_reg_mem_imm_16bit_store
	cmp	ax,-80h
	jl	imul_reg_mem_imm_16bit_store
	cmp	ax,80h
	jl	imul_reg_mem_imm_8bit_store
      imul_reg_mem_imm_16bit_store:
	pop	ecx ebx edx
	mov	[base_code],69h
	call	store_instruction_with_imm16
	jmp	instruction_assembled
      imul_reg_mem_imm_32bit:
	call	operand_32bit
	call	get_dword_value
      imul_reg_mem_imm_32bit_ok:
	mov	dword [value],eax
	cmp	[value_type],0
	jne	imul_reg_mem_imm_32bit_store
	cmp	[size_declared],0
	jne	imul_reg_mem_imm_32bit_store
	cmp	eax,-80h
	jl	imul_reg_mem_imm_32bit_store
	cmp	eax,80h
	jl	imul_reg_mem_imm_8bit_store
      imul_reg_mem_imm_32bit_store:
	pop	ecx ebx edx
	mov	[base_code],69h
	call	store_instruction_with_imm32
	jmp	instruction_assembled
      imul_reg_mem_imm_8bit_store:
	pop	ecx ebx edx
	mov	[base_code],6Bh
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      imul_reg_imm:
	mov	bl,[postbyte_register]
	dec	esi
	jmp	imul_reg_reg_imm
      imul_reg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	cmp	byte [esi],','
	je	imul_reg_reg_imm
	mov	al,ah
	call	operand_autodetect
	mov	[base_code],0Fh
	mov	[extended_code],0AFh
	jmp	nomem_instruction_ready
      imul_reg_reg_imm:
	inc	esi
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	mov	al,[operand_size]
	cmp	al,2
	je	imul_reg_reg_imm_16bit
	cmp	al,4
	je	imul_reg_reg_imm_32bit
	cmp	al,8
	jne	invalid_operand_size
      imul_reg_reg_imm_64bit:
	cmp	[size_declared],0
	jne	long_immediate_not_encodable
	call	operand_64bit
	push	ebx
	call	get_simm32
	cmp	[value_type],4
	jae	long_immediate_not_encodable
	jmp	imul_reg_reg_imm_32bit_ok
      imul_reg_reg_imm_16bit:
	call	operand_16bit
	push	ebx
	call	get_word_value
	pop	ebx
	mov	dx,ax
	cmp	[value_type],0
	jne	imul_reg_reg_imm_16bit_store
	cmp	[size_declared],0
	jne	imul_reg_reg_imm_16bit_store
	cmp	ax,-80h
	jl	imul_reg_reg_imm_16bit_store
	cmp	ax,80h
	jl	imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_16bit_store:
	mov	[base_code],69h
	call	store_nomem_instruction
	mov	ax,dx
	call	mark_relocation
	stos	word [edi]
	jmp	instruction_assembled
      imul_reg_reg_imm_32bit:
	call	operand_32bit
	push	ebx
	call	get_dword_value
      imul_reg_reg_imm_32bit_ok:
	pop	ebx
	mov	edx,eax
	cmp	[value_type],0
	jne	imul_reg_reg_imm_32bit_store
	cmp	[size_declared],0
	jne	imul_reg_reg_imm_32bit_store
	cmp	eax,-80h
	jl	imul_reg_reg_imm_32bit_store
	cmp	eax,80h
	jl	imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_32bit_store:
	mov	[base_code],69h
	call	store_nomem_instruction
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      imul_reg_reg_imm_8bit_store:
	mov	[base_code],6Bh
	call	store_nomem_instruction
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
in_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	or	al,al
	jnz	invalid_operand
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	al,ah
	push	eax
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	in_imm
	cmp	al,10h
	je	in_reg
	jmp	invalid_operand
      in_reg:
	lods	byte [esi]
	cmp	al,22h
	jne	invalid_operand
	pop	eax
	cmp	al,1
	je	in_al_dx
	cmp	al,2
	je	in_ax_dx
	cmp	al,4
	jne	invalid_operand_size
      in_ax_dx:
	call	operand_autodetect
	mov	[base_code],0EDh
	call	store_instruction_code
	jmp	instruction_assembled
      in_al_dx:
	mov	al,0ECh
	stos	byte [edi]
	jmp	instruction_assembled
      in_imm:
	mov	al,[operand_size]
	or	al,al
	jz	in_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      in_imm_size_ok:
	call	get_byte_value
	mov	dl,al
	pop	eax
	cmp	al,1
	je	in_al_imm
	cmp	al,2
	je	in_ax_imm
	cmp	al,4
	jne	invalid_operand_size
      in_ax_imm:
	call	operand_autodetect
	mov	[base_code],0E5h
	call	store_instruction_code
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      in_al_imm:
	mov	al,0E4h
	stos	byte [edi]
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
out_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'('
	je	out_imm
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,22h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	or	al,al
	jnz	invalid_operand
	mov	al,ah
	cmp	al,1
	je	out_dx_al
	cmp	al,2
	je	out_dx_ax
	cmp	al,4
	jne	invalid_operand_size
      out_dx_ax:
	call	operand_autodetect
	mov	[base_code],0EFh
	call	store_instruction_code
	jmp	instruction_assembled
      out_dx_al:
	mov	al,0EEh
	stos	byte [edi]
	jmp	instruction_assembled
      out_imm:
	mov	al,[operand_size]
	or	al,al
	jz	out_imm_size_ok
	cmp	al,1
	jne	invalid_operand_size
      out_imm_size_ok:
	call	get_byte_value
	mov	dl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	or	al,al
	jnz	invalid_operand
	mov	al,ah
	cmp	al,1
	je	out_imm_al
	cmp	al,2
	je	out_imm_ax
	cmp	al,4
	jne	invalid_operand_size
      out_imm_ax:
	call	operand_autodetect
	mov	[base_code],0E7h
	call	store_instruction_code
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
      out_imm_al:
	mov	al,0E6h
	stos	byte [edi]
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled

call_instruction:
	mov	[postbyte_register],10b
	mov	[base_code],0E8h
	mov	[extended_code],9Ah
	jmp	process_jmp
jmp_instruction:
	mov	[postbyte_register],100b
	mov	[base_code],0E9h
	mov	[extended_code],0EAh
      process_jmp:
	lods	byte [esi]
	call	get_jump_operator
	call	get_size_operator
	cmp	al,'('
	je	jmp_imm
	mov	[base_code],0FFh
	cmp	al,10h
	je	jmp_reg
	cmp	al,'['
	jne	invalid_operand
      jmp_mem:
	cmp	[jump_type],1
	je	illegal_instruction
	call	get_address
	mov	edx,eax
	mov	al,[operand_size]
	or	al,al
	jz	jmp_mem_size_not_specified
	cmp	al,2
	je	jmp_mem_16bit
	cmp	al,4
	je	jmp_mem_32bit
	cmp	al,6
	je	jmp_mem_48bit
	cmp	al,8
	je	jmp_mem_64bit
	cmp	al,10
	je	jmp_mem_80bit
	jmp	invalid_operand_size
      jmp_mem_size_not_specified:
	cmp	[jump_type],3
	je	jmp_mem_far
	cmp	[jump_type],2
	je	jmp_mem_near
	call	recoverable_unknown_size
      jmp_mem_near:
	cmp	[code_type],16
	je	jmp_mem_16bit
	cmp	[code_type],32
	je	jmp_mem_near_32bit
      jmp_mem_64bit:
	cmp	[jump_type],3
	je	invalid_operand_size
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	instruction_ready
      jmp_mem_far:
	cmp	[code_type],16
	je	jmp_mem_far_32bit
      jmp_mem_48bit:
	call	operand_32bit
      jmp_mem_far_store:
	cmp	[jump_type],2
	je	invalid_operand_size
	inc	[postbyte_register]
	jmp	instruction_ready
      jmp_mem_80bit:
	call	operand_64bit
	jmp	jmp_mem_far_store
      jmp_mem_far_32bit:
	call	operand_16bit
	jmp	jmp_mem_far_store
      jmp_mem_32bit:
	cmp	[jump_type],3
	je	jmp_mem_far_32bit
	cmp	[jump_type],2
	je	jmp_mem_near_32bit
	cmp	[code_type],16
	je	jmp_mem_far_32bit
      jmp_mem_near_32bit:
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	instruction_ready
      jmp_mem_16bit:
	cmp	[jump_type],3
	je	invalid_operand_size
	call	operand_16bit
	jmp	instruction_ready
      jmp_reg:
	test	[jump_type],1
	jnz	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	cmp	al,2
	je	jmp_reg_16bit
	cmp	al,4
	je	jmp_reg_32bit
	cmp	al,8
	jne	invalid_operand_size
      jmp_reg_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	nomem_instruction_ready
      jmp_reg_32bit:
	cmp	[code_type],64
	je	illegal_instruction
	call	operand_32bit
	jmp	nomem_instruction_ready
      jmp_reg_16bit:
	call	operand_16bit
	jmp	nomem_instruction_ready
      jmp_imm:
	cmp	byte [esi],'.'
	je	invalid_value
	mov	ebx,esi
	dec	esi
	call	skip_symbol
	xchg	esi,ebx
	cmp	byte [ebx],':'
	je	jmp_far
	cmp	[jump_type],3
	je	invalid_operand
      jmp_near:
	mov	al,[operand_size]
	cmp	al,2
	je	jmp_imm_16bit
	cmp	al,4
	je	jmp_imm_32bit
	cmp	al,8
	je	jmp_imm_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	[code_type],16
	je	jmp_imm_16bit
	cmp	[code_type],64
	je	jmp_imm_64bit
      jmp_imm_32bit:
	cmp	[code_type],64
	je	invalid_operand_size
	call	get_address_dword_value
	cmp	[code_type],16
	jne	jmp_imm_32bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      jmp_imm_32bit_prefix_ok:
	call	calculate_jump_offset
	cdq
	call	check_for_short_jump
	jc	jmp_short
      jmp_imm_32bit_store:
	mov	edx,eax
	sub	edx,3
	jno	jmp_imm_32bit_ok
	cmp	[code_type],64
	je	relative_jump_out_of_range
      jmp_imm_32bit_ok:
	mov	al,[base_code]
	stos	byte [edi]
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      jmp_imm_64bit:
	cmp	[code_type],64
	jne	invalid_operand_size
	call	get_address_qword_value
	call	calculate_jump_offset
	mov	ecx,edx
	cdq
	cmp	edx,ecx
	jne	relative_jump_out_of_range
	call	check_for_short_jump
	jnc	jmp_imm_32bit_store
      jmp_short:
	mov	ah,al
	mov	al,0EBh
	stos	word [edi]
	jmp	instruction_assembled
      jmp_imm_16bit:
	call	get_address_word_value
	cmp	[code_type],16
	je	jmp_imm_16bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      jmp_imm_16bit_prefix_ok:
	call	calculate_jump_offset
	cwde
	cdq
	call	check_for_short_jump
	jc	jmp_short
	cmp	[value_type],0
	jne	invalid_use_of_symbol
	mov	edx,eax
	dec	edx
	mov	al,[base_code]
	stos	byte [edi]
	mov	eax,edx
	stos	word [edi]
	jmp	instruction_assembled
      calculate_jump_offset:
	add	edi,2
	mov	ebp,[addressing_space]
	call	calculate_relative_offset
	sub	edi,2
	ret
      check_for_short_jump:
	cmp	[jump_type],1
	je	forced_short
	ja	no_short_jump
	cmp	[base_code],0E8h
	je	no_short_jump
	cmp	[value_type],0
	jne	no_short_jump
	cmp	eax,80h
	jb	short_jump
	cmp	eax,-80h
	jae	short_jump
      no_short_jump:
	clc
	ret
      forced_short:
	cmp	[base_code],0E8h
	je	illegal_instruction
	cmp	[next_pass_needed],0
	jne	jmp_short_value_type_ok
	cmp	[value_type],0
	jne	invalid_use_of_symbol
      jmp_short_value_type_ok:
	cmp	eax,-80h
	jae	short_jump
	cmp	eax,80h
	jae	jump_out_of_range
      short_jump:
	stc
	ret
      jump_out_of_range:
	cmp	[error_line],0
	jne	instruction_assembled
	mov	eax,[current_line]
	mov	[error_line],eax
	mov	[error],relative_jump_out_of_range
	jmp	instruction_assembled
      jmp_far:
	cmp	[jump_type],2
	je	invalid_operand
	cmp	[code_type],64
	je	illegal_instruction
	mov	al,[extended_code]
	mov	[base_code],al
	call	get_word_value
	push	eax
	inc	esi
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_operand
	mov	al,[value_type]
	push	eax [symbol_identifier]
	cmp	byte [esi],'.'
	je	invalid_value
	mov	al,[operand_size]
	cmp	al,4
	je	jmp_far_16bit
	cmp	al,6
	je	jmp_far_32bit
	or	al,al
	jnz	invalid_operand_size
	cmp	[code_type],16
	jne	jmp_far_32bit
      jmp_far_16bit:
	call	get_word_value
	mov	ebx,eax
	call	operand_16bit
	call	store_instruction_code
	mov	ax,bx
	call	mark_relocation
	stos	word [edi]
      jmp_far_segment:
	pop	[symbol_identifier] eax
	mov	[value_type],al
	pop	eax
	call	mark_relocation
	stos	word [edi]
	jmp	instruction_assembled
      jmp_far_32bit:
	call	get_dword_value
	mov	ebx,eax
	call	operand_32bit
	call	store_instruction_code
	mov	eax,ebx
	call	mark_relocation
	stos	dword [edi]
	jmp	jmp_far_segment
conditional_jump:
	mov	[base_code],al
	lods	byte [esi]
	call	get_jump_operator
	cmp	[jump_type],3
	je	invalid_operand
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	cmp	byte [esi],'.'
	je	invalid_value
	mov	al,[operand_size]
	cmp	al,2
	je	conditional_jump_16bit
	cmp	al,4
	je	conditional_jump_32bit
	cmp	al,8
	je	conditional_jump_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	[code_type],16
	je	conditional_jump_16bit
	cmp	[code_type],64
	je	conditional_jump_64bit
      conditional_jump_32bit:
	cmp	[code_type],64
	je	invalid_operand_size
	call	get_address_dword_value
	cmp	[code_type],16
	jne	conditional_jump_32bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      conditional_jump_32bit_prefix_ok:
	call	calculate_jump_offset
	cdq
	call	check_for_short_jump
	jc	conditional_jump_short
      conditional_jump_32bit_store:
	mov	edx,eax
	sub	edx,4
	jno	conditional_jump_32bit_range_ok
	cmp	[code_type],64
	je	relative_jump_out_of_range
      conditional_jump_32bit_range_ok:
	mov	ah,[base_code]
	add	ah,10h
	mov	al,0Fh
	stos	word [edi]
	mov	eax,edx
	call	mark_relocation
	stos	dword [edi]
	jmp	instruction_assembled
      conditional_jump_64bit:
	cmp	[code_type],64
	jne	invalid_operand_size
	call	get_address_qword_value
	call	calculate_jump_offset
	mov	ecx,edx
	cdq
	cmp	edx,ecx
	jne	relative_jump_out_of_range
	call	check_for_short_jump
	jnc	conditional_jump_32bit_store
      conditional_jump_short:
	mov	ah,al
	mov	al,[base_code]
	stos	word [edi]
	jmp	instruction_assembled
      conditional_jump_16bit:
	call	get_address_word_value
	cmp	[code_type],16
	je	conditional_jump_16bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      conditional_jump_16bit_prefix_ok:
	call	calculate_jump_offset
	cwde
	cdq
	call	check_for_short_jump
	jc	conditional_jump_short
	cmp	[value_type],0
	jne	invalid_use_of_symbol
	mov	edx,eax
	sub	dx,2
	mov	ah,[base_code]
	add	ah,10h
	mov	al,0Fh
	stos	word [edi]
	mov	eax,edx
	stos	word [edi]
	jmp	instruction_assembled
loop_instruction_16bit:
	cmp	[code_type],64
	je	illegal_instruction
	cmp	[code_type],16
	je	loop_instruction
	mov	[operand_prefix],67h
	jmp	loop_instruction
loop_instruction_32bit:
	cmp	[code_type],32
	je	loop_instruction
	mov	[operand_prefix],67h
      jmp     loop_instruction
loop_instruction_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
loop_instruction:
	mov	[base_code],al
	lods	byte [esi]
	call	get_jump_operator
	cmp	[jump_type],1
	ja	invalid_operand
	call	get_size_operator
	cmp	al,'('
	jne	invalid_operand
	cmp	byte [esi],'.'
	je	invalid_value
	mov	al,[operand_size]
	cmp	al,2
	je	loop_jump_16bit
	cmp	al,4
	je	loop_jump_32bit
	cmp	al,8
	je	loop_jump_64bit
	or	al,al
	jnz	invalid_operand_size
	cmp	[code_type],16
	je	loop_jump_16bit
	cmp	[code_type],64
	je	loop_jump_64bit
      loop_jump_32bit:
	cmp	[code_type],64
	je	invalid_operand_size
	call	get_address_dword_value
	cmp	[code_type],16
	jne	loop_jump_32bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      loop_jump_32bit_prefix_ok:
	call	loop_counter_size
	call	calculate_jump_offset
	cdq
      make_loop_jump:
	call	check_for_short_jump
	jc	conditional_jump_short
	scas	word [edi]
	jmp	jump_out_of_range
      loop_counter_size:
	cmp	[operand_prefix],0
	je	loop_counter_size_ok
	push	eax
	mov	al,[operand_prefix]
	stos	byte [edi]
	pop	eax
      loop_counter_size_ok:
	ret
      loop_jump_64bit:
	cmp	[code_type],64
	jne	invalid_operand_size
	call	get_address_qword_value
	call	loop_counter_size
	call	calculate_jump_offset
	mov	ecx,edx
	cdq
	cmp	edx,ecx
	jne	relative_jump_out_of_range
	jmp	make_loop_jump
      loop_jump_16bit:
	call	get_address_word_value
	cmp	[code_type],16
	je	loop_jump_16bit_prefix_ok
	mov	byte [edi],66h
	inc	edi
      loop_jump_16bit_prefix_ok:
	call	loop_counter_size
	call	calculate_jump_offset
	cwde
	cdq
	jmp	make_loop_jump

movs_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	[segment_register],1
	ja	invalid_address
	push	ebx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	pop	edx
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	mov	al,dh
	mov	ah,bh
	shr	al,4
	shr	ah,4
	cmp	al,ah
	jne	address_sizes_do_not_agree
	and	bh,111b
	and	dh,111b
	cmp	bh,6
	jne	invalid_address
	cmp	dh,7
	jne	invalid_address
	cmp	al,2
	je	movs_address_16bit
	cmp	al,4
	je	movs_address_32bit
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	movs_store
      movs_address_32bit:
	call	address_32bit_prefix
	jmp	movs_store
      movs_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      movs_store:
	xor	ebx,ebx
	call	store_segment_prefix_if_necessary
	mov	al,0A4h
      movs_check_size:
	mov	bl,[operand_size]
	cmp	bl,1
	je	simple_instruction
	inc	al
	cmp	bl,2
	je	simple_instruction_16bit
	cmp	bl,4
	je	simple_instruction_32bit
	cmp	bl,8
	je	simple_instruction_64bit
	or	bl,bl
	jnz	invalid_operand_size
	call	recoverable_unknown_size
	jmp	simple_instruction
lods_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	bh,26h
	je	lods_address_16bit
	cmp	bh,46h
	je	lods_address_32bit
	cmp	bh,86h
	jne	invalid_address
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	lods_store
      lods_address_32bit:
	call	address_32bit_prefix
	jmp	lods_store
      lods_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      lods_store:
	xor	ebx,ebx
	call	store_segment_prefix_if_necessary
	mov	al,0ACh
	jmp	movs_check_size
stos_instruction:
	mov	[base_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	bh,27h
	je	stos_address_16bit
	cmp	bh,47h
	je	stos_address_32bit
	cmp	bh,87h
	jne	invalid_address
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	stos_store
      stos_address_32bit:
	call	address_32bit_prefix
	jmp	stos_store
      stos_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      stos_store:
	cmp	[segment_register],1
	ja	invalid_address
	mov	al,[base_code]
	jmp	movs_check_size
cmps_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	mov	al,[segment_register]
	push	eax ebx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	pop	edx eax
	cmp	[segment_register],1
	ja	invalid_address
	mov	[segment_register],al
	mov	al,dh
	mov	ah,bh
	shr	al,4
	shr	ah,4
	cmp	al,ah
	jne	address_sizes_do_not_agree
	and	bh,111b
	and	dh,111b
	cmp	bh,7
	jne	invalid_address
	cmp	dh,6
	jne	invalid_address
	cmp	al,2
	je	cmps_address_16bit
	cmp	al,4
	je	cmps_address_32bit
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	cmps_store
      cmps_address_32bit:
	call	address_32bit_prefix
	jmp	cmps_store
      cmps_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      cmps_store:
	xor	ebx,ebx
	call	store_segment_prefix_if_necessary
	mov	al,0A6h
	jmp	movs_check_size
ins_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	bh,27h
	je	ins_address_16bit
	cmp	bh,47h
	je	ins_address_32bit
	cmp	bh,87h
	jne	invalid_address
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	ins_store
      ins_address_32bit:
	call	address_32bit_prefix
	jmp	ins_store
      ins_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      ins_store:
	cmp	[segment_register],1
	ja	invalid_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,22h
	jne	invalid_operand
	mov	al,6Ch
      ins_check_size:
	cmp	[operand_size],8
	jne	movs_check_size
	jmp	invalid_operand_size
outs_instruction:
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,22h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	bh,26h
	je	outs_address_16bit
	cmp	bh,46h
	je	outs_address_32bit
	cmp	bh,86h
	jne	invalid_address
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	outs_store
      outs_address_32bit:
	call	address_32bit_prefix
	jmp	outs_store
      outs_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      outs_store:
	xor	ebx,ebx
	call	store_segment_prefix_if_necessary
	mov	al,6Eh
	jmp	ins_check_size
xlat_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	or	eax,eax
	jnz	invalid_address
	or	bl,ch
	jnz	invalid_address
	cmp	bh,23h
	je	xlat_address_16bit
	cmp	bh,43h
	je	xlat_address_32bit
	cmp	bh,83h
	jne	invalid_address
	cmp	[code_type],64
	jne	invalid_address_size
	jmp	xlat_store
      xlat_address_32bit:
	call	address_32bit_prefix
	jmp	xlat_store
      xlat_address_16bit:
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
      xlat_store:
	call	store_segment_prefix_if_necessary
	mov	al,0D7h
	cmp	[operand_size],1
	jbe	simple_instruction
	jmp	invalid_operand_size

pm_word_instruction:
	mov	ah,al
	shr	ah,4
	and	al,111b
	mov	[base_code],0Fh
	mov	[extended_code],ah
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pm_reg
      pm_mem:
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,2
	je	pm_mem_store
	or	al,al
	jnz	invalid_operand_size
      pm_mem_store:
	jmp	instruction_ready
      pm_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	cmp	ah,2
	jne	invalid_operand_size
	jmp	nomem_instruction_ready
pm_store_word_instruction:
	mov	ah,al
	shr	ah,4
	and	al,111b
	mov	[base_code],0Fh
	mov	[extended_code],ah
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	pm_mem
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	call	operand_autodetect
	jmp	nomem_instruction_ready
lgdt_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],1
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,6
	je	lgdt_mem_48bit
	cmp	al,10
	je	lgdt_mem_80bit
	or	al,al
	jnz	invalid_operand_size
	jmp	lgdt_mem_store
      lgdt_mem_80bit:
	cmp	[code_type],64
	jne	illegal_instruction
	jmp	lgdt_mem_store
      lgdt_mem_48bit:
	cmp	[code_type],64
	je	illegal_instruction
	cmp	[postbyte_register],2
	jb	lgdt_mem_store
	call	operand_32bit
      lgdt_mem_store:
	jmp	instruction_ready
lar_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	xor	al,al
	xchg	al,[operand_size]
	call	operand_autodetect
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	lar_reg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	lar_reg_mem
	cmp	al,2
	jne	invalid_operand_size
      lar_reg_mem:
	jmp	instruction_ready
      lar_reg_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,2
	jne	invalid_operand_size
	mov	bl,al
	jmp	nomem_instruction_ready
invlpg_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],1
	mov	[postbyte_register],7
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	jmp	instruction_ready
swapgs_instruction:
	cmp	[code_type],64
	jne	illegal_instruction
rdtscp_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],1
	mov	[postbyte_register],7
	mov	bl,al
	jmp	nomem_instruction_ready

basic_486_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	basic_486_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	al,ah
	cmp	al,1
	je	basic_486_mem_reg_8bit
	call	operand_autodetect
	inc	[extended_code]
      basic_486_mem_reg_8bit:
	jmp	instruction_ready
      basic_486_reg:
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,[postbyte_register]
	mov	[postbyte_register],al
	mov	al,ah
	cmp	al,1
	je	basic_486_reg_reg_8bit
	call	operand_autodetect
	inc	[extended_code]
      basic_486_reg_reg_8bit:
	jmp	nomem_instruction_ready
bswap_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	test	al,1000b
	jz	bswap_reg_code_ok
	or	[rex_prefix],41h
	and	al,111b
      bswap_reg_code_ok:
	add	al,0C8h
	mov	[extended_code],al
	mov	[base_code],0Fh
	cmp	ah,8
	je	bswap_reg64
	cmp	ah,4
	jne	invalid_operand_size
	call	operand_32bit
	call	store_instruction_code
	jmp	instruction_assembled
      bswap_reg64:
	call	operand_64bit
	call	store_instruction_code
	jmp	instruction_assembled
cmpxchgx_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],0C7h
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	ah,1
	xchg	[postbyte_register],ah
	mov	al,[operand_size]
	or	al,al
	jz	cmpxchgx_size_ok
	cmp	al,ah
	jne	invalid_operand_size
      cmpxchgx_size_ok:
	cmp	ah,16
	jne	cmpxchgx_store
	call	operand_64bit
      cmpxchgx_store:
	jmp	instruction_ready
nop_instruction:
	mov	ah,[esi]
	cmp	ah,10h
	je	extended_nop
	cmp	ah,11h
	je	extended_nop
	cmp	ah,'['
	je	extended_nop
	stos	byte [edi]
	jmp	instruction_assembled
      extended_nop:
	mov	[base_code],0Fh
	mov	[extended_code],1Fh
	mov	[postbyte_register],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	extended_nop_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	extended_nop_store
	call	operand_autodetect
      extended_nop_store:
	jmp	instruction_ready
      extended_nop_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	call	operand_autodetect
	jmp	nomem_instruction_ready

basic_fpu_instruction:
	mov	[postbyte_register],al
	mov	[base_code],0D8h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	basic_fpu_streg
	cmp	al,'['
	je	basic_fpu_mem
	dec	esi
	mov	ah,[postbyte_register]
	cmp	ah,2
	jb	invalid_operand
	cmp	ah,3
	ja	invalid_operand
	mov	bl,1
	jmp	nomem_instruction_ready
      basic_fpu_mem:
	call	get_address
	mov	al,[operand_size]
	cmp	al,4
	je	basic_fpu_mem_32bit
	cmp	al,8
	je	basic_fpu_mem_64bit
	or	al,al
	jnz	invalid_operand_size
	call	recoverable_unknown_size
      basic_fpu_mem_32bit:
	jmp	instruction_ready
      basic_fpu_mem_64bit:
	mov	[base_code],0DCh
	jmp	instruction_ready
      basic_fpu_streg:
	lods	byte [esi]
	call	convert_fpu_register
	mov	bl,al
	mov	ah,[postbyte_register]
	cmp	ah,2
	je	basic_fpu_single_streg
	cmp	ah,3
	je	basic_fpu_single_streg
	or	al,al
	jz	basic_fpu_st0
	test	ah,110b
	jz	basic_fpu_streg_st0
	xor	[postbyte_register],1
      basic_fpu_streg_st0:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_fpu_register
	or	al,al
	jnz	invalid_operand
	mov	[base_code],0DCh
	jmp	nomem_instruction_ready
      basic_fpu_st0:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_fpu_register
	mov	bl,al
      basic_fpu_single_streg:
	mov	[base_code],0D8h
	jmp	nomem_instruction_ready
simple_fpu_instruction:
	mov	ah,al
	or	ah,11000000b
	mov	al,0D9h
	stos	word [edi]
	jmp	instruction_assembled
fi_instruction:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,2
	je	fi_mem_16bit
	cmp	al,4
	je	fi_mem_32bit
	or	al,al
	jnz	invalid_operand_size
	call	recoverable_unknown_size
      fi_mem_32bit:
	mov	[base_code],0DAh
	jmp	instruction_ready
      fi_mem_16bit:
	mov	[base_code],0DEh
	jmp	instruction_ready
fld_instruction:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	fld_streg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,4
	je	fld_mem_32bit
	cmp	al,8
	je	fld_mem_64bit
	cmp	al,10
	je	fld_mem_80bit
	or	al,al
	jnz	invalid_operand_size
	call	recoverable_unknown_size
      fld_mem_32bit:
	mov	[base_code],0D9h
	jmp	instruction_ready
      fld_mem_64bit:
	mov	[base_code],0DDh
	jmp	instruction_ready
      fld_mem_80bit:
	mov	al,[postbyte_register]
	cmp	al,0
	je	fld_mem_80bit_store
	dec	[postbyte_register]
	cmp	al,3
	je	fld_mem_80bit_store
	jmp	invalid_operand_size
      fld_mem_80bit_store:
	add	[postbyte_register],5
	mov	[base_code],0DBh
	jmp	instruction_ready
      fld_streg:
	lods	byte [esi]
	call	convert_fpu_register
	mov	bl,al
	cmp	[postbyte_register],2
	jae	fst_streg
	mov	[base_code],0D9h
	jmp	nomem_instruction_ready
      fst_streg:
	mov	[base_code],0DDh
	jmp	nomem_instruction_ready
fild_instruction:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,2
	je	fild_mem_16bit
	cmp	al,4
	je	fild_mem_32bit
	cmp	al,8
	je	fild_mem_64bit
	or	al,al
	jnz	invalid_operand_size
	call	recoverable_unknown_size
      fild_mem_32bit:
	mov	[base_code],0DBh
	jmp	instruction_ready
      fild_mem_16bit:
	mov	[base_code],0DFh
	jmp	instruction_ready
      fild_mem_64bit:
	mov	al,[postbyte_register]
	cmp	al,1
	je	fisttp_64bit_store
	jb	fild_mem_64bit_store
	dec	[postbyte_register]
	cmp	al,3
	je	fild_mem_64bit_store
	jmp	invalid_operand_size
      fild_mem_64bit_store:
	add	[postbyte_register],5
	mov	[base_code],0DFh
	jmp	instruction_ready
      fisttp_64bit_store:
	mov	[base_code],0DDh
	jmp	instruction_ready
fbld_instruction:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	fbld_mem_80bit
	cmp	al,10
	je	fbld_mem_80bit
	jmp	invalid_operand_size
      fbld_mem_80bit:
	mov	[base_code],0DFh
	jmp	instruction_ready
faddp_instruction:
	mov	[postbyte_register],al
	mov	[base_code],0DEh
	mov	edx,esi
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	faddp_streg
	mov	esi,edx
	mov	bl,1
	jmp	nomem_instruction_ready
      faddp_streg:
	lods	byte [esi]
	call	convert_fpu_register
	mov	bl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_fpu_register
	or	al,al
	jnz	invalid_operand
	jmp	nomem_instruction_ready
fcompp_instruction:
	mov	ax,0D9DEh
	stos	word [edi]
	jmp	instruction_assembled
fucompp_instruction:
	mov	ax,0E9DAh
	stos	word [edi]
	jmp	instruction_assembled
fxch_instruction:
	mov	dx,01D9h
	jmp	fpu_single_operand
ffreep_instruction:
	mov	dx,00DFh
	jmp	fpu_single_operand
ffree_instruction:
	mov	dl,0DDh
	mov	dh,al
      fpu_single_operand:
	mov	ebx,esi
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	fpu_streg
	or	dh,dh
	jz	invalid_operand
	mov	esi,ebx
	shl	dh,3
	or	dh,11000001b
	mov	ax,dx
	stos	word [edi]
	jmp	instruction_assembled
      fpu_streg:
	lods	byte [esi]
	call	convert_fpu_register
	shl	dh,3
	or	dh,al
	or	dh,11000000b
	mov	ax,dx
	stos	word [edi]
	jmp	instruction_assembled

fstenv_instruction:
	mov	byte [edi],9Bh
	inc	edi
fldenv_instruction:
	mov	[base_code],0D9h
	jmp	fpu_mem
fstenv_instruction_16bit:
	mov	byte [edi],9Bh
	inc	edi
fldenv_instruction_16bit:
	call	operand_16bit
	jmp	fldenv_instruction
fstenv_instruction_32bit:
	mov	byte [edi],9Bh
	inc	edi
fldenv_instruction_32bit:
	call	operand_32bit
	jmp	fldenv_instruction
fsave_instruction_32bit:
	mov	byte [edi],9Bh
	inc	edi
fnsave_instruction_32bit:
	call	operand_32bit
	jmp	fnsave_instruction
fsave_instruction_16bit:
	mov	byte [edi],9Bh
	inc	edi
fnsave_instruction_16bit:
	call	operand_16bit
	jmp	fnsave_instruction
fsave_instruction:
	mov	byte [edi],9Bh
	inc	edi
fnsave_instruction:
	mov	[base_code],0DDh
      fpu_mem:
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	jne	invalid_operand_size
	jmp	instruction_ready
fstcw_instruction:
	mov	byte [edi],9Bh
	inc	edi
fldcw_instruction:
	mov	[postbyte_register],al
	mov	[base_code],0D9h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	fldcw_mem_16bit
	cmp	al,2
	je	fldcw_mem_16bit
	jmp	invalid_operand_size
      fldcw_mem_16bit:
	jmp	instruction_ready
fstsw_instruction:
	mov	al,9Bh
	stos	byte [edi]
fnstsw_instruction:
	mov	[base_code],0DDh
	mov	[postbyte_register],7
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	fstsw_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	fstsw_mem_16bit
	cmp	al,2
	je	fstsw_mem_16bit
	jmp	invalid_operand_size
      fstsw_mem_16bit:
	jmp	instruction_ready
      fstsw_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ax,0200h
	jne	invalid_operand
	mov	ax,0E0DFh
	stos	word [edi]
	jmp	instruction_assembled
finit_instruction:
	mov	byte [edi],9Bh
	inc	edi
fninit_instruction:
	mov	ah,al
	mov	al,0DBh
	stos	word [edi]
	jmp	instruction_assembled
fcmov_instruction:
	mov	dh,0DAh
	jmp	fcomi_streg
fcomi_instruction:
	mov	dh,0DBh
	jmp	fcomi_streg
fcomip_instruction:
	mov	dh,0DFh
      fcomi_streg:
	mov	dl,al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_fpu_register
	mov	ah,al
	cmp	byte [esi],','
	je	fcomi_st0_streg
	add	ah,dl
	mov	al,dh
	stos	word [edi]
	jmp	instruction_assembled
      fcomi_st0_streg:
	or	ah,ah
	jnz	invalid_operand
	inc	esi
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_fpu_register
	mov	ah,al
	add	ah,dl
	mov	al,dh
	stos	word [edi]
	jmp	instruction_assembled

basic_mmx_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
      mmx_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	mmx_mmreg_mmreg
	cmp	al,'['
	jne	invalid_operand
      mmx_mmreg_mem:
	call	get_address
	jmp	instruction_ready
      mmx_mmreg_mmreg:
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	jmp	nomem_instruction_ready
mmx_bit_shift_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	mmx_mmreg_mmreg
	cmp	al,'('
	je	mmx_ps_mmreg_imm8
	cmp	al,'['
	je	mmx_mmreg_mem
	jmp	invalid_operand
      mmx_ps_mmreg_imm8:
	call	get_byte_value
	mov	byte [value],al
	test	[operand_size],not 1
	jnz	invalid_value
	mov	bl,[extended_code]
	mov	al,bl
	shr	bl,4
	and	al,1111b
	add	al,70h
	mov	[extended_code],al
	sub	bl,0Ch
	shl	bl,1
	xchg	bl,[postbyte_register]
	call	store_nomem_instruction
	mov	al,byte [value]
	stos	byte [edi]
	jmp	instruction_assembled
pmovmskb_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	je	pmovmskb_reg_size_ok
	cmp	[code_type],64
	jne	invalid_operand_size
	cmp	ah,8
	jnz	invalid_operand_size
      pmovmskb_reg_size_ok:
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	call	make_mmx_prefix
	cmp	[extended_code],0C5h
	je	mmx_nomem_imm8
	jmp	nomem_instruction_ready
      mmx_imm8:
	push	ebx ecx edx
	xor	cl,cl
	xchg	cl,[operand_size]
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	test	ah,not 1
	jnz	invalid_operand_size
	mov	[operand_size],cl
	cmp	al,'('
	jne	invalid_operand
	call	get_byte_value
	mov	byte [value],al
	pop	edx ecx ebx
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      mmx_nomem_imm8:
	call	store_nomem_instruction
	call	append_imm8
	jmp	instruction_assembled
      append_imm8:
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	test	ah,not 1
	jnz	invalid_operand_size
	cmp	al,'('
	jne	invalid_operand
	call	get_byte_value
	stosb
	ret
pinsrw_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pinsrw_mmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	mmx_imm8
	cmp	[operand_size],2
	jne	invalid_operand_size
	jmp	mmx_imm8
      pinsrw_mmreg_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	jne	invalid_operand_size
	mov	bl,al
	jmp	mmx_nomem_imm8
pshufw_instruction:
	mov	[mmx_size],8
	mov	[opcode_prefix],al
	jmp	pshuf_instruction
pshufd_instruction:
	mov	[mmx_size],16
	mov	[opcode_prefix],al
      pshuf_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],70h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,[mmx_size]
	jne	invalid_operand_size
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pshuf_mmreg_mmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	jmp	mmx_imm8
      pshuf_mmreg_mmreg:
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	jmp	mmx_nomem_imm8
movd_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],7Eh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movd_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	test	[operand_size],not 4
	jnz	invalid_operand_size
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	jmp	instruction_ready
      movd_reg:
	lods	byte [esi]
	cmp	al,0B0h
	jae	movd_mmreg
	call	convert_register
	cmp	ah,4
	jne	invalid_operand_size
	mov	[operand_size],0
	mov	bl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	mov	[postbyte_register],al
	call	make_mmx_prefix
	jmp	nomem_instruction_ready
      movd_mmreg:
	mov	[extended_code],6Eh
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movd_mmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	test	[operand_size],not 4
	jnz	invalid_operand_size
	jmp	instruction_ready
      movd_mmreg_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	jne	invalid_operand_size
	mov	bl,al
	jmp	nomem_instruction_ready
      make_mmx_prefix:
	cmp	[vex_required],0
	jne	mmx_prefix_for_vex
	cmp	[operand_size],16
	jne	no_mmx_prefix
	mov	[operand_prefix],66h
      no_mmx_prefix:
	ret
      mmx_prefix_for_vex:
	cmp	[operand_size],16
	jne	invalid_operand
	mov	[opcode_prefix],66h
	ret
movq_instruction:
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movq_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	test	[operand_size],not 8
	jnz	invalid_operand_size
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	mov	[postbyte_register],al
	cmp	ah,16
	je	movq_mem_xmmreg
	mov	[extended_code],7Fh
	jmp	instruction_ready
     movq_mem_xmmreg:
	mov	[extended_code],0D6h
	mov	[opcode_prefix],66h
	jmp	instruction_ready
     movq_reg:
	lods	byte [esi]
	cmp	al,0B0h
	jae	movq_mmreg
	call	convert_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	bl,al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	convert_mmx_register
	mov	[postbyte_register],al
	call	make_mmx_prefix
	mov	[extended_code],7Eh
	call	operand_64bit
	jmp	nomem_instruction_ready
     movq_mmreg:
	call	convert_mmx_register
	mov	[postbyte_register],al
	mov	[extended_code],6Fh
	mov	[mmx_size],ah
	cmp	ah,16
	jne	movq_mmreg_
	mov	[extended_code],7Eh
	mov	[opcode_prefix],0F3h
      movq_mmreg_:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movq_mmreg_reg
	call	get_address
	test	[operand_size],not 8
	jnz	invalid_operand_size
	jmp	instruction_ready
      movq_mmreg_reg:
	lods	byte [esi]
	cmp	al,0B0h
	jae	movq_mmreg_mmreg
	mov	[operand_size],0
	call	convert_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	[extended_code],6Eh
	mov	[opcode_prefix],0
	mov	bl,al
	cmp	[mmx_size],16
	jne	movq_mmreg_reg_store
	mov	[opcode_prefix],66h
      movq_mmreg_reg_store:
	call	operand_64bit
	jmp	nomem_instruction_ready
      movq_mmreg_mmreg:
	call	convert_mmx_register
	cmp	ah,[mmx_size]
	jne	invalid_operand_size
	mov	bl,al
	jmp	nomem_instruction_ready
movdq_instruction:
	mov	[opcode_prefix],al
	mov	[base_code],0Fh
	mov	[extended_code],6Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movdq_mmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	mov	[extended_code],7Fh
	jmp	instruction_ready
      movdq_mmreg:
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	movdq_mmreg_mmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	jmp	instruction_ready
      movdq_mmreg_mmreg:
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	jmp	nomem_instruction_ready
lddqu_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	push	eax
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	pop	eax
	mov	[postbyte_register],al
	mov	[opcode_prefix],0F2h
	mov	[base_code],0Fh
	mov	[extended_code],0F0h
	jmp	instruction_ready

movdq2q_instruction:
	mov	[opcode_prefix],0F2h
	mov	[mmx_size],8
	jmp	movq2dq_
movq2dq_instruction:
	mov	[opcode_prefix],0F3h
	mov	[mmx_size],16
      movq2dq_:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,[mmx_size]
	jne	invalid_operand_size
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	xor	[mmx_size],8+16
	cmp	ah,[mmx_size]
	jne	invalid_operand_size
	mov	bl,al
	mov	[base_code],0Fh
	mov	[extended_code],0D6h
	jmp	nomem_instruction_ready

sse_ps_instruction_imm8:
	mov	[immediate_size],1
sse_ps_instruction:
	mov	[mmx_size],16
	jmp	sse_instruction
sse_pd_instruction_imm8:
	mov	[immediate_size],1
sse_pd_instruction:
	mov	[mmx_size],16
	mov	[opcode_prefix],66h
	jmp	sse_instruction
sse_ss_instruction:
	mov	[mmx_size],4
	mov	[opcode_prefix],0F3h
	jmp	sse_instruction
sse_sd_instruction:
	mov	[mmx_size],8
	mov	[opcode_prefix],0F2h
	jmp	sse_instruction
cmp_pd_instruction:
	mov	[opcode_prefix],66h
cmp_ps_instruction:
	mov	[mmx_size],16
	mov	byte [value],al
	mov	al,0C2h
	jmp	sse_instruction
cmp_ss_instruction:
	mov	[mmx_size],4
	mov	[opcode_prefix],0F3h
	jmp	cmp_sx_instruction
cmpsd_instruction:
	mov	al,0A7h
	mov	ah,[esi]
	or	ah,ah
	jz	simple_instruction_32bit
	cmp	ah,0Fh
	je	simple_instruction_32bit
	mov	al,-1
cmp_sd_instruction:
	mov	[mmx_size],8
	mov	[opcode_prefix],0F2h
      cmp_sx_instruction:
	mov	byte [value],al
	mov	al,0C2h
	jmp	sse_instruction
comiss_instruction:
	mov	[mmx_size],4
	jmp	sse_instruction
comisd_instruction:
	mov	[mmx_size],8
	mov	[opcode_prefix],66h
	jmp	sse_instruction
cvtdq2pd_instruction:
	mov	[opcode_prefix],0F3h
cvtps2pd_instruction:
	mov	[mmx_size],8
	jmp	sse_instruction
cvtpd2dq_instruction:
	mov	[mmx_size],16
	mov	[opcode_prefix],0F2h
	jmp	sse_instruction
movshdup_instruction:
	mov	[mmx_size],16
	mov	[opcode_prefix],0F3h
sse_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
      sse_xmmreg:
	lods	byte [esi]
	call	convert_xmm_register
      sse_reg:
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	sse_xmmreg_xmmreg
      sse_reg_mem:
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	sse_mem_size_ok
	mov	al,[mmx_size]
	cmp	[operand_size],al
	jne	invalid_operand_size
      sse_mem_size_ok:
	mov	al,[extended_code]
	mov	ah,[supplemental_code]
	cmp	al,0C2h
	je	sse_cmp_mem_ok
	cmp	ax,443Ah
	je	sse_cmp_mem_ok
	cmp	[immediate_size],1
	je	mmx_imm8
	cmp	[immediate_size],-1
	jne	sse_ok
	call	take_additional_xmm0
	mov	[immediate_size],0
      sse_ok:
	jmp	instruction_ready
      sse_cmp_mem_ok:
	cmp	byte [value],-1
	je	mmx_imm8
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      sse_xmmreg_xmmreg:
	cmp	[operand_prefix],66h
	jne	sse_xmmreg_xmmreg_ok
	cmp	[extended_code],12h
	je	invalid_operand
	cmp	[extended_code],16h
	je	invalid_operand
      sse_xmmreg_xmmreg_ok:
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	mov	al,[extended_code]
	mov	ah,[supplemental_code]
	cmp	al,0C2h
	je	sse_cmp_nomem_ok
	cmp	ax,443Ah
	je	sse_cmp_nomem_ok
	cmp	[immediate_size],1
	je	mmx_nomem_imm8
	cmp	[immediate_size],-1
	jne	sse_nomem_ok
	call	take_additional_xmm0
	mov	[immediate_size],0
      sse_nomem_ok:
	jmp	nomem_instruction_ready
      sse_cmp_nomem_ok:
	cmp	byte [value],-1
	je	mmx_nomem_imm8
	call	store_nomem_instruction
	mov	al,byte [value]
	stosb
	jmp	instruction_assembled
      take_additional_xmm0:
	cmp	byte [esi],','
	jne	additional_xmm0_ok
	inc	esi
	lods	byte [esi]
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	test	al,al
	jnz	invalid_operand
      additional_xmm0_ok:
	ret

pslldq_instruction:
	mov	[postbyte_register],al
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],73h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	jmp	mmx_nomem_imm8
movpd_instruction:
	mov	[opcode_prefix],66h
movps_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	mov	[mmx_size],16
	jmp	sse_mov_instruction
movss_instruction:
	mov	[mmx_size],4
	mov	[opcode_prefix],0F3h
	jmp	sse_movs
movsd_instruction:
	mov	al,0A5h
	mov	ah,[esi]
	or	ah,ah
	jz	simple_instruction_32bit
	cmp	ah,0Fh
	je	simple_instruction_32bit
	mov	[mmx_size],8
	mov	[opcode_prefix],0F2h
      sse_movs:
	mov	[base_code],0Fh
	mov	[extended_code],10h
	jmp	sse_mov_instruction
sse_mov_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	sse_xmmreg
      sse_mem:
	cmp	al,'['
	jne	invalid_operand
	inc	[extended_code]
	call	get_address
	cmp	[operand_size],0
	je	sse_mem_xmmreg
	mov	al,[mmx_size]
	cmp	[operand_size],al
	jne	invalid_operand_size
	mov	[operand_size],0
      sse_mem_xmmreg:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	jmp	instruction_ready
movlpd_instruction:
	mov	[opcode_prefix],66h
movlps_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	mov	[mmx_size],8
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	sse_mem
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	jmp	sse_reg_mem
movhlps_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	mov	[mmx_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	sse_xmmreg_xmmreg_ok
	jmp	invalid_operand
maskmovq_instruction:
	mov	cl,8
	jmp	maskmov_instruction
maskmovdqu_instruction:
	mov	cl,16
	mov	[opcode_prefix],66h
      maskmov_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],0F7h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,cl
	jne	invalid_operand_size
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	jmp	nomem_instruction_ready
movmskpd_instruction:
	mov	[opcode_prefix],66h
movmskps_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],50h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	cmp	ah,4
	je	movmskps_reg_ok
	cmp	ah,8
	jne	invalid_operand_size
	cmp	[code_type],64
	jne	invalid_operand
      movmskps_reg_ok:
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	sse_xmmreg_xmmreg_ok
	jmp	invalid_operand

cvtpi2pd_instruction:
	mov	[opcode_prefix],66h
cvtpi2ps_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	cvtpi_xmmreg_xmmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	cvtpi_size_ok
	cmp	[operand_size],8
	jne	invalid_operand_size
      cvtpi_size_ok:
	jmp	instruction_ready
      cvtpi_xmmreg_xmmreg:
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	bl,al
	jmp	nomem_instruction_ready
cvtsi2ss_instruction:
	mov	[opcode_prefix],0F3h
	jmp	cvtsi_instruction
cvtsi2sd_instruction:
	mov	[opcode_prefix],0F2h
      cvtsi_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
      cvtsi_xmmreg:
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	cvtsi_xmmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	cvtsi_size_ok
	cmp	[operand_size],4
	je	cvtsi_size_ok
	cmp	[operand_size],8
	jne	invalid_operand_size
	call	operand_64bit
      cvtsi_size_ok:
	jmp	instruction_ready
      cvtsi_xmmreg_reg:
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	je	cvtsi_xmmreg_reg_store
	cmp	ah,8
	jne	invalid_operand_size
	call	operand_64bit
      cvtsi_xmmreg_reg_store:
	mov	bl,al
	jmp	nomem_instruction_ready
cvtps2pi_instruction:
	mov	[mmx_size],8
	jmp	cvtpd_instruction
cvtpd2pi_instruction:
	mov	[opcode_prefix],66h
	mov	[mmx_size],16
      cvtpd_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	[operand_size],0
	jmp	sse_reg
cvtss2si_instruction:
	mov	[opcode_prefix],0F3h
	mov	[mmx_size],4
	jmp	cvt2si_instruction
cvtsd2si_instruction:
	mov	[opcode_prefix],0F2h
	mov	[mmx_size],8
      cvt2si_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[operand_size],0
	cmp	ah,4
	je	sse_reg
	cmp	ah,8
	jne	invalid_operand_size
	call	operand_64bit
	jmp	sse_reg

ssse3_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],38h
	mov	[supplemental_code],al
	jmp	mmx_instruction
palignr_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],3Ah
	mov	[supplemental_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	call	make_mmx_prefix
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	palignr_mmreg_mmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	jmp	mmx_imm8
      palignr_mmreg_mmreg:
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	jmp	mmx_nomem_imm8
amd3dnow_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],0Fh
	mov	byte [value],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	amd3dnow_mmreg_mmreg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	call	store_instruction_with_imm8
	jmp	instruction_assembled
      amd3dnow_mmreg_mmreg:
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,8
	jne	invalid_operand_size
	mov	bl,al
	call	store_nomem_instruction
	mov	al,byte [value]
	stos	byte [edi]
	jmp	instruction_assembled

sse4_instruction_38_xmm0:
	mov	[immediate_size],-1
sse4_instruction_38:
	mov	[mmx_size],16
	mov	[opcode_prefix],66h
	mov	[supplemental_code],al
	mov	al,38h
	jmp	sse_instruction
sse4_ss_instruction_3a_imm8:
	mov	[immediate_size],1
	mov	[mmx_size],4
	jmp	sse4_instruction_3a_setup
sse4_sd_instruction_3a_imm8:
	mov	[immediate_size],1
	mov	[mmx_size],8
	jmp	sse4_instruction_3a_setup
sse4_instruction_3a_imm8:
	mov	[immediate_size],1
	mov	[mmx_size],16
      sse4_instruction_3a_setup:
	mov	[opcode_prefix],66h
	mov	[supplemental_code],al
	mov	al,3Ah
	jmp	sse_instruction
pclmulqdq_instruction:
	mov	byte [value],al
	mov	[mmx_size],16
	mov	al,44h
	jmp	sse4_instruction_3a_setup
extractps_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],3Ah
	mov	[supplemental_code],17h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	extractps_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],4
	je	extractps_size_ok
	cmp	[operand_size],0
	jne	invalid_operand_size
      extractps_size_ok:
	push	edx ebx ecx
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	jmp	mmx_imm8
      extractps_reg:
	lods	byte [esi]
	call	convert_register
	push	eax
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	pop	ebx
	mov	al,bh
	cmp	al,4
	je	mmx_nomem_imm8
	cmp	al,8
	jne	invalid_operand_size
	call	operand_64bit
	jmp	mmx_nomem_imm8
insertps_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
      insertps_xmmreg:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],3Ah
	mov	[supplemental_code],21h
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	insertps_xmmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],4
	je	insertps_size_ok
	cmp	[operand_size],0
	jne	invalid_operand_size
      insertps_size_ok:
	jmp	mmx_imm8
      insertps_xmmreg_reg:
	lods	byte [esi]
	call	convert_mmx_register
	mov	bl,al
	jmp	mmx_nomem_imm8
pextrq_instruction:
	mov	[mmx_size],8
	jmp	pextr_instruction
pextrd_instruction:
	mov	[mmx_size],4
	jmp	pextr_instruction
pextrw_instruction:
	mov	[mmx_size],2
	jmp	pextr_instruction
pextrb_instruction:
	mov	[mmx_size],1
      pextr_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],3Ah
	mov	[supplemental_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pextr_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[mmx_size]
	cmp	al,[operand_size]
	je	pextr_size_ok
	cmp	[operand_size],0
	jne	invalid_operand_size
      pextr_size_ok:
	cmp	al,8
	jne	pextr_prefix_ok
	call	operand_64bit
      pextr_prefix_ok:
	push	edx ebx ecx
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	jmp	mmx_imm8
      pextr_reg:
	lods	byte [esi]
	call	convert_register
	cmp	[mmx_size],4
	ja	pextrq_reg
	cmp	ah,4
	je	pextr_reg_size_ok
	cmp	[code_type],64
	jne	pextr_invalid_size
	cmp	ah,8
	je	pextr_reg_size_ok
      pextr_invalid_size:
	jmp	invalid_operand_size
      pextrq_reg:
	cmp	ah,8
	jne	pextr_invalid_size
	call	operand_64bit
      pextr_reg_size_ok:
	mov	[operand_size],0
	push	eax
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	mov	ebx,eax
	pop	eax
	mov	[postbyte_register],al
	mov	al,ah
	cmp	[mmx_size],2
	jne	pextr_reg_store
	mov	[opcode_prefix],0
	mov	[extended_code],0C5h
	call	make_mmx_prefix
	jmp	mmx_nomem_imm8
      pextr_reg_store:
	cmp	bh,16
	jne	invalid_operand_size
	xchg	bl,[postbyte_register]
	call	operand_autodetect
	jmp	mmx_nomem_imm8
pinsrb_instruction:
	mov	[mmx_size],1
	jmp	pinsr_instruction
pinsrd_instruction:
	mov	[mmx_size],4
	jmp	pinsr_instruction
pinsrq_instruction:
	mov	[mmx_size],8
	call	operand_64bit
      pinsr_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],3Ah
	mov	[supplemental_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
      pinsr_xmmreg:
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pinsr_xmmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	mmx_imm8
	mov	al,[mmx_size]
	cmp	al,[operand_size]
	je	mmx_imm8
	jmp	invalid_operand_size
      pinsr_xmmreg_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	cmp	[mmx_size],8
	je	pinsrq_xmmreg_reg
	cmp	ah,4
	je	mmx_nomem_imm8
	jmp	invalid_operand_size
      pinsrq_xmmreg_reg:
	cmp	ah,8
	je	mmx_nomem_imm8
	jmp	invalid_operand_size
pmovsxbw_instruction:
	mov	[mmx_size],8
	jmp	pmovsx_instruction
pmovsxbd_instruction:
	mov	[mmx_size],4
	jmp	pmovsx_instruction
pmovsxbq_instruction:
	mov	[mmx_size],2
	jmp	pmovsx_instruction
pmovsxwd_instruction:
	mov	[mmx_size],8
	jmp	pmovsx_instruction
pmovsxwq_instruction:
	mov	[mmx_size],4
	jmp	pmovsx_instruction
pmovsxdq_instruction:
	mov	[mmx_size],8
      pmovsx_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],38h
	mov	[supplemental_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	pmovsx_xmmreg_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	cmp	[operand_size],0
	je	instruction_ready
	mov	al,[mmx_size]
	cmp	al,[operand_size]
	jne	invalid_operand_size
	jmp	instruction_ready
      pmovsx_xmmreg_reg:
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	jmp	nomem_instruction_ready

fxsave_instruction_64bit:
	call	operand_64bit
fxsave_instruction:
	mov	[extended_code],0AEh
	mov	[base_code],0Fh
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	ah,[operand_size]
	or	ah,ah
	jz	fxsave_size_ok
	mov	al,[postbyte_register]
	cmp	al,111b
	je	clflush_size_check
	cmp	al,10b
	jb	invalid_operand_size
	cmp	al,11b
	ja	invalid_operand_size
	cmp	ah,4
	jne	invalid_operand_size
	jmp	fxsave_size_ok
      clflush_size_check:
	cmp	ah,1
	jne	invalid_operand_size
      fxsave_size_ok:
	jmp	instruction_ready
prefetch_instruction:
	mov	[extended_code],18h
      prefetch_mem_8bit:
	mov	[base_code],0Fh
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	or	ah,ah
	jz	prefetch_size_ok
	cmp	ah,1
	jne	invalid_operand_size
      prefetch_size_ok:
	call	get_address
	jmp	instruction_ready
amd_prefetch_instruction:
	mov	[extended_code],0Dh
	jmp	prefetch_mem_8bit
fence_instruction:
	mov	bl,al
	mov	ax,0AE0Fh
	stos	word [edi]
	mov	al,bl
	stos	byte [edi]
	jmp	instruction_assembled
pause_instruction:
	mov	ax,90F3h
	stos	word [edi]
	jmp	instruction_assembled
movntq_instruction:
	mov	[mmx_size],8
	jmp	movnt_instruction
movntpd_instruction:
	mov	[opcode_prefix],66h
movntps_instruction:
	mov	[mmx_size],16
      movnt_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_mmx_register
	cmp	ah,[mmx_size]
	jne	invalid_operand_size
	mov	[postbyte_register],al
	jmp	instruction_ready

movntsd_instruction:
	mov	[opcode_prefix],0F2h
	mov	[mmx_size],8
	jmp	movnts_instruction
movntss_instruction:
	mov	[opcode_prefix],0F3h
	mov	[mmx_size],4
      movnts_instruction:
	mov	[extended_code],al
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	cmp	al,[mmx_size]
	je	movnts_size_ok
	test	al,al
	jnz	invalid_operand_size
      movnts_size_ok:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	jmp	instruction_ready

movnti_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ah,4
	je	movnti_store
	cmp	ah,8
	jne	invalid_operand_size
	call	operand_64bit
      movnti_store:
	mov	[postbyte_register],al
	jmp	instruction_ready
monitor_instruction:
	mov	[postbyte_register],al
	cmp	byte [esi],0
	je	monitor_instruction_store
	cmp	byte [esi],0Fh
	je	monitor_instruction_store
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ax,0400h
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ax,0401h
	jne	invalid_operand
	cmp	[postbyte_register],0C8h
	jne	monitor_instruction_store
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ax,0402h
	jne	invalid_operand
      monitor_instruction_store:
	mov	ax,010Fh
	stos	word [edi]
	mov	al,[postbyte_register]
	stos	byte [edi]
	jmp	instruction_assembled
movntdqa_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],38h
	mov	[supplemental_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	jmp	instruction_ready

extrq_instruction:
	mov	[opcode_prefix],66h
	mov	[base_code],0Fh
	mov	[extended_code],78h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	extrq_xmmreg_xmmreg
	test	ah,not 1
	jnz	invalid_operand_size
	cmp	al,'('
	jne	invalid_operand
	xor	bl,bl
	xchg	bl,[postbyte_register]
	call	store_nomem_instruction
	call	get_byte_value
	stosb
	call	append_imm8
	jmp	instruction_assembled
      extrq_xmmreg_xmmreg:
	inc	[extended_code]
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	jmp	nomem_instruction_ready
insertq_instruction:
	mov	[opcode_prefix],0F2h
	mov	[base_code],0Fh
	mov	[extended_code],78h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	[postbyte_register],al
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_xmm_register
	mov	bl,al
	cmp	byte [esi],','
	je	insertq_with_imm
	inc	[extended_code]
	jmp	nomem_instruction_ready
      insertq_with_imm:
	call	store_nomem_instruction
	call	append_imm8
	call	append_imm8
	jmp	instruction_assembled

crc32_instruction:
	mov	[opcode_prefix],0F2h
	mov	[base_code],0Fh
	mov	[extended_code],38h
	mov	[supplemental_code],0F0h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	cmp	ah,8
	je	crc32_reg64
	cmp	ah,4
	jne	invalid_operand
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	crc32_reg32_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	test	al,al
	jz	crc32_unknown_size
	cmp	al,1
	je	crc32_reg32_mem_store
	cmp	al,4
	ja	invalid_operand_size
	inc	[supplemental_code]
	call	operand_autodetect
      crc32_reg32_mem_store:
	jmp	instruction_ready
      crc32_unknown_size:
	call	recoverable_unknown_size
	jmp	crc32_reg32_mem_store
      crc32_reg32_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	cmp	al,1
	je	crc32_reg32_reg_store
	cmp	al,4
	ja	invalid_operand_size
	inc	[supplemental_code]
	call	operand_autodetect
      crc32_reg32_reg_store:
	jmp	nomem_instruction_ready
      crc32_reg64:
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	mov	[operand_size],0
	call	operand_64bit
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	crc32_reg64_reg
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	ah,[operand_size]
	mov	al,8
	test	ah,ah
	jz	crc32_unknown_size
	cmp	ah,1
	je	crc32_reg32_mem_store
	cmp	ah,al
	jne	invalid_operand_size
	inc	[supplemental_code]
	jmp	crc32_reg32_mem_store
      crc32_reg64_reg:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,8
	cmp	ah,1
	je	crc32_reg32_reg_store
	cmp	ah,al
	jne	invalid_operand_size
	inc	[supplemental_code]
	jmp	crc32_reg32_reg_store
popcnt_instruction:
	mov	[opcode_prefix],0F3h
	jmp	bs_instruction
movbe_instruction:
	mov	[supplemental_code],al
	mov	[extended_code],38h
	mov	[base_code],0Fh
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	je	movbe_mem
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_argument
	call	get_address
	mov	al,[operand_size]
	call	operand_autodetect
	jmp	instruction_ready
      movbe_mem:
	inc	[supplemental_code]
	call	get_address
	push	edx ebx ecx
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	pop	ecx ebx edx
	mov	al,[operand_size]
	call	operand_autodetect
	jmp	instruction_ready
adx_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],38h
	mov	[supplemental_code],0F6h
	mov	[operand_prefix],al
	call	get_reg_mem
	jc	adx_reg_reg
	mov	al,[operand_size]
	cmp	al,4
	je	instruction_ready
	cmp	al,8
	jne	invalid_operand_size
	call	operand_64bit
	jmp	instruction_ready
      adx_reg_reg:
	cmp	ah,4
	je	nomem_instruction_ready
	cmp	ah,8
	jne	invalid_operand_size
	call	operand_64bit
	jmp	nomem_instruction_ready

simple_vmx_instruction:
	mov	ah,al
	mov	al,0Fh
	stos	byte [edi]
	mov	al,1
	stos	word [edi]
	jmp	instruction_assembled
vmclear_instruction:
	mov	[opcode_prefix],66h
	jmp	vmx_instruction
vmxon_instruction:
	mov	[opcode_prefix],0F3h
vmx_instruction:
	mov	[postbyte_register],al
	mov	[extended_code],0C7h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	vmx_size_ok
	cmp	al,8
	jne	invalid_operand_size
      vmx_size_ok:
	mov	[base_code],0Fh
	jmp	instruction_ready
vmread_instruction:
	mov	[extended_code],78h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	vmread_nomem
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	call	vmread_check_size
	jmp	vmx_size_ok
      vmread_nomem:
	lods	byte [esi]
	call	convert_register
	push	eax
	call	vmread_check_size
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	call	vmread_check_size
	pop	ebx
	mov	[base_code],0Fh
	jmp	nomem_instruction_ready
      vmread_check_size:
	cmp	[code_type],64
	je	vmread_long
	cmp	[operand_size],4
	jne	invalid_operand_size
	ret
      vmread_long:
	cmp	[operand_size],8
	jne	invalid_operand_size
	ret
vmwrite_instruction:
	mov	[extended_code],79h
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	je	vmwrite_nomem
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	call	vmread_check_size
	jmp	vmx_size_ok
      vmwrite_nomem:
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	[base_code],0Fh
	jmp	nomem_instruction_ready
vmx_inv_instruction:
	mov	[opcode_prefix],66h
	mov	[extended_code],38h
	mov	[supplemental_code],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	[postbyte_register],al
	call	vmread_check_size
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,'['
	jne	invalid_operand
	call	get_address
	mov	al,[operand_size]
	or	al,al
	jz	vmx_size_ok
	cmp	al,16
	jne	invalid_operand_size
	jmp	vmx_size_ok
simple_svm_instruction:
	push	eax
	mov	[base_code],0Fh
	mov	[extended_code],1
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	or	al,al
	jnz	invalid_operand
      simple_svm_detect_size:
	cmp	ah,2
	je	simple_svm_16bit
	cmp	ah,4
	je	simple_svm_32bit
	cmp	[code_type],64
	jne	invalid_operand_size
	jmp	simple_svm_store
      simple_svm_16bit:
	cmp	[code_type],16
	je	simple_svm_store
	cmp	[code_type],64
	je	invalid_operand_size
	jmp	prefixed_svm_store
      simple_svm_32bit:
	cmp	[code_type],32
	je	simple_svm_store
      prefixed_svm_store:
	mov	al,67h
	stos	byte [edi]
      simple_svm_store:
	call	store_instruction_code
	pop	eax
	stos	byte [edi]
	jmp	instruction_assembled
skinit_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ax,0400h
	jne	invalid_operand
	mov	al,0DEh
	jmp	simple_vmx_instruction
invlpga_instruction:
	push	eax
	mov	[base_code],0Fh
	mov	[extended_code],1
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	or	al,al
	jnz	invalid_operand
	mov	bl,ah
	mov	[operand_size],0
	lods	byte [esi]
	cmp	al,','
	jne	invalid_operand
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	cmp	ax,0401h
	jne	invalid_operand
	mov	ah,bl
	jmp	simple_svm_detect_size

rdrand_instruction:
	mov	[base_code],0Fh
	mov	[extended_code],0C7h
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	call	operand_autodetect
	jmp	nomem_instruction_ready
rdfsbase_instruction:
	cmp	[code_type],64
	jne	illegal_instruction
	mov	[opcode_prefix],0F3h
	mov	[base_code],0Fh
	mov	[extended_code],0AEh
	mov	[postbyte_register],al
	lods	byte [esi]
	call	get_size_operator
	cmp	al,10h
	jne	invalid_operand
	lods	byte [esi]
	call	convert_register
	mov	bl,al
	mov	al,ah
	cmp	ah,2
	je	invalid_operand_size
	call	operand_autodetect
	jmp	nomem_instruction_ready

xabort_instruction:
	lods	byte [esi]
	call	get_size_operator
	cmp	ah,1
	ja	invalid_operand_size
	cmp	al,'('
	jne	invalid_operand
	call	get_byte_value
	mov	dl,al
	mov	ax,0F8C6h
	stos	word [edi]
	mov	al,dl
	stos	byte [edi]
	jmp	instruction_assembled
xbegin_instruction:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_operand
	mov	al,[code_type]
	cmp	al,64
	je	xbegin_64bit
	cmp	al,32
	je	xbegin_32bit
      xbegin_16bit:
	call	get_address_word_value
	add	edi,4
	mov	ebp,[addressing_space]
	call	calculate_relative_offset
	sub	edi,4
	shl	eax,16
	mov	ax,0F8C7h
	stos	dword [edi]
	jmp	instruction_assembled
      xbegin_32bit:
	call	get_address_dword_value
	jmp	xbegin_address_ok
      xbegin_64bit:
	call	get_address_qword_value
      xbegin_address_ok:
	add	edi,5
	mov	ebp,[addressing_space]
	call	calculate_relative_offset
	sub	edi,5
	mov	edx,eax
	cwde
	cmp	eax,edx
	jne	xbegin_rel32
	mov	al,66h
	stos	byte [edi]
	mov	eax,edx
	shl	eax,16
	mov	ax,0F8C7h
	stos	dword [edi]
	jmp	instruction_assembled
      xbegin_rel32:
	sub	edx,1
	jno	xbegin_rel32_ok
	cmp	[code_type],64
	je	relative_jump_out_of_range
      xbegin_rel32_ok:
	mov	ax,0F8C7h
	stos	word [edi]
	mov	eax,edx
	stos	dword [edi]
	jmp	instruction_assembled

convert_register:
	mov	ah,al
	shr	ah,4
	and	al,0Fh
	cmp	ah,8
	je	match_register_size
	cmp	ah,4
	ja	invalid_operand
	cmp	ah,1
	ja	match_register_size
	cmp	al,4
	jb	match_register_size
	or	ah,ah
	jz	high_byte_register
	or	[rex_prefix],40h
      match_register_size:
	cmp	ah,[operand_size]
	je	register_size_ok
	cmp	[operand_size],0
	jne	operand_sizes_do_not_match
	mov	[operand_size],ah
      register_size_ok:
	ret
      high_byte_register:
	mov	ah,1
	or	[rex_prefix],80h
	jmp	match_register_size
convert_fpu_register:
	mov	ah,al
	shr	ah,4
	and	al,111b
	cmp	ah,10
	jne	invalid_operand
	jmp	match_register_size
convert_mmx_register:
	mov	ah,al
	shr	ah,4
	cmp	ah,0Ch
	je	xmm_register
	ja	invalid_operand
	and	al,111b
	cmp	ah,0Bh
	jne	invalid_operand
	mov	ah,8
	cmp	[vex_required],0
	jne	invalid_operand
	jmp	match_register_size
      xmm_register:
	and	al,0Fh
	mov	ah,16
	cmp	al,8
	jb	match_register_size
	cmp	[code_type],64
	jne	invalid_operand
	jmp	match_register_size
convert_xmm_register:
	mov	ah,al
	shr	ah,4
	cmp	ah,0Ch
	je	xmm_register
	jmp	invalid_operand
get_size_operator:
	xor	ah,ah
	cmp	al,11h
	jne	no_size_operator
	mov	[size_declared],1
	lods	word [esi]
	xchg	al,ah
	mov	[size_override],1
	cmp	ah,[operand_size]
	je	size_operator_ok
	cmp	[operand_size],0
	jne	operand_sizes_do_not_match
	mov	[operand_size],ah
      size_operator_ok:
	ret
      no_size_operator:
	mov	[size_declared],0
	cmp	al,'['
	jne	size_operator_ok
	mov	[size_override],0
	ret
get_jump_operator:
	mov	[jump_type],0
	cmp	al,12h
	jne	jump_operator_ok
	lods	word [esi]
	mov	[jump_type],al
	mov	al,ah
      jump_operator_ok:
	ret
get_address:
	mov	[segment_register],0
	mov	[address_size],0
	mov	[free_address_range],0
	mov	al,[code_type]
	shr	al,3
	mov	[value_size],al
	mov	al,[esi]
	and	al,11110000b
	cmp	al,60h
	jne	get_size_prefix
	lods	byte [esi]
	sub	al,60h
	mov	[segment_register],al
	mov	al,[esi]
	and	al,11110000b
      get_size_prefix:
	cmp	al,70h
	jne	address_size_prefix_ok
	lods	byte [esi]
	sub	al,70h
	cmp	al,2
	jb	invalid_address_size
	cmp	al,8
	ja	invalid_address_size
	mov	[address_size],al
	mov	[value_size],al
      address_size_prefix_ok:
	call	calculate_address
	cmp	byte [esi-1],']'
	jne	invalid_address
	mov	[address_high],edx
	mov	edx,eax
	cmp	[code_type],64
	jne	address_ok
	or	bx,bx
	jnz	address_ok
	test	ch,0Fh
	jnz	address_ok
      calculate_relative_address:
	mov	edx,[address_symbol]
	mov	[symbol_identifier],edx
	mov	edx,[address_high]
	mov	ebp,[addressing_space]
	call	calculate_relative_offset
	mov	[address_high],edx
	cdq
	cmp	edx,[address_high]
	je	address_high_ok
	call	recoverable_overflow
      address_high_ok:
	mov	edx,eax
	ror	ecx,16
	mov	cl,[value_type]
	rol	ecx,16
	mov	bx,0FF00h
      address_ok:
	ret
operand_16bit:
	cmp	[code_type],16
	je	size_prefix_ok
	mov	[operand_prefix],66h
	ret
operand_32bit:
	cmp	[code_type],16
	jne	size_prefix_ok
	mov	[operand_prefix],66h
      size_prefix_ok:
	ret
operand_64bit:
	cmp	[code_type],64
	jne	illegal_instruction
	or	[rex_prefix],48h
	ret
operand_autodetect:
	cmp	al,2
	je	operand_16bit
	cmp	al,4
	je	operand_32bit
	cmp	al,8
	je	operand_64bit
	jmp	invalid_operand_size
store_segment_prefix_if_necessary:
	mov	al,[segment_register]
	or	al,al
	jz	segment_prefix_ok
	cmp	al,4
	ja	segment_prefix_386
	cmp	[code_type],64
	je	segment_prefix_ok
	cmp	al,3
	je	ss_prefix
	jb	segment_prefix_86
	cmp	bl,25h
	je	segment_prefix_86
	cmp	bh,25h
	je	segment_prefix_86
	cmp	bh,45h
	je	segment_prefix_86
	cmp	bh,44h
	je	segment_prefix_86
	ret
      ss_prefix:
	cmp	bl,25h
	je	segment_prefix_ok
	cmp	bh,25h
	je	segment_prefix_ok
	cmp	bh,45h
	je	segment_prefix_ok
	cmp	bh,44h
	je	segment_prefix_ok
	jmp	segment_prefix_86
store_segment_prefix:
	mov	al,[segment_register]
	or	al,al
	jz	segment_prefix_ok
	cmp	al,5
	jae	segment_prefix_386
      segment_prefix_86:
	dec	al
	shl	al,3
	add	al,26h
	stos	byte [edi]
	jmp	segment_prefix_ok
      segment_prefix_386:
	add	al,64h-5
	stos	byte [edi]
      segment_prefix_ok:
	ret
store_instruction_code:
	cmp	[vex_required],0
	jne	store_vex_instruction_code
	mov	al,[operand_prefix]
	or	al,al
	jz	operand_prefix_ok
	stos	byte [edi]
      operand_prefix_ok:
	mov	al,[opcode_prefix]
	or	al,al
	jz	opcode_prefix_ok
	stos	byte [edi]
      opcode_prefix_ok:
	mov	al,[rex_prefix]
	test	al,40h
	jz	rex_prefix_ok
	cmp	[code_type],64
	jne	invalid_operand
	test	al,0B0h
	jnz	disallowed_combination_of_registers
	stos	byte [edi]
      rex_prefix_ok:
	mov	al,[base_code]
	stos	byte [edi]
	cmp	al,0Fh
	jne	instruction_code_ok
      store_extended_code:
	mov	al,[extended_code]
	stos	byte [edi]
	cmp	al,38h
	je	store_supplemental_code
	cmp	al,3Ah
	je	store_supplemental_code
      instruction_code_ok:
	ret
      store_supplemental_code:
	mov	al,[supplemental_code]
	stos	byte [edi]
	ret
store_nomem_instruction:
	test	[postbyte_register],1000b
	jz	nomem_reg_code_ok
	or	[rex_prefix],44h
	and	[postbyte_register],111b
      nomem_reg_code_ok:
	test	bl,1000b
	jz	nomem_rm_code_ok
	or	[rex_prefix],41h
	and	bl,111b
      nomem_rm_code_ok:
	call	store_instruction_code
	mov	al,[postbyte_register]
	shl	al,3
	or	al,bl
	or	al,11000000b
	stos	byte [edi]
	ret
store_instruction:
	mov	[current_offset],edi
	test	[postbyte_register],1000b
	jz	reg_code_ok
	or	[rex_prefix],44h
	and	[postbyte_register],111b
      reg_code_ok:
	cmp	[code_type],64
	jne	address_value_ok
	xor	eax,eax
	bt	edx,31
	sbb	eax,[address_high]
	jz	address_value_ok
	cmp	[address_high],0
	jne	address_value_out_of_range
	test	ch,44h
	jnz	address_value_ok
	test	bx,8080h
	jz	address_value_ok
      address_value_out_of_range:
	call	recoverable_overflow
      address_value_ok:
	call	store_segment_prefix_if_necessary
	test	[vex_required],4
	jnz	address_vsib
	or	bx,bx
	jz	address_immediate
	cmp	bx,0F800h
	je	address_rip_based
	cmp	bx,0F400h
	je	address_eip_based
	cmp	bx,0FF00h
	je	address_relative
	mov	al,bl
	or	al,bh
	and	al,11110000b
	cmp	al,80h
	je	postbyte_64bit
	cmp	al,40h
	je	postbyte_32bit
	cmp	al,20h
	jne	invalid_address
	cmp	[code_type],64
	je	invalid_address_size
	call	address_16bit_prefix
	call	store_instruction_code
	cmp	bl,bh
	jbe	determine_16bit_address
	xchg	bl,bh
      determine_16bit_address:
	cmp	bx,2600h
	je	address_si
	cmp	bx,2700h
	je	address_di
	cmp	bx,2300h
	je	address_bx
	cmp	bx,2500h
	je	address_bp
	cmp	bx,2625h
	je	address_bp_si
	cmp	bx,2725h
	je	address_bp_di
	cmp	bx,2723h
	je	address_bx_di
	cmp	bx,2623h
	jne	invalid_address
      address_bx_si:
	xor	al,al
	jmp	postbyte_16bit
      address_bx_di:
	mov	al,1
	jmp	postbyte_16bit
      address_bp_si:
	mov	al,10b
	jmp	postbyte_16bit
      address_bp_di:
	mov	al,11b
	jmp	postbyte_16bit
      address_si:
	mov	al,100b
	jmp	postbyte_16bit
      address_di:
	mov	al,101b
	jmp	postbyte_16bit
      address_bx:
	mov	al,111b
	jmp	postbyte_16bit
      address_bp:
	mov	al,110b
      postbyte_16bit:
	test	ch,22h
	jnz	address_16bit_value
	or	ch,ch
	jnz	address_sizes_do_not_agree
	cmp	edx,10000h
	jge	value_out_of_range
	cmp	edx,-8000h
	jl	value_out_of_range
	or	dx,dx
	jz	address
	cmp	dx,80h
	jb	address_8bit_value
	cmp	dx,-80h
	jae	address_8bit_value
      address_16bit_value:
	or	al,10000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	mov	eax,edx
	stos	word [edi]
	ret
      address_8bit_value:
	or	al,01000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	mov	al,dl
	stos	byte [edi]
	cmp	dx,80h
	jge	value_out_of_range
	cmp	dx,-80h
	jl	value_out_of_range
	ret
      address:
	cmp	al,110b
	je	address_8bit_value
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	ret
      address_vsib:
	mov	al,bl
	shr	al,4
	cmp	al,0Ch
	je	vector_index_ok
	cmp	al,0Dh
	jne	invalid_address
      vector_index_ok:
	mov	al,bh
	shr	al,4
	cmp	al,4
	je	postbyte_32bit
	cmp	[code_type],64
	je	address_prefix_ok
	test	al,al
	jnz	invalid_address
      postbyte_32bit:
	call	address_32bit_prefix
	jmp	address_prefix_ok
      postbyte_64bit:
	cmp	[code_type],64
	jne	invalid_address_size
      address_prefix_ok:
	cmp	bl,44h
	je	invalid_address
	cmp	bl,84h
	je	invalid_address
	test	bh,1000b
	jz	base_code_ok
	or	[rex_prefix],41h
      base_code_ok:
	test	bl,1000b
	jz	index_code_ok
	or	[rex_prefix],42h
      index_code_ok:
	call	store_instruction_code
	or	cl,cl
	jz	only_base_register
      base_and_index:
	mov	al,100b
	xor	ah,ah
	cmp	cl,1
	je	scale_ok
	cmp	cl,2
	je	scale_1
	cmp	cl,4
	je	scale_2
	or	ah,11000000b
	jmp	scale_ok
      scale_2:
	or	ah,10000000b
	jmp	scale_ok
      scale_1:
	or	ah,01000000b
      scale_ok:
	or	bh,bh
	jz	only_index_register
	and	bl,111b
	shl	bl,3
	or	ah,bl
	and	bh,111b
	or	ah,bh
      sib_ready:
	test	ch,44h
	jnz	sib_address_32bit_value
	test	ch,88h
	jnz	sib_address_32bit_value
	or	ch,ch
	jnz	address_sizes_do_not_agree
	cmp	bh,5
	je	address_value
	or	edx,edx
	jz	sib_address
      address_value:
	cmp	edx,80h
	jb	sib_address_8bit_value
	cmp	edx,-80h
	jae	sib_address_8bit_value
      sib_address_32bit_value:
	or	al,10000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	word [edi]
	jmp	store_address_32bit_value
      sib_address_8bit_value:
	or	al,01000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	word [edi]
	mov	al,dl
	stos	byte [edi]
	cmp	edx,80h
	jge	value_out_of_range
	cmp	edx,-80h
	jl	value_out_of_range
	ret
      sib_address:
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	word [edi]
	ret
      only_index_register:
	or	ah,101b
	and	bl,111b
	shl	bl,3
	or	ah,bl
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	word [edi]
	test	ch,44h
	jnz	store_address_32bit_value
	test	ch,88h
	jnz	store_address_32bit_value
	or	ch,ch
	jnz	invalid_address_size
	jmp	store_address_32bit_value
      zero_index_register:
	mov	bl,4
	mov	cl,1
	jmp	base_and_index
      only_base_register:
	mov	al,bh
	and	al,111b
	cmp	al,4
	je	zero_index_register
	test	ch,44h
	jnz	simple_address_32bit_value
	test	ch,88h
	jnz	simple_address_32bit_value
	or	ch,ch
	jnz	address_sizes_do_not_agree
	or	edx,edx
	jz	simple_address
	cmp	edx,80h
	jb	simple_address_8bit_value
	cmp	edx,-80h
	jae	simple_address_8bit_value
      simple_address_32bit_value:
	or	al,10000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	jmp	store_address_32bit_value
      simple_address_8bit_value:
	or	al,01000000b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	mov	al,dl
	stos	byte [edi]
	cmp	edx,80h
	jge	value_out_of_range
	cmp	edx,-80h
	jl	value_out_of_range
	ret
      simple_address:
	cmp	al,5
	je	simple_address_8bit_value
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	ret
      address_immediate:
	cmp	[code_type],64
	je	address_immediate_sib
	test	ch,44h
	jnz	address_immediate_32bit
	test	ch,88h
	jnz	address_immediate_32bit
	test	ch,22h
	jnz	address_immediate_16bit
	or	ch,ch
	jnz	invalid_address_size
	cmp	[code_type],16
	je	addressing_16bit
      address_immediate_32bit:
	call	address_32bit_prefix
	call	store_instruction_code
      store_immediate_address:
	mov	al,101b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
      store_address_32bit_value:
	test	ch,0F0h
	jz	address_32bit_relocation_ok
	mov	eax,ecx
	shr	eax,16
	cmp	al,4
	jne	address_32bit_relocation
	mov	al,2
      address_32bit_relocation:
	xchg	[value_type],al
	mov	ebx,[address_symbol]
	xchg	ebx,[symbol_identifier]
	call	mark_relocation
	mov	[value_type],al
	mov	[symbol_identifier],ebx
      address_32bit_relocation_ok:
	mov	eax,edx
	stos	dword [edi]
	ret
      store_address_64bit_value:
	test	ch,0F0h
	jz	address_64bit_relocation_ok
	mov	eax,ecx
	shr	eax,16
	xchg	[value_type],al
	mov	ebx,[address_symbol]
	xchg	ebx,[symbol_identifier]
	call	mark_relocation
	mov	[value_type],al
	mov	[symbol_identifier],ebx
      address_64bit_relocation_ok:
	mov	eax,edx
	stos	dword [edi]
	mov	eax,[address_high]
	stos	dword [edi]
	ret
      address_immediate_sib:
	test	ch,44h
	jnz	address_immediate_sib_32bit
	test	ch,not 88h
	jnz	invalid_address_size
      address_immediate_sib_store:
	call	store_instruction_code
	mov	al,100b
	mov	ah,100101b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	word [edi]
	jmp	store_address_32bit_value
      address_immediate_sib_32bit:
	test	ecx,0FF0000h
	jnz	address_immediate_sib_nosignextend
	test	edx,80000000h
	jz	address_immediate_sib_store
      address_immediate_sib_nosignextend:
	call	address_32bit_prefix
	jmp	address_immediate_sib_store
      address_eip_based:
	mov	al,67h
	stos	byte [edi]
      address_rip_based:
	cmp	[code_type],64
	jne	invalid_address
	call	store_instruction_code
	jmp	store_immediate_address
      address_relative:
	call	store_instruction_code
	movzx	eax,[immediate_size]
	add	eax,edi
	sub	eax,[current_offset]
	add	eax,5
	sub	edx,eax
	jo	value_out_of_range
	mov	al,101b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	shr	ecx,16
	xchg	[value_type],cl
	mov	ebx,[address_symbol]
	xchg	ebx,[symbol_identifier]
	mov	eax,edx
	call	mark_relocation
	mov	[value_type],cl
	mov	[symbol_identifier],ebx
	stos	dword [edi]
	ret
      addressing_16bit:
	cmp	edx,10000h
	jge	address_immediate_32bit
	cmp	edx,-8000h
	jl	address_immediate_32bit
	movzx	edx,dx
      address_immediate_16bit:
	call	address_16bit_prefix
	call	store_instruction_code
	mov	al,110b
	mov	cl,[postbyte_register]
	shl	cl,3
	or	al,cl
	stos	byte [edi]
	mov	eax,edx
	stos	word [edi]
	cmp	edx,10000h
	jge	value_out_of_range
	cmp	edx,-8000h
	jl	value_out_of_range
	ret
      address_16bit_prefix:
	cmp	[code_type],16
	je	instruction_prefix_ok
	mov	al,67h
	stos	byte [edi]
	ret
      address_32bit_prefix:
	cmp	[code_type],32
	je	instruction_prefix_ok
	mov	al,67h
	stos	byte [edi]
      instruction_prefix_ok:
	ret
store_instruction_with_imm8:
	mov	[immediate_size],1
	call	store_instruction
	mov	al,byte [value]
	stos	byte [edi]
	ret
store_instruction_with_imm16:
	mov	[immediate_size],2
	call	store_instruction
	mov	ax,word [value]
	call	mark_relocation
	stos	word [edi]
	ret
store_instruction_with_imm32:
	mov	[immediate_size],4
	call	store_instruction
	mov	eax,dword [value]
	call	mark_relocation
	stos	dword [edi]
	ret
