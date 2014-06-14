
; flat assembler core
; Copyright (c) 1999-2014, Tomasz Grysztar.
; All rights reserved.

assembler:
	xor	eax,eax
	mov	[stub_size],eax
	mov	[current_pass],ax
	mov	[resolver_flags],eax
	mov	[number_of_sections],eax
	mov	[actual_fixups_size],eax
      assembler_loop:
	mov	eax,[labels_list]
	mov	[tagged_blocks],eax
	mov	eax,[additional_memory]
	mov	[free_additional_memory],eax
	mov	eax,[additional_memory_end]
	mov	[structures_buffer],eax
	mov	esi,[source_start]
	mov	edi,[code_start]
	xor	eax,eax
	mov	dword [adjustment],eax
	mov	dword [adjustment+4],eax
	mov	[addressing_space],eax
	mov	[error_line],eax
	mov	[counter],eax
	mov	[format_flags],eax
	mov	[number_of_relocations],eax
	mov	[undefined_data_end],eax
	mov	[file_extension],eax
	mov	[next_pass_needed],al
	mov	[output_format],al
	mov	[adjustment_sign],al
	mov	[code_type],16
	call	init_addressing_space
      pass_loop:
	call	assemble_line
	jnc	pass_loop
	mov	eax,[additional_memory_end]
	cmp	eax,[structures_buffer]
	je	pass_done
	sub	eax,18h
	mov	eax,[eax+4]
	mov	[current_line],eax
	jmp	missing_end_directive
      pass_done:
	call	close_pass
	mov	eax,[labels_list]
      check_symbols:
	cmp	eax,[memory_end]
	jae	symbols_checked
	test	byte [eax+8],8
	jz	symbol_defined_ok
	mov	cx,[current_pass]
	cmp	cx,[eax+18]
	jne	symbol_defined_ok
	test	byte [eax+8],1
	jz	symbol_defined_ok
	sub	cx,[eax+16]
	cmp	cx,1
	jne	symbol_defined_ok
	and	byte [eax+8],not 1
	or	[next_pass_needed],-1
      symbol_defined_ok:
	test	byte [eax+8],10h
	jz	use_prediction_ok
	mov	cx,[current_pass]
	and	byte [eax+8],not 10h
	test	byte [eax+8],20h
	jnz	check_use_prediction
	cmp	cx,[eax+18]
	jne	use_prediction_ok
	test	byte [eax+8],8
	jz	use_prediction_ok
	jmp	use_misprediction
      check_use_prediction:
	test	byte [eax+8],8
	jz	use_misprediction
	cmp	cx,[eax+18]
	je	use_prediction_ok
      use_misprediction:
	or	[next_pass_needed],-1
      use_prediction_ok:
	test	byte [eax+8],40h
	jz	check_next_symbol
	and	byte [eax+8],not 40h
	test	byte [eax+8],4
	jnz	define_misprediction
	mov	cx,[current_pass]
	test	byte [eax+8],80h
	jnz	check_define_prediction
	cmp	cx,[eax+16]
	jne	check_next_symbol
	test	byte [eax+8],1
	jz	check_next_symbol
	jmp	define_misprediction
      check_define_prediction:
	test	byte [eax+8],1
	jz	define_misprediction
	cmp	cx,[eax+16]
	je	check_next_symbol
      define_misprediction:
	or	[next_pass_needed],-1
      check_next_symbol:
	add	eax,LABEL_STRUCTURE_SIZE
	jmp	check_symbols
      symbols_checked:
	cmp	[next_pass_needed],0
	jne	next_pass
	mov	eax,[error_line]
	or	eax,eax
	jz	assemble_ok
	mov	[current_line],eax
	cmp	[error],undefined_symbol
	jne	error_confirmed
	mov	eax,[error_info]
	or	eax,eax
	jz	error_confirmed
	test	byte [eax+8],1
	jnz	next_pass
      error_confirmed:
	call	error_handler
      error_handler:
	mov	eax,[error]
	sub	eax,error_handler
	add	[esp],eax
	ret
      next_pass:
	inc	[current_pass]
	mov	ax,[current_pass]
	cmp	ax,[passes_limit]
	je	code_cannot_be_generated
	jmp	assembler_loop
      assemble_ok:
	ret

create_addressing_space:
	mov	ebx,[addressing_space]
	test	ebx,ebx
	jz	init_addressing_space
	test	byte [ebx+0Ah],1
	jnz	illegal_instruction
	mov	eax,edi
	sub	eax,[ebx+18h]
	mov	[ebx+1Ch],eax
      init_addressing_space:
	mov	ebx,[tagged_blocks]
	mov	dword [ebx-4],10h
	mov	dword [ebx-8],20h
	sub	ebx,8+20h
	cmp	ebx,edi
	jbe	out_of_memory
	mov	[tagged_blocks],ebx
	mov	[addressing_space],ebx
	xor	eax,eax
	mov	[ebx],edi
	mov	[ebx+4],eax
	mov	[ebx+8],eax
	mov	[ebx+10h],eax
	mov	[ebx+14h],eax
	mov	[ebx+18h],edi
	mov	[ebx+1Ch],eax
	ret

assemble_line:
	mov	eax,[tagged_blocks]
	sub	eax,100h
	cmp	edi,eax
	ja	out_of_memory
	lods	byte [esi]
	cmp	al,1
	je	assemble_instruction
	jb	source_end
	cmp	al,3
	jb	define_label
	je	define_constant
	cmp	al,4
	je	label_addressing_space
	cmp	al,0Fh
	je	new_line
	cmp	al,13h
	je	code_type_setting
	cmp	al,10h
	jne	illegal_instruction
	lods	byte [esi]
	jmp	segment_prefix
      code_type_setting:
	lods	byte [esi]
	mov	[code_type],al
	jmp	instruction_assembled
      new_line:
	lods	dword [esi]
	mov	[current_line],eax
	mov	[prefixed_instruction],0
	cmp	[symbols_file],0
	je	continue_line
	cmp	[next_pass_needed],0
	jne	continue_line
	mov	ebx,[tagged_blocks]
	mov	dword [ebx-4],1
	mov	dword [ebx-8],14h
	sub	ebx,8+14h
	cmp	ebx,edi
	jbe	out_of_memory
	mov	[tagged_blocks],ebx
	mov	[ebx],eax
	mov	[ebx+4],edi
	mov	eax,[addressing_space]
	mov	[ebx+8],eax
	mov	al,[code_type]
	mov	[ebx+10h],al
      continue_line:
	cmp	byte [esi],0Fh
	je	line_assembled
	jmp	assemble_line
      define_label:
	lods	dword [esi]
	cmp	eax,0Fh
	jb	invalid_use_of_symbol
	je	reserved_word_used_as_symbol
	mov	ebx,eax
	lods	byte [esi]
	mov	[label_size],al
	call	make_label
	jmp	continue_line
      make_label:
	mov	eax,edi
	xor	edx,edx
	xor	cl,cl
	mov	ebp,[addressing_space]
	sub	eax,[ds:ebp]
	sbb	edx,[ds:ebp+4]
	sbb	cl,[ds:ebp+8]
	jp	label_value_ok
	call	recoverable_overflow
      label_value_ok:
	mov	[address_sign],cl
	test	byte [ds:ebp+0Ah],1
	jnz	make_virtual_label
	or	byte [ebx+9],1
	xchg	eax,[ebx]
	xchg	edx,[ebx+4]
	mov	ch,[ebx+9]
	shr	ch,1
	and	ch,1
	neg	ch
	sub	eax,[ebx]
	sbb	edx,[ebx+4]
	sbb	ch,cl
	mov	dword [adjustment],eax
	mov	dword [adjustment+4],edx
	mov	[adjustment_sign],ch
	or	al,ch
	or	eax,edx
	setnz	ah
	jmp	finish_label
      make_virtual_label:
	and	byte [ebx+9],not 1
	cmp	eax,[ebx]
	mov	[ebx],eax
	setne	ah
	cmp	edx,[ebx+4]
	mov	[ebx+4],edx
	setne	al
	or	ah,al
      finish_label:
	mov	ebp,[addressing_space]
	mov	ch,[ds:ebp+9]
	mov	cl,[label_size]
	mov	edx,[ds:ebp+14h]
	mov	ebp,[ds:ebp+10h]
      finish_label_symbol:
	mov	al,[address_sign]
	xor	al,[ebx+9]
	and	al,10b
	or	ah,al
	xor	[ebx+9],al
	cmp	cl,[ebx+10]
	mov	[ebx+10],cl
	setne	al
	or	ah,al
	cmp	ch,[ebx+11]
	mov	[ebx+11],ch
	setne	al
	or	ah,al
	cmp	ebp,[ebx+12]
	mov	[ebx+12],ebp
	setne	al
	or	ah,al
	or	ch,ch
	jz	label_symbol_ok
	cmp	edx,[ebx+20]
	mov	[ebx+20],edx
	setne	al
	or	ah,al
      label_symbol_ok:
	mov	cx,[current_pass]
	xchg	[ebx+16],cx
	mov	edx,[current_line]
	mov	[ebx+28],edx
	and	byte [ebx+8],not 2
	test	byte [ebx+8],1
	jz	new_label
	cmp	cx,[ebx+16]
	je	symbol_already_defined
	btr	dword [ebx+8],10
	jc	requalified_label
	inc	cx
	sub	cx,[ebx+16]
	setnz	al
	or	ah,al
	jz	label_made
	test	byte [ebx+8],8
	jz	label_made
	mov	cx,[current_pass]
	cmp	cx,[ebx+18]
	jne	label_made
      requalified_label:
	or	[next_pass_needed],-1
      label_made:
	ret
      new_label:
	or	byte [ebx+8],1
	ret
      define_constant:
	lods	dword [esi]
	inc	esi
	cmp	eax,0Fh
	jb	invalid_use_of_symbol
	je	reserved_word_used_as_symbol
	mov	edx,[eax+8]
	push	edx
	cmp	[current_pass],0
	je	get_constant_value
	test	dl,4
	jnz	get_constant_value
	mov	cx,[current_pass]
	cmp	cx,[eax+16]
	je	get_constant_value
	or	dl,4
	mov	[eax+8],dl
      get_constant_value:
	push	eax
	mov	al,byte [esi-1]
	push	eax
	or	[size_override],-1
	call	get_value
	pop	ebx
	mov	ch,bl
	pop	ebx
	pop	ecx
	test	cl,4
	jnz	constant_referencing_mode_ok
	and	byte [ebx+8],not 4
      constant_referencing_mode_ok:
	xor	cl,cl
	mov	ch,[value_type]
	cmp	ch,3
	je	invalid_use_of_symbol
      make_constant:
	and	byte [ebx+9],not 1
	cmp	eax,[ebx]
	mov	[ebx],eax
	setne	ah
	cmp	edx,[ebx+4]
	mov	[ebx+4],edx
	setne	al
	or	ah,al
	mov	al,[value_sign]
	xor	al,[ebx+9]
	and	al,10b
	or	ah,al
	xor	[ebx+9],al
	cmp	cl,[ebx+10]
	mov	[ebx+10],cl
	setne	al
	or	ah,al
	cmp	ch,[ebx+11]
	mov	[ebx+11],ch
	setne	al
	or	ah,al
	xor	edx,edx
	cmp	edx,[ebx+12]
	mov	[ebx+12],edx
	setne	al
	or	ah,al
	or	ch,ch
	jz	constant_symbol_ok
	mov	edx,[symbol_identifier]
	cmp	edx,[ebx+20]
	mov	[ebx+20],edx
	setne	al
	or	ah,al
      constant_symbol_ok:
	mov	cx,[current_pass]
	xchg	[ebx+16],cx
	mov	edx,[current_line]
	mov	[ebx+28],edx
	test	byte [ebx+8],1
	jz	new_constant
	cmp	cx,[ebx+16]
	jne	redeclare_constant
	test	byte [ebx+8],2
	jz	symbol_already_defined
	or	byte [ebx+8],4
	and	byte [ebx+9],not 4
	jmp	instruction_assembled
      redeclare_constant:
	btr	dword [ebx+8],10
	jc	requalified_constant
	inc	cx
	sub	cx,[ebx+16]
	setnz	al
	or	ah,al
	jz	instruction_assembled
	test	byte [ebx+8],4
	jnz	instruction_assembled
	test	byte [ebx+8],8
	jz	instruction_assembled
	mov	cx,[current_pass]
	cmp	cx,[ebx+18]
	jne	instruction_assembled
      requalified_constant:
	or	[next_pass_needed],-1
	jmp	instruction_assembled
      new_constant:
	or	byte [ebx+8],1+2
	jmp	instruction_assembled
      label_addressing_space:
	lods	dword [esi]
	cmp	eax,0Fh
	jb	invalid_use_of_symbol
	je	reserved_word_used_as_symbol
	mov	cx,[current_pass]
	test	byte [eax+8],1
	jz	make_addressing_space_label
	cmp	cx,[eax+16]
	je	symbol_already_defined
	test	byte [eax+9],4
	jnz	make_addressing_space_label
	or	[next_pass_needed],-1
      make_addressing_space_label:
	mov	dx,[eax+8]
	and	dx,not (2 or 100h)
	or	dx,1 or 4 or 400h
	mov	[eax+8],dx
	mov	[eax+16],cx
	mov	edx,[current_line]
	mov	[eax+28],edx
	mov	ebx,[addressing_space]
	mov	[eax],ebx
	or	byte [ebx+0Ah],2
	jmp	continue_line
      assemble_instruction:
;        mov     [operand_size],0
;        mov     [size_override],0
;        mov     [operand_prefix],0
;        mov     [opcode_prefix],0
	and	dword [operand_size],0
;        mov     [rex_prefix],0
;        mov     [vex_required],0
;        mov     [vex_register],0
;        mov     [immediate_size],0
	and	dword [rex_prefix],0
	call	instruction_handler
      instruction_handler:
	movzx	ebx,word [esi]
	mov	al,[esi+2]
	add	esi,3
	add	[esp],ebx
	ret
      instruction_assembled:
	mov	al,[esi]
	cmp	al,0Fh
	je	line_assembled
	or	al,al
	jnz	extra_characters_on_line
      line_assembled:
	clc
	ret
      source_end:
	dec	esi
	stc
	ret

org_directive:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_qword_value
	mov	cl,[value_type]
	test	cl,1
	jnz	invalid_use_of_symbol
	push	eax
	mov	ebx,[addressing_space]
	mov	eax,edi
	sub	eax,[ebx+18h]
	mov	[ebx+1Ch],eax
	test	byte [ebx+0Ah],1
	jnz	in_virtual
	call	init_addressing_space
	jmp	org_space_ok
      in_virtual:
	call	close_virtual_addressing_space
	call	init_addressing_space
	or	byte [ebx+0Ah],1
      org_space_ok:
	pop	eax
	mov	[ebx+9],cl
	mov	cl,[value_sign]
	sub	[ebx],eax
	sbb	[ebx+4],edx
	sbb	byte [ebx+8],cl
	jp	org_value_ok
	call	recoverable_overflow
      org_value_ok:
	mov	edx,[symbol_identifier]
	mov	[ebx+14h],edx
	cmp	[output_format],1
	ja	instruction_assembled
	cmp	edi,[code_start]
	jne	instruction_assembled
	cmp	eax,100h
	jne	instruction_assembled
	bts	[format_flags],0
	jmp	instruction_assembled
label_directive:
	lods	byte [esi]
	cmp	al,2
	jne	invalid_argument
	lods	dword [esi]
	cmp	eax,0Fh
	jb	invalid_use_of_symbol
	je	reserved_word_used_as_symbol
	inc	esi
	mov	ebx,eax
	mov	[label_size],0
	lods	byte [esi]
	cmp	al,':'
	je	get_label_size
	dec	esi
	cmp	al,11h
	jne	label_size_ok
      get_label_size:
	lods	word [esi]
	cmp	al,11h
	jne	invalid_argument
	mov	[label_size],ah
      label_size_ok:
	cmp	byte [esi],80h
	je	get_free_label_value
	call	make_label
	jmp	instruction_assembled
      get_free_label_value:
	inc	esi
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	push	ebx ecx
	or	byte [ebx+8],4
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_address_value
	or	bh,bh
	setnz	ch
	xchg	ch,cl
	mov	bp,cx
	shl	ebp,16
	xchg	bl,bh
	mov	bp,bx
	pop	ecx ebx
	and	byte [ebx+8],not 4
	mov	ch,[value_type]
	test	ch,1
	jnz	invalid_use_of_symbol
      make_free_label:
	and	byte [ebx+9],not 1
	cmp	eax,[ebx]
	mov	[ebx],eax
	setne	ah
	cmp	edx,[ebx+4]
	mov	[ebx+4],edx
	setne	al
	or	ah,al
	mov	edx,[address_symbol]
	mov	cl,[label_size]
	call	finish_label_symbol
	jmp	instruction_assembled
load_directive:
	lods	byte [esi]
	cmp	al,2
	jne	invalid_argument
	lods	dword [esi]
	cmp	eax,0Fh
	jb	invalid_use_of_symbol
	je	reserved_word_used_as_symbol
	inc	esi
	push	eax
	mov	al,1
	cmp	byte [esi],11h
	jne	load_size_ok
	lods	byte [esi]
	lods	byte [esi]
      load_size_ok:
	cmp	al,8
	ja	invalid_value
	mov	[operand_size],al
	and	dword [value],0
	and	dword [value+4],0
	lods	byte [esi]
	cmp	al,82h
	jne	invalid_argument
	call	get_data_point
	jc	value_loaded
	push	esi edi
	mov	esi,ebx
	mov	edi,value
	rep	movs byte [edi],[esi]
	pop	edi esi
      value_loaded:
	mov	[value_sign],0
	mov	eax,dword [value]
	mov	edx,dword [value+4]
	pop	ebx
	xor	cx,cx
	jmp	make_constant
      get_data_point:
	mov	ebx,[addressing_space]
	mov	ecx,edi
	sub	ecx,[ebx+18h]
	mov	[ebx+1Ch],ecx
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],11h
	jne	get_data_address
	cmp	word [esi+1+4],'):'
	jne	get_data_address
	inc	esi
	lods	dword [esi]
	add	esi,2
	cmp	byte [esi],'('
	jne	invalid_argument
	inc	esi
	cmp	eax,0Fh
	jbe	reserved_word_used_as_symbol
	mov	edx,undefined_symbol
	test	byte [eax+8],1
	jz	addressing_space_unavailable
	mov	edx,symbol_out_of_scope
	mov	cx,[eax+16]
	cmp	cx,[current_pass]
	jne	addressing_space_unavailable
	test	byte [eax+9],4
	jz	invalid_use_of_symbol
	mov	ebx,eax
	mov	ax,[current_pass]
	mov	[ebx+18],ax
	or	byte [ebx+8],8
	cmp	[symbols_file],0
	je	get_addressing_space
	cmp	[next_pass_needed],0
	jne	get_addressing_space
	call	store_label_reference
      get_addressing_space:
	mov	ebx,[ebx]
      get_data_address:
	push	ebx
	cmp	byte [esi],'.'
	je	invalid_value
	or	[size_override],-1
	call	get_address_value
	pop	ebp
	call	calculate_relative_offset
	cmp	[next_pass_needed],0
	jne	data_address_type_ok
	cmp	[value_type],0
	jne	invalid_use_of_symbol
      data_address_type_ok:
	mov	ebx,edi
	xor	ecx,ecx
	add	ebx,eax
	adc	edx,ecx
	mov	eax,ebx
	sub	eax,[ds:ebp+18h]
	sbb	edx,ecx
	jnz	bad_data_address
	mov	cl,[operand_size]
	add	eax,ecx
	cmp	eax,[ds:ebp+1Ch]
	ja	bad_data_address
	clc
	ret
      addressing_space_unavailable:
	cmp	[error_line],0
	jne	get_data_address
	push	[current_line]
	pop	[error_line]
	mov	[error],edx
	mov	[error_info],eax
	jmp	get_data_address
      bad_data_address:
	call	recoverable_overflow
	stc
	ret
store_directive:
	cmp	byte [esi],11h
	je	sized_store
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	call	get_byte_value
	xor	edx,edx
	movzx	eax,al
	mov	[operand_size],1
	jmp	store_value_ok
      sized_store:
	or	[size_override],-1
	call	get_value
      store_value_ok:
	cmp	[value_type],0
	jne	invalid_use_of_symbol
	mov	dword [value],eax
	mov	dword [value+4],edx
	lods	byte [esi]
	cmp	al,80h
	jne	invalid_argument
	call	get_data_point
	jc	instruction_assembled
	push	esi edi
	mov	esi,value
	mov	edi,ebx
	rep	movs byte [edi],[esi]
	mov	eax,edi
	pop	edi esi
	cmp	ebx,[undefined_data_end]
	jae	instruction_assembled
	cmp	eax,[undefined_data_start]
	jbe	instruction_assembled
	mov	[undefined_data_start],eax
	jmp	instruction_assembled

display_directive:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],0
	jne	display_byte
	inc	esi
	lods	dword [esi]
	mov	ecx,eax
	push	edi
	mov	edi,[tagged_blocks]
	sub	edi,8
	sub	edi,eax
	cmp	edi,[esp]
	jbe	out_of_memory
	mov	[tagged_blocks],edi
	rep	movs byte [edi],[esi]
	stos	dword [edi]
	xor	eax,eax
	stos	dword [edi]
	pop	edi
	inc	esi
	jmp	display_next
      display_byte:
	call	get_byte_value
	push	edi
	mov	edi,[tagged_blocks]
	sub	edi,8+1
	mov	[tagged_blocks],edi
	stos	byte [edi]
	mov	eax,1
	stos	dword [edi]
	dec	eax
	stos	dword [edi]
	pop	edi
      display_next:
	cmp	edi,[tagged_blocks]
	ja	out_of_memory
	lods	byte [esi]
	cmp	al,','
	je	display_directive
	dec	esi
	jmp	instruction_assembled
show_display_buffer:
	mov	eax,[tagged_blocks]
	or	eax,eax
	jz	display_done
	mov	esi,[labels_list]
	cmp	esi,eax
	je	display_done
      display_messages:
	sub	esi,8
	mov	eax,[esi+4]
	mov	ecx,[esi]
	sub	esi,ecx
	test	eax,eax
	jnz	skip_block
	push	esi
	call	display_block
	pop	esi
      skip_block:
	cmp	esi,[tagged_blocks]
	jne	display_messages
      display_done:
	ret

times_directive:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	cmp	eax,0
	je	zero_times
	cmp	byte [esi],':'
	jne	times_argument_ok
	inc	esi
      times_argument_ok:
	push	[counter]
	push	[counter_limit]
	mov	[counter_limit],eax
	mov	[counter],1
      times_loop:
	mov	eax,esp
	sub	eax,100h
	jc	stack_overflow
	cmp	eax,[stack_limit]
	jb	stack_overflow
	push	esi
	or	[prefixed_instruction],-1
	call	continue_line
	mov	eax,[counter_limit]
	cmp	[counter],eax
	je	times_done
	inc	[counter]
	pop	esi
	jmp	times_loop
      times_done:
	pop	eax
	pop	[counter_limit]
	pop	[counter]
	jmp	instruction_assembled
      zero_times:
	call	skip_symbol
	jnc	zero_times
	jmp	instruction_assembled

virtual_directive:
	lods	byte [esi]
	cmp	al,80h
	jne	virtual_at_current
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_address_value
	mov	ebp,[address_symbol]
	or	bh,bh
	setnz	ch
	jmp	set_virtual
      virtual_at_current:
	dec	esi
	mov	ebp,[addressing_space]
	mov	al,[ds:ebp+9]
	mov	[value_type],al
	mov	eax,edi
	xor	edx,edx
	xor	cl,cl
	sub	eax,[ds:ebp]
	sbb	edx,[ds:ebp+4]
	sbb	cl,[ds:ebp+8]
	mov	[address_sign],cl
	mov	bx,[ds:ebp+10h]
	mov	cx,[ds:ebp+10h+2]
	xchg	bh,bl
	xchg	ch,cl
	mov	ebp,[ds:ebp+14h]
      set_virtual:
	xchg	bl,bh
	xchg	cl,ch
	shl	ecx,16
	mov	cx,bx
	push	ecx eax
	call	allocate_structure_data
	mov	word [ebx],virtual_directive-instruction_handler
	mov	ecx,[addressing_space]
	mov	[ebx+12],ecx
	mov	[ebx+8],edi
	mov	ecx,[current_line]
	mov	[ebx+4],ecx
	mov	ebx,[addressing_space]
	mov	eax,edi
	sub	eax,[ebx+18h]
	mov	[ebx+1Ch],eax
	call	init_addressing_space
	or	byte [ebx+0Ah],1
	pop	eax
	mov	cl,[address_sign]
	not	eax
	not	edx
	not	cl
	add	eax,1
	adc	edx,0
	adc	cl,0
	add	eax,edi
	adc	edx,0
	adc	cl,0
	mov	[ebx],eax
	mov	[ebx+4],edx
	mov	[ebx+8],cl
	pop	dword [ebx+10h]
	mov	[ebx+14h],ebp
	mov	al,[value_type]
	test	al,1
	jnz	invalid_use_of_symbol
	mov	[ebx+9],al
	jmp	instruction_assembled
      allocate_structure_data:
	mov	ebx,[structures_buffer]
	sub	ebx,18h
	cmp	ebx,[free_additional_memory]
	jb	out_of_memory
	mov	[structures_buffer],ebx
	ret
      find_structure_data:
	mov	ebx,[structures_buffer]
      scan_structures:
	cmp	ebx,[additional_memory_end]
	je	no_such_structure
	cmp	ax,[ebx]
	je	structure_data_found
	add	ebx,18h
	jmp	scan_structures
      structure_data_found:
	ret
      no_such_structure:
	stc
	ret
      end_virtual:
	call	find_structure_data
	jc	unexpected_instruction
	push	ebx
	call	close_virtual_addressing_space
	pop	ebx
	mov	eax,[ebx+12]
	mov	[addressing_space],eax
	mov	edi,[ebx+8]
      remove_structure_data:
	push	esi edi
	mov	ecx,ebx
	sub	ecx,[structures_buffer]
	shr	ecx,2
	lea	esi,[ebx-4]
	lea	edi,[esi+18h]
	std
	rep	movs dword [edi],[esi]
	cld
	add	[structures_buffer],18h
	pop	edi esi
	ret
      close_virtual_addressing_space:
	mov	ebx,[addressing_space]
	mov	eax,edi
	sub	eax,[ebx+18h]
	mov	[ebx+1Ch],eax
	test	byte [ebx+0Ah],2
	jz	addressing_space_closed
	push	esi edi ecx edx
	mov	ecx,eax
	mov	eax,[tagged_blocks]
	mov	dword [eax-4],11h
	mov	dword [eax-8],ecx
	sub	eax,8
	sub	eax,ecx
	mov	[tagged_blocks],eax
	lea	edi,[eax+ecx-1]
	xchg	eax,[ebx+18h]
	lea	esi,[eax+ecx-1]
	mov	eax,edi
	sub	eax,esi
	std
	shr	ecx,1
	jnc	virtual_byte_ok
	movs	byte [edi],[esi]
      virtual_byte_ok:
	dec	esi
	dec	edi
	shr	ecx,1
	jnc	virtual_word_ok
	movs	word [edi],[esi]
      virtual_word_ok:
	sub	esi,2
	sub	edi,2
	rep	movs dword [edi],[esi]
	cld
	xor	edx,edx
	add	[ebx],eax
	adc	dword [ebx+4],edx
	adc	byte [ebx+8],dl
	pop	edx ecx edi esi
      addressing_space_closed:
	ret
repeat_directive:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	cmp	eax,0
	je	zero_repeat
	call	allocate_structure_data
	mov	word [ebx],repeat_directive-instruction_handler
	xchg	eax,[counter_limit]
	mov	[ebx+10h],eax
	mov	eax,1
	xchg	eax,[counter]
	mov	[ebx+14h],eax
	mov	[ebx+8],esi
	mov	eax,[current_line]
	mov	[ebx+4],eax
	jmp	instruction_assembled
      end_repeat:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	call	find_structure_data
	jc	unexpected_instruction
	mov	eax,[counter_limit]
	inc	[counter]
	cmp	[counter],eax
	jbe	continue_repeating
      stop_repeat:
	mov	eax,[ebx+10h]
	mov	[counter_limit],eax
	mov	eax,[ebx+14h]
	mov	[counter],eax
	call	remove_structure_data
	jmp	instruction_assembled
      continue_repeating:
	mov	esi,[ebx+8]
	jmp	instruction_assembled
      zero_repeat:
	mov	al,[esi]
	or	al,al
	jz	missing_end_directive
	cmp	al,0Fh
	jne	extra_characters_on_line
	call	find_end_repeat
	jmp	instruction_assembled
      find_end_repeat:
	call	find_structure_end
	cmp	ax,repeat_directive-instruction_handler
	jne	unexpected_instruction
	ret
while_directive:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	call	allocate_structure_data
	mov	word [ebx],while_directive-instruction_handler
	mov	eax,1
	xchg	eax,[counter]
	mov	[ebx+10h],eax
	mov	[ebx+8],esi
	mov	eax,[current_line]
	mov	[ebx+4],eax
      do_while:
	push	ebx
	call	calculate_logical_expression
	or	al,al
	jnz	while_true
	mov	al,[esi]
	or	al,al
	jz	missing_end_directive
	cmp	al,0Fh
	jne	extra_characters_on_line
      stop_while:
	call	find_end_while
	pop	ebx
	mov	eax,[ebx+10h]
	mov	[counter],eax
	call	remove_structure_data
	jmp	instruction_assembled
      while_true:
	pop	ebx
	jmp	instruction_assembled
      end_while:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	call	find_structure_data
	jc	unexpected_instruction
	mov	eax,[ebx+4]
	mov	[current_line],eax
	inc	[counter]
	jz	too_many_repeats
	mov	esi,[ebx+8]
	jmp	do_while
      find_end_while:
	call	find_structure_end
	cmp	ax,while_directive-instruction_handler
	jne	unexpected_instruction
	ret
if_directive:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	call	calculate_logical_expression
	mov	dl,al
	mov	al,[esi]
	or	al,al
	jz	missing_end_directive
	cmp	al,0Fh
	jne	extra_characters_on_line
	or	dl,dl
	jnz	if_true
	call	find_else
	jc	instruction_assembled
	mov	al,[esi]
	cmp	al,1
	jne	else_true
	cmp	word [esi+1],if_directive-instruction_handler
	jne	else_true
	add	esi,4
	jmp	if_directive
      if_true:
	xor	al,al
      make_if_structure:
	call	allocate_structure_data
	mov	word [ebx],if_directive-instruction_handler
	mov	byte [ebx+2],al
	mov	eax,[current_line]
	mov	[ebx+4],eax
	jmp	instruction_assembled
      else_true:
	or	al,al
	jz	missing_end_directive
	cmp	al,0Fh
	jne	extra_characters_on_line
	or	al,-1
	jmp	make_if_structure
      else_directive:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	mov	ax,if_directive-instruction_handler
	call	find_structure_data
	jc	unexpected_instruction
	cmp	byte [ebx+2],0
	jne	unexpected_instruction
      found_else:
	mov	al,[esi]
	cmp	al,1
	jne	skip_else
	cmp	word [esi+1],if_directive-instruction_handler
	jne	skip_else
	add	esi,4
	call	find_else
	jnc	found_else
	call	remove_structure_data
	jmp	instruction_assembled
      skip_else:
	or	al,al
	jz	missing_end_directive
	cmp	al,0Fh
	jne	extra_characters_on_line
	call	find_end_if
	call	remove_structure_data
	jmp	instruction_assembled
      end_if:
	cmp	[prefixed_instruction],0
	jne	unexpected_instruction
	call	find_structure_data
	jc	unexpected_instruction
	call	remove_structure_data
	jmp	instruction_assembled
      find_else:
	call	find_structure_end
	cmp	ax,else_directive-instruction_handler
	je	else_found
	cmp	ax,if_directive-instruction_handler
	jne	unexpected_instruction
	stc
	ret
      else_found:
	clc
	ret
      find_end_if:
	call	find_structure_end
	cmp	ax,if_directive-instruction_handler
	jne	unexpected_instruction
	ret
      find_structure_end:
	push	[error_line]
	mov	eax,[current_line]
	mov	[error_line],eax
      find_end_directive:
	call	skip_symbol
	jnc	find_end_directive
	lods	byte [esi]
	cmp	al,0Fh
	jne	no_end_directive
	lods	dword [esi]
	mov	[current_line],eax
      skip_labels:
	cmp	byte [esi],2
	jne	labels_ok
	add	esi,6
	jmp	skip_labels
      labels_ok:
	cmp	byte [esi],1
	jne	find_end_directive
	mov	ax,[esi+1]
	cmp	ax,prefix_instruction-instruction_handler
	je	find_end_directive
	add	esi,4
	cmp	ax,repeat_directive-instruction_handler
	je	skip_repeat
	cmp	ax,while_directive-instruction_handler
	je	skip_while
	cmp	ax,if_directive-instruction_handler
	je	skip_if
	cmp	ax,else_directive-instruction_handler
	je	structure_end
	cmp	ax,end_directive-instruction_handler
	jne	find_end_directive
	cmp	byte [esi],1
	jne	find_end_directive
	mov	ax,[esi+1]
	add	esi,4
	cmp	ax,repeat_directive-instruction_handler
	je	structure_end
	cmp	ax,while_directive-instruction_handler
	je	structure_end
	cmp	ax,if_directive-instruction_handler
	jne	find_end_directive
      structure_end:
	pop	[error_line]
	ret
      no_end_directive:
	mov	eax,[error_line]
	mov	[current_line],eax
	jmp	missing_end_directive
      skip_repeat:
	call	find_end_repeat
	jmp	find_end_directive
      skip_while:
	call	find_end_while
	jmp	find_end_directive
      skip_if:
	call	skip_if_block
	jmp	find_end_directive
      skip_if_block:
	call	find_else
	jc	if_block_skipped
	cmp	byte [esi],1
	jne	skip_after_else
	cmp	word [esi+1],if_directive-instruction_handler
	jne	skip_after_else
	add	esi,4
	jmp	skip_if_block
      skip_after_else:
	call	find_end_if
      if_block_skipped:
	ret
end_directive:
	lods	byte [esi]
	cmp	al,1
	jne	invalid_argument
	lods	word [esi]
	inc	esi
	cmp	ax,virtual_directive-instruction_handler
	je	end_virtual
	cmp	ax,repeat_directive-instruction_handler
	je	end_repeat
	cmp	ax,while_directive-instruction_handler
	je	end_while
	cmp	ax,if_directive-instruction_handler
	je	end_if
	cmp	ax,data_directive-instruction_handler
	je	end_data
	jmp	invalid_argument
break_directive:
	mov	ebx,[structures_buffer]
	mov	al,[esi]
	or	al,al
	jz	find_breakable_structure
	cmp	al,0Fh
	jne	extra_characters_on_line
      find_breakable_structure:
	cmp	ebx,[additional_memory_end]
	je	unexpected_instruction
	mov	ax,[ebx]
	cmp	ax,repeat_directive-instruction_handler
	je	break_repeat
	cmp	ax,while_directive-instruction_handler
	je	break_while
	cmp	ax,if_directive-instruction_handler
	je	break_if
	add	ebx,18h
	jmp	find_breakable_structure
      break_if:
	push	[current_line]
	mov	eax,[ebx+4]
	mov	[current_line],eax
	call	remove_structure_data
	call	skip_if_block
	pop	[current_line]
	mov	ebx,[structures_buffer]
	jmp	find_breakable_structure
      break_repeat:
	push	ebx
	call	find_end_repeat
	pop	ebx
	jmp	stop_repeat
      break_while:
	push	ebx
	jmp	stop_while

data_bytes:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_byte
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	mov	byte [edi],0
	inc	edi
	jmp	undefined_data
      get_byte:
	cmp	byte [esi],0
	je	get_string
	call	get_byte_value
	stos	byte [edi]
	ret
      get_string:
	inc	esi
	lods	dword [esi]
	mov	ecx,eax
	lea	eax,[edi+ecx]
	cmp	eax,[tagged_blocks]
	ja	out_of_memory
	rep	movs byte [edi],[esi]
	inc	esi
	ret
      undefined_data:
	mov	ebp,[addressing_space]
	test	byte [ds:ebp+0Ah],1
	jz	mark_undefined_data
	ret
      mark_undefined_data:
	cmp	eax,[undefined_data_end]
	je	undefined_data_ok
	mov	[undefined_data_start],eax
      undefined_data_ok:
	mov	[undefined_data_end],edi
	ret
      define_data:
	cmp	edi,[tagged_blocks]
	jae	out_of_memory
	cmp	byte [esi],'('
	jne	simple_data_value
	mov	ebx,esi
	inc	esi
	call	skip_expression
	xchg	esi,ebx
	cmp	byte [ebx],81h
	jne	simple_data_value
	inc	esi
	call	get_count_value
	inc	esi
	or	eax,eax
	jz	duplicate_zero_times
	cmp	byte [esi],'{'
	jne	duplicate_single_data_value
	inc	esi
      duplicate_data:
	push	eax esi
      duplicated_values:
	cmp	edi,[tagged_blocks]
	jae	out_of_memory
	call	near dword [esp+8]
	lods	byte [esi]
	cmp	al,','
	je	duplicated_values
	cmp	al,'}'
	jne	invalid_argument
	pop	ebx eax
	dec	eax
	jz	data_defined
	mov	esi,ebx
	jmp	duplicate_data
      duplicate_single_data_value:
	cmp	edi,[tagged_blocks]
	jae	out_of_memory
	push	eax esi
	call	near dword [esp+8]
	pop	ebx eax
	dec	eax
	jz	data_defined
	mov	esi,ebx
	jmp	duplicate_single_data_value
      duplicate_zero_times:
	cmp	byte [esi],'{'
	jne	skip_single_data_value
	inc	esi
      skip_data_value:
	call	skip_symbol
	jc	invalid_argument
	cmp	byte [esi],'}'
	jne	skip_data_value
	inc	esi
	jmp	data_defined
      skip_single_data_value:
	call	skip_symbol
	jmp	data_defined
      simple_data_value:
	cmp	edi,[tagged_blocks]
	jae	out_of_memory
	call	near dword [esp]
      data_defined:
	lods	byte [esi]
	cmp	al,','
	je	define_data
	dec	esi
	add	esp,4
	jmp	instruction_assembled
data_unicode:
	or	[base_code],-1
	jmp	define_words
data_words:
	mov	[base_code],0
      define_words:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_word
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	and	word [edi],0
	scas	word [edi]
	jmp	undefined_data
	ret
      get_word:
	cmp	[base_code],0
	je	word_data_value
	cmp	byte [esi],0
	je	word_string
      word_data_value:
	call	get_word_value
	call	mark_relocation
	stos	word [edi]
	ret
      word_string:
	inc	esi
	lods	dword [esi]
	mov	ecx,eax
	jecxz	word_string_ok
	lea	eax,[edi+ecx*2]
	cmp	eax,[tagged_blocks]
	ja	out_of_memory
	xor	ah,ah
      copy_word_string:
	lods	byte [esi]
	stos	word [edi]
	loop	copy_word_string
      word_string_ok:
	inc	esi
	ret
data_dwords:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_dword
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	and	dword [edi],0
	scas	dword [edi]
	jmp	undefined_data
      get_dword:
	push	esi
	call	get_dword_value
	pop	ebx
	cmp	byte [esi],':'
	je	complex_dword
	call	mark_relocation
	stos	dword [edi]
	ret
      complex_dword:
	mov	esi,ebx
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_word_value
	push	eax
	inc	esi
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_operand
	mov	al,[value_type]
	push	eax
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_word_value
	call	mark_relocation
	stos	word [edi]
	pop	eax
	mov	[value_type],al
	pop	eax
	call	mark_relocation
	stos	word [edi]
	ret
data_pwords:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_pword
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	and	dword [edi],0
	scas	dword [edi]
	and	word [edi],0
	scas	word [edi]
	jmp	undefined_data
      get_pword:
	push	esi
	call	get_pword_value
	pop	ebx
	cmp	byte [esi],':'
	je	complex_pword
	call	mark_relocation
	stos	dword [edi]
	mov	ax,dx
	stos	word [edi]
	ret
      complex_pword:
	mov	esi,ebx
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_word_value
	push	eax
	inc	esi
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_operand
	mov	al,[value_type]
	push	eax
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_dword_value
	call	mark_relocation
	stos	dword [edi]
	pop	eax
	mov	[value_type],al
	pop	eax
	call	mark_relocation
	stos	word [edi]
	ret
data_qwords:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_qword
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	and	dword [edi],0
	scas	dword [edi]
	and	dword [edi],0
	scas	dword [edi]
	jmp	undefined_data
      get_qword:
	call	get_qword_value
	call	mark_relocation
	stos	dword [edi]
	mov	eax,edx
	stos	dword [edi]
	ret
data_twords:
	call	define_data
	lods	byte [esi]
	cmp	al,'('
	je	get_tword
	cmp	al,'?'
	jne	invalid_argument
	mov	eax,edi
	and	dword [edi],0
	scas	dword [edi]
	and	dword [edi],0
	scas	dword [edi]
	and	word [edi],0
	scas	word [edi]
	jmp	undefined_data
      get_tword:
	cmp	byte [esi],'.'
	jne	complex_tword
	inc	esi
	cmp	word [esi+8],8000h
	je	fp_zero_tword
	mov	eax,[esi]
	stos	dword [edi]
	mov	eax,[esi+4]
	stos	dword [edi]
	mov	ax,[esi+8]
	add	ax,3FFFh
	jo	value_out_of_range
	cmp	ax,7FFFh
	jge	value_out_of_range
	cmp	ax,0
	jg	tword_exp_ok
	mov	cx,ax
	neg	cx
	inc	cx
	cmp	cx,64
	jae	value_out_of_range
	cmp	cx,32
	ja	large_shift
	mov	eax,[esi]
	mov	edx,[esi+4]
	mov	ebx,edx
	shr	edx,cl
	shrd	eax,ebx,cl
	jmp	tword_mantissa_shift_done
      large_shift:
	sub	cx,32
	xor	edx,edx
	mov	eax,[esi+4]
	shr	eax,cl
      tword_mantissa_shift_done:
	jnc	store_shifted_mantissa
	add	eax,1
	adc	edx,0
      store_shifted_mantissa:
	mov	[edi-8],eax
	mov	[edi-4],edx
	xor	ax,ax
	test	edx,1 shl 31
	jz	tword_exp_ok
	inc	ax
      tword_exp_ok:
	mov	bl,[esi+11]
	shl	bx,15
	or	ax,bx
	stos	word [edi]
	add	esi,13
	ret
      fp_zero_tword:
	xor	eax,eax
	stos	dword [edi]
	stos	dword [edi]
	mov	al,[esi+11]
	shl	ax,15
	stos	word [edi]
	add	esi,13
	ret
      complex_tword:
	call	get_word_value
	push	eax
	cmp	byte [esi],':'
	jne	invalid_operand
	inc	esi
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_operand
	mov	al,[value_type]
	push	eax
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_qword_value
	call	mark_relocation
	stos	dword [edi]
	mov	eax,edx
	stos	dword [edi]
	pop	eax
	mov	[value_type],al
	pop	eax
	call	mark_relocation
	stos	word [edi]
	ret
data_file:
	lods	word [esi]
	cmp	ax,'('
	jne	invalid_argument
	add	esi,4
	call	open_binary_file
	mov	eax,[esi-4]
	lea	esi,[esi+eax+1]
	mov	al,2
	xor	edx,edx
	call	lseek
	push	eax
	xor	edx,edx
	cmp	byte [esi],':'
	jne	position_ok
	inc	esi
	cmp	byte [esi],'('
	jne	invalid_argument
	inc	esi
	cmp	byte [esi],'.'
	je	invalid_value
	push	ebx
	call	get_count_value
	pop	ebx
	mov	edx,eax
	sub	[esp],edx
	jc	value_out_of_range
      position_ok:
	cmp	byte [esi],','
	jne	size_ok
	inc	esi
	cmp	byte [esi],'('
	jne	invalid_argument
	inc	esi
	cmp	byte [esi],'.'
	je	invalid_value
	push	ebx edx
	call	get_count_value
	pop	edx ebx
	cmp	eax,[esp]
	ja	value_out_of_range
	mov	[esp],eax
      size_ok:
	xor	al,al
	call	lseek
	pop	ecx
	mov	edx,edi
	add	edi,ecx
	jc	out_of_memory
	cmp	edi,[tagged_blocks]
	ja	out_of_memory
	call	read
	jc	error_reading_file
	call	close
	lods	byte [esi]
	cmp	al,','
	je	data_file
	dec	esi
	jmp	instruction_assembled
      open_binary_file:
	push	esi
	push	edi
	mov	eax,[current_line]
      find_current_source_path: 
	mov	esi,[eax] 
	test	byte [eax+7],80h 
	jz	get_current_path 
	mov	eax,[eax+8]
	jmp	find_current_source_path
      get_current_path:
	lodsb
	stosb
	or	al,al
	jnz	get_current_path
      cut_current_path:
	cmp	edi,[esp]
	je	current_path_ok
	cmp	byte [edi-1],'\'
	je	current_path_ok
	cmp	byte [edi-1],'/'
	je	current_path_ok
	dec	edi
	jmp	cut_current_path
      current_path_ok:
	mov	esi,[esp+4]
	call	expand_path
	pop	edx
	mov	esi,edx
	call	open
	jnc	file_opened
	mov	edx,[include_paths]
      search_in_include_paths:
	push	edx esi
	mov	edi,esi
	mov	esi,[esp+4]
	call	get_include_directory
	mov	[esp+4],esi
	mov	esi,[esp+8]
	call	expand_path
	pop	edx
	mov	esi,edx
	call	open
	pop	edx
	jnc	file_opened
	cmp	byte [edx],0
	jne	search_in_include_paths
	mov	edi,esi
	mov	esi,[esp]
	push	edi
	call	expand_path
	pop	edx
	mov	esi,edx
	call	open
	jc	file_not_found
      file_opened:
	mov	edi,esi
	pop	esi
	ret
reserve_bytes:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	mov	edx,ecx
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_bytes
	add	edi,ecx
	jmp	reserved_data
      zero_bytes:
	xor	eax,eax
	shr	ecx,1
	jnc	bytes_stosb_ok
	stos	byte [edi]
      bytes_stosb_ok:
	shr	ecx,1
	jnc	bytes_stosw_ok
	stos	word [edi]
      bytes_stosw_ok:
	rep	stos dword [edi]
      reserved_data:
	pop	eax
	call	undefined_data
	jmp	instruction_assembled
reserve_words:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	mov	edx,ecx
	shl	edx,1
	jc	out_of_memory
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_words
	lea	edi,[edi+ecx*2]
	jmp	reserved_data
      zero_words:
	xor	eax,eax
	shr	ecx,1
	jnc	words_stosw_ok
	stos	word [edi]
      words_stosw_ok:
	rep	stos dword [edi]
	jmp	reserved_data
reserve_dwords:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	mov	edx,ecx
	shl	edx,1
	jc	out_of_memory
	shl	edx,1
	jc	out_of_memory
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_dwords
	lea	edi,[edi+ecx*4]
	jmp	reserved_data
      zero_dwords:
	xor	eax,eax
	rep	stos dword [edi]
	jmp	reserved_data
reserve_pwords:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	shl	ecx,1
	jc	out_of_memory
	add	ecx,eax
	mov	edx,ecx
	shl	edx,1
	jc	out_of_memory
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_words
	lea	edi,[edi+ecx*2]
	jmp	reserved_data
reserve_qwords:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	shl	ecx,1
	jc	out_of_memory
	mov	edx,ecx
	shl	edx,1
	jc	out_of_memory
	shl	edx,1
	jc	out_of_memory
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_dwords
	lea	edi,[edi+ecx*4]
	jmp	reserved_data
reserve_twords:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	ecx,eax
	shl	ecx,2
	jc	out_of_memory
	add	ecx,eax
	mov	edx,ecx
	shl	edx,1
	jc	out_of_memory
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	zero_words
	lea	edi,[edi+ecx*2]
	jmp	reserved_data
align_directive:
	lods	byte [esi]
	cmp	al,'('
	jne	invalid_argument
	cmp	byte [esi],'.'
	je	invalid_value
	call	get_count_value
	mov	edx,eax
	dec	edx
	test	eax,edx
	jnz	invalid_align_value
	or	eax,eax
	jz	invalid_align_value
	cmp	eax,1
	je	instruction_assembled
	mov	ecx,edi
	mov	ebp,[addressing_space]
	sub	ecx,[ds:ebp]
	cmp	dword [ds:ebp+10h],0
	jne	section_not_aligned_enough
	cmp	byte [ds:ebp+9],0
	je	make_alignment
	cmp	[output_format],3
	je	pe_alignment
	mov	ebx,[ds:ebp+14h]
	cmp	byte [ebx],0
	jne	section_not_aligned_enough
	cmp	eax,[ebx+10h]
	jbe	make_alignment
	jmp	section_not_aligned_enough
      pe_alignment:
	cmp	eax,1000h
	ja	section_not_aligned_enough
      make_alignment:
	dec	eax
	and	ecx,eax
	jz	instruction_assembled
	neg	ecx
	add	ecx,eax
	inc	ecx
	mov	edx,ecx
	add	edx,edi
	jc	out_of_memory
	cmp	edx,[tagged_blocks]
	ja	out_of_memory
	push	edi
	cmp	[next_pass_needed],0
	je	nops
	add	edi,ecx
	jmp	reserved_data
      invalid_align_value:
	cmp	[error_line],0
	jne	instruction_assembled
	mov	eax,[current_line]
	mov	[error_line],eax
	mov	[error],invalid_value
	jmp	instruction_assembled
      nops:
	mov	eax,90909090h
	shr	ecx,1
	jnc	nops_stosb_ok
	stos	byte [edi]
      nops_stosb_ok:
	shr	ecx,1
	jnc	nops_stosw_ok
	stos	word [edi]
      nops_stosw_ok:
	rep	stos dword [edi]
	jmp	reserved_data
err_directive:
	mov	al,[esi]
	cmp	al,0Fh
	je	invoked_error
	or	al,al
	jz	invoked_error
	jmp	extra_characters_on_line
assert_directive:
	call	calculate_logical_expression
	or	al,al
	jnz	instruction_assembled
	cmp	[error_line],0
	jne	instruction_assembled
	mov	eax,[current_line]
	mov	[error_line],eax
	mov	[error],assertion_failed
	jmp	instruction_assembled
