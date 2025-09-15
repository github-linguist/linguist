  bits 16
  cpu 386

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Token values as computed by the tokenizer's
;;; atoi() calculation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%define TOK_INT         6388
%define TOK_VOID        11386
%define TOK_ASM         5631
%define TOK_COMM        65532
%define TOK_SEMI        11
%define TOK_LPAREN      65528
%define TOK_RPAREN      65529
%define TOK_START       20697
%define TOK_DEREF       64653
%define TOK_WHILE_BEGIN 55810
%define TOK_IF_BEGIN    6232
%define TOK_BODY_BEGIN  5
%define TOK_BLK_BEGIN   75
%define TOK_BLK_END     77
%define TOK_ASSIGN      13
%define TOK_ADDR        65526
%define TOK_SUB         65533
%define TOK_ADD         65531
%define TOK_MUL         65530
%define TOK_AND         65526
%define TOK_OR          76
%define TOK_XOR         46
%define TOK_SHL         132
%define TOK_SHR         154
%define TOK_EQ          143
%define TOK_NE          65399
%define TOK_LT          12
%define TOK_GT          14
%define TOK_LE          133
%define TOK_GE          153

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common register uses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ax: current token / scratch register / emit val for stosw
;;; bx: current token
;;; cx: used by tok_next for trailing 2 bytes
;;; dl: flag for "tok_is_num"
;;; dh: flags for "tok_is_call", trailing "()"
;;; bp: saved token for assigned variable
;;; sp: stack pointer, we don't mess with this
;;; si: used with lodsw for table scans
;;; ds: fn symbol table segment (occasionally set to "cs" to access binary_oper_tbl)
;;; di: codegen destination offset
;;; es: codegen destination segment
;;; cs: always 0x07c0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  jmp 0x07c0:entry
entry:
  push 0x3000                   ; segment 0x3000 is used for fn symbol table
  pop  ds
  push 0x2000                   ; segment 0x2000 is used for codegen output buffer
  pop  es
  xor di,di                     ; codegen index, zero'd
  ;; [fall-through]

  ;; main loop for parsing all decls
compile:
  ;; advance to either "int" or "void"
  call tok_next

  ;; if "int" then skip a variable
  cmp ax,TOK_INT
  jne compile_function
  call tok_next2                ; consume "int" and <ident>
  jmp compile

compile_function:               ; parse and compile a function decl
  call tok_next                 ; consume "void"
  push bx                       ; save function name token
  mov [bx],di                   ; record function address in symtbl
  call compile_stmts_tok_next2  ; compile function body

  mov al,0xc3                   ; emit "ret" instruction
  stosb

  pop bx                        ; if the function is _start(), we're done
  cmp bx,TOK_START
  jne compile                   ; otherwise, loop and compile another declaration
  ;; [fall-through]

  ;; done compiling, execute the binary
execute:
  push es                       ; push the codegen segment
  push word [bx]                ; push the offset to "_start()"
  push 0x4000                   ; load new segment for variable data
  pop ds
  retf                          ; jump into it via "retf"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile statements (optionally advancing tokens beforehand)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_stmts_tok_next2:
  call tok_next
compile_stmts_tok_next:
  call tok_next
compile_stmts:
  mov ax,bx
  cmp ax,TOK_BLK_END            ; if we reach '}' then return
  je return

  test dh,dh                    ; if dh is 0, it's not a call
  je _not_call
  mov al,0xe8                   ; emit "call" instruction
  stosb

  mov ax,[bx]                   ; load function offset from symbol-table
  sub ax,di                     ; compute relative to this location: "dest - cur - 2"
  sub ax,2
  stosw                         ;  emit target

  jmp compile_stmts_tok_next2   ; loop to compile next statement

_not_call:
  cmp ax,TOK_ASM                ; check for "asm"
  jne _not_asm
  call tok_next                 ; tok_next to get literal byte
  stosb                         ; emit the literal
  jmp compile_stmts_tok_next2   ; loop to compile next statement

_not_asm:
  cmp ax,TOK_IF_BEGIN           ; check for "if"
  jne _not_if
  call _control_flow_block      ; compile control-flow block
  jmp _patch_fwd                ; patch up forward jump of if-stmt

_not_if:
  cmp ax,TOK_WHILE_BEGIN        ; check for "while"
  jne _not_while
  push di                       ; save loop start location
  call _control_flow_block      ; compile control-flow block
  jmp _patch_back               ; patch up backward and forward jumps of while-stmt

_not_while:
  call compile_assign           ; handle an assignment statement
  jmp compile_stmts             ; loop to compile next statement

_patch_back:
  mov al,0xe9                   ; emit "jmp" instruction (backwards)
  stosb
  pop ax                        ; restore loop start location
  sub ax,di                     ; compute relative to this location: "dest - cur - 2"
  sub ax,2
  stosw                         ; emit target
  ;; [fall-through]
_patch_fwd:
  mov ax,di                     ; compute relative fwd jump to this location: "dest - src"
  sub ax,si
  mov es:[si-2],ax              ; patch "src - 2"
  jmp compile_stmts_tok_next    ; loop to compile next statement

_control_flow_block:
  call compile_expr_tok_next    ; compile loop or if condition expr

  ;; emit forward jump
  mov ax,0xc085                 ; emit "test ax,ax"
  stosw
  mov ax,0x840f                 ; emit "je" instruction
  stosw
  stosw                         ; emit placeholder for target

  push di                       ; save forward patch location
  call compile_stmts_tok_next   ; compile a block of statements
  pop si                        ; restore forward patch location

return:                         ; this label gives us a way to do conditional returns
  ret                           ; (e.g. "jne return")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile assignment statement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_assign:
  cmp ax,TOK_DEREF              ; check for "*(int*)"
  jne _not_deref_store
  call tok_next                 ; consume "*(int*)"
  call save_var_and_compile_expr ; compile rhs first
  ;; [fall-through]

compile_store_deref:
  mov bx,bp                     ; restore dest var token
  mov ax,0x0489                 ; code for "mov [si],ax"
  ;; [fall-through]

emit_common_ptr_op:
  push ax
  mov ax,0x368b                 ; emit "mov si,[imm]"
  call emit_var
  pop ax
  stosw                         ; emit
  ret

_not_deref_store:
  call save_var_and_compile_expr ; compile rhs first
  ;; [fall-through]

compile_store:
  mov bx,bp                     ; restore dest var token
  mov ax,0x0689                 ; code for "mov [imm],ax"
  jmp emit_var                  ; [tail-call]

save_var_and_compile_expr:
  mov bp,bx                     ; save dest to bp
  call tok_next                 ; consume dest
  ;; [fall-through]             ; fall-through will consume "=" before compiling expr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_expr_tok_next:
  call tok_next
compile_expr:
  call compile_unary            ; compile left-hand side

  push ds                       ; need to swap out 'ds' to scan the table with lodsw
  push cs
  pop ds

  mov si,binary_oper_tbl - 2    ; load ptr to operator table (biased backwards)
_check_next:
  lodsw                         ; discard 16-bit of machine-code
  lodsw                         ; load 16-bit token value
  cmp ax,bx                     ; matches token?
  je _found
  test ax,ax                    ; end of table?
  jne _check_next

  pop ds
  ret                           ; all-done, not found

_found:
  lodsw                         ; load 16-bit of machine-code
  push ax                       ; save it to the stack
  mov al,0x50                   ; code for "push ax"
  stosb                         ; emit
  call tok_next                 ; consume operator token
  call compile_unary            ; compile right-hand side
  mov ax,0x9159                 ; code for "pop cx; xchg ax,cx"
  stosw                         ; emit

  pop bx                        ; restore 16-bit of machine-code
  cmp bh,0xc0                   ; detect the special case for comparison ops
  jne emit_op
emit_cmp_op:
  mov ax,0xc839                 ; code for "cmp ax,cx"
  stosw                         ; emit
  mov ax,0x00b8                 ; code for "mov ax,0x00"
  stosw                         ; emit
  mov ax,0x0f00                 ; code for the rest of imm and prefix for "setX" instrs
  stosw                         ; emit
  ;; [fall-through]

emit_op:
  mov ax,bx
  stosw                         ; emit machine code for op
  pop ds
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile unary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
compile_unary:
  cmp ax,TOK_DEREF              ; check for "*(int*)"
  jne _not_deref
  ;; compile deref (load)
  call tok_next                 ; consume "*(int*)"
  mov ax,0x048b                 ; code for "mov ax,[si]"
  jmp emit_common_ptr_op        ; [tail-call]

_not_deref:
  cmp ax,TOK_LPAREN             ; check for "*(int*)"
  jne _not_paren
  call compile_expr_tok_next    ; consume "(" and compile expr
  jmp tok_next                  ; [tail-call] to consume ")"

_not_paren:
  cmp ax,TOK_ADDR               ; check for "&"
  jne _not_addr
  call tok_next                 ; consume "&"
  mov ax,0x068d                 ; code for "lea ax,[imm]"
  jmp emit_var                  ; [tail-call] to emit code

_not_addr:
  test dl,dl                    ; check for tok_is_num
  je _not_int
  mov al,0xb8                   ; code for "mov ax,imm"
  stosb                         ; emit
  jmp emit_tok                  ; [tail-call] to emit imm

_not_int:
  ;; compile var
  mov ax,0x068b                 ; code for "mov ax,[imm]"
  ;; [fall-through]

emit_var:
  stosw                         ; emit
  add bx,bx                     ; bx = 2*bx (scale up for 16-bit)
  ;; [fall-through]

emit_tok:
  mov ax,bx
  stosw                         ; emit token value
  jmp tok_next                  ; [tail-call]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get next token, setting the following:
;;;   ax: token
;;;   bx: token
;;;   dl: tok_is_num
;;;   dh: tok_is_call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tok_next2:
  call tok_next
  ;; [fall-through]
tok_next:
  call getch
  cmp al,32                     ; skip spaces (anything <= ' ' is considered space)
  jle tok_next

  xor bx,bx                     ; zero token reg
  xor cx,cx                     ; zero last-two chars reg

  cmp al,57
  setle dl                      ; tok_is_num = (al <= '9')

_nextch:
  cmp al,32
  jle _done                     ; if char is space then break

  shl cx,8
  mov cl,al                     ; shift this char into cx

  imul bx,10
  sub ax,48
  add bx,ax                     ; atoi computation: bx = 10 * bx + (ax - '0')

  call getch
  jmp _nextch                   ; [loop]

_done:
  mov ax,cx
  cmp ax,0x2f2f                 ; check for single-line comment "//"
  je _comment_double_slash
  cmp ax,0x2f2a                 ; check for multi-line comment "/*"
  je _comment_multi_line
  cmp ax,0x2829                 ; check for call parens "()"
  sete dh

  mov ax,bx                     ; return token in ax also
  ret

_comment_double_slash:
  call getch                    ; get next char
  cmp al,10                     ; check for newline '\n'
  jne _comment_double_slash     ; [loop]
  jmp tok_next                  ; [tail-call]

_comment_multi_line:
  call tok_next                 ; get next token
  cmp ax,65475                  ; check for token "*/"
  jne _comment_multi_line       ; [loop]
  jmp tok_next                  ; [tail-call]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get next char: returned in ax (ah == 0, al == ch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
getch:
  push dx                       ; need to save dx because tok_next uses it for flags
  xor si,si                     ; use ds:0 as a semi-colon buffer, encodes smaller via si
  mov ax,[si]                   ; load the semi-colon buffer
  xor [si],ax                   ; zero the buffer
  cmp al,59                     ; check for ';'
  je getch_done                 ; if ';' return it

getch_tryagain:
  mov ax,0x0200
  xor dx,dx
  int 0x14                      ; get a char from serial (bios function)

  and ah,0x80                   ; check for failure and clear ah as a side-effect
  jne getch_tryagain            ; failed, try again later

  cmp al,59                     ; check for ';'
  jne getch_done                ; if not ';' return it
  mov [si],ax                   ; save the ';'
  xor ax,ax                     ; return 0 instead, treated as whitespcae

getch_done:
  pop dx
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binary operator table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
binary_oper_tbl:
  dw TOK_ADD,0xc103             ; add ax,cx
  dw TOK_SUB,0xc12b             ; sub ax,cx
  dw TOK_MUL,0xe1f7             ; mul ax,cx
  dw TOK_AND,0xc123             ; and ax,cx
  dw TOK_OR,0xc10b              ; or ax,cx
  dw TOK_XOR,0xc133             ; xor ax,cx
  dw TOK_SHL,0xe0d3             ; shl ax,cx
  dw TOK_SHR,0xf8d3             ; shr ax,cx
  dw TOK_EQ,0xc094              ; sete al
  dw TOK_NE,0xc095              ; setne al
  dw TOK_LT,0xc09c              ; setl al
  dw TOK_GT,0xc09f              ; setg al
  dw TOK_LE,0xc09e              ; setle al
  dw TOK_GE,0xc09d              ; setge al
  dw 0                          ; [sentinel]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; boot signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  times 510-($-$$) db 0
  db 0x55, 0xaa
