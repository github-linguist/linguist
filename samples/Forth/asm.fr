\ Copyright 2013-2014 Lars Brinkhoff

\ Assembler for x86.

\ Adds to FORTH vocabulary: ASSEMBLER CODE ;CODE.
\ Creates ASSEMBLER vocabulary with: END-CODE and x86 opcodes.

\ Conventional prefix syntax: "<source> <destination> <opcode>,".
\ Addressing modes:
\ - immediate: "n #"
\ - direct: n
\ - register: <reg>
\ - indirect: "<reg> )"
\ - indirect with displacement: "n <reg> )#"
\ - indexed: not supported yet

require lib/common.fth
require search.fth

vocabulary assembler
also assembler definitions

\ Access to the target image.
' header,   defer header,  is header,
' cell   defer cell   is cell
' dp   defer dp   is dp
0 value delta

: aligned   cell + 1 - cell negate nand invert ;
: align   dp @ aligned dp ! ;
: allot   dp +! ;
: here   dp @ ;
: cells   cell * ;
: c!   delta + c! ;
: c,   here c!  1 allot ;
: h,   dup c,  8 rshift c, ;
: ,   dup h,  16 rshift h, ;

base @  hex

\ This constant signals that an operand is not a direct address.
deadbeef constant -addr

\ Assembler state.
variable opcode
variable d
variable s
variable dir?
variable mrrm   defer ?mrrm,
variable sib    defer ?sib,
variable disp   defer ?disp,
variable imm    defer ?imm,
defer imm,
defer immediate-opcode
defer reg
defer ?opsize

\ Set opcode.  And destination: register or memory.
: opcode!   3@ is immediate-opcode >r opcode ! ;
: !reg   dir? @ if 2 d ! then dir? off ;
: !mem   dir? off ;

\ Set bits in mod/reg/rm byte.
: -mrrm   ['] nop is ?mrrm, ;
: mod!   mrrm c0 !bits ;
: reg@   mrrm 38 @bits ;
: reg!   mrrm 38 !bits ;
: rm@   mrrm 7 @bits ;
: rm!   rm@ 3 lshift reg!  mrrm 7 !bits ;
: reg>opcode   rm@ opcode 07 !bits ;
: opcode>reg   opcode @ dup 3 rshift rm!  8 rshift opcode ! ;

\ Write parts of instruction to memory.
: ds   d @ s @ + ;
: ?twobyte   dup FF > if dup 8 rshift c, then ;
: opcode,   opcode @ ?twobyte ds + c, ;
: mrrm,   mrrm @ c, ;
: sib,   sib @ c, ;
: imm8,   imm @ c, ;
: imm16,   imm @ h, ;
: imm32,   imm @ , ;
: disp8,   disp @ c, ;
: disp32,   disp @ , ;

\ Set operand size.
: -opsize   2drop r> drop ;
: opsize!   is imm,  s !  ['] -opsize is ?opsize ;
: !op8    0 ['] imm8, ?opsize ;
: !op32   1 ['] imm32, ?opsize ;
: !op16   1 ['] imm16, ?opsize 66 c, ;

\ Set SIB byte.
: !sib   ['] sib, is ?sib, ;
: sib!   3 lshift + sib !  !sib ;

\ Set displacement.
: byte?   -80 80 within ;
: disp!   is ?disp, disp ! ;
: !disp8   ['] disp8, disp! ;
: !disp32   ['] disp32, disp! ;
: !disp ( a -- u ) dup byte? if !disp8 40 else !disp32 80 then ;
: -pc   here 5 + negate ;
: relative   -pc disp +! ;

\ Set immediate operand.
: imm!   imm !  ['] imm, is ?imm, ;

\ Implements addressing modes: register, indirect, indexed, and direct.
: reg1   rm! !reg ;
: reg2   3 lshift reg! ;
: !reg2   ['] reg2 is reg ;
: ind   dup mod! rm! !mem !reg2 ;
: ind#   swap !disp + ind ;
: idx   04 ind  sib! ;
: idx#   rot !disp 04 + ind  sib! ;
: addr   !disp32  05 ind ;

\ Reset assembler state.
: 0opsize   ['] opsize! is ?opsize ;
: 0ds   d off  s off ;
: 0reg   ['] reg1 is reg ;
: 0mrrm   c0 mrrm !  ['] mrrm, is ?mrrm, ;
: 0sib   ['] nop is ?sib, ;
: 0disp   ['] nop is ?disp, ;
: 0imm   imm off  ['] nop is ?imm,  0 is imm, ;
: 0asm   0imm 0disp 0reg 0ds 0mrrm 0sib 0opsize  dir? on ;

\ Enter and exit assembler mode.
: start-code   also assembler 0asm ;
: end-code     align previous ;

\ Implements addressing mode: immediate.
: imm8?   imm @ byte? ;
: ?sign-extend   d off  imm8? if 2 d !  ['] imm8, is ?imm, then ;
: alu#   opcode @ reg! 80 opcode ! ?sign-extend ;
: mov#   B0 s @ 3 lshift + rm@ + opcode ! 0ds -mrrm ;
: push#   imm8? if ['] imm8, 6A else ['] imm32, 68 then dup opcode ! rm! is ?imm, ;
: test#   F6 opcode ! ;
: imm-op   imm! immediate-opcode ;

\ Process one operand.  All operands except a direct address
\ have the stack picture ( n*x xt -addr ).
: addr?   dup -addr <> ;
: op   addr? if addr else drop execute then ;

\ Define instruction formats.
: instruction,   opcode! opcode, ?mrrm, ?sib, ?disp, ?imm, 0asm ;
: mnemonic ( u a "name" -- ) create ['] nop 3,  does> instruction, ;
: format:   create ] !csp  does> mnemonic ;
: immediate:   ' latestxt >body ! ;

\ Instruction formats.
format: 0op   -mrrm ;
format: 1reg   op reg>opcode 0ds -mrrm ;
format: 1op   opcode>reg op d off ;
format: 2op   op op ;
format: 2op-d   op op d off ;
format: 2op-ds   op op 0ds ;
format: 1addr   op relative -mrrm ;
format: 1imm8   !op8 op -mrrm ;

\ Instruction mnemonics.
00 2op add,  immediate: alu#
08 2op or,   immediate: alu#
0F44 2op-ds cmove,  \ Todo: other condition codes.
0FB6 2op-ds movzx,
0FBE 2op-ds movsx,
10 2op adc,  immediate: alu#
18 2op sbb,  immediate: alu#
20 2op and,  immediate: alu#
26 0op es,
28 2op sub,  immediate: alu#
2E 0op cs,
30 2op xor,  immediate: alu#
36 0op ss,
38 2op cmp,  immediate: alu#
3E 0op ds,
50 1reg push,  immediate: push#
58 1reg pop,
64 0op fs,
65 0op gs,
\ 70 jcc
84 2op-d test,   immediate: test#
86 2op-d xchg,
88 2op mov,  immediate: mov#
8D 2op-ds lea,
\ 8F/0 pop, rm
90 0op nop,
C3 0op ret,
\ C6/0 immediate mov to r/m
\ C7/0 immediate mov to r/m
CD 1imm8 int,
E8 1addr call,
E9 1addr jmp,
\ EB jmp rel8
F0 0op lock,
F2 0op rep,
F3 0op repz,
F4 0op hlt,
F5 0op cmc,
F610 1op not,
F618 1op neg,
F8 0op clc,
F9 0op stc,
FA 0op cli,
FB 0op sti,
FC 0op cld,
FD 0op std,
\ FE 0 inc rm
\ FF 1 dec rm
\ FF 2 call rm
\ FF 4 jmp rm
\ FF 6 push rm

: sp?   dup 4 = ;

\ Addressing mode syntax: immediate, indirect, and displaced indirect.
: #   ['] imm-op -addr ;
: )   2drop  sp? if 4 ['] idx else ['] ind then -addr  0reg 0opsize ;
: )#   2drop  sp? if 4 ['] idx# else ['] ind# then -addr  0reg 0opsize ;

\ Define registers.
: reg8    create ,  does> @ ['] reg -addr !op8 ;
: reg16   create ,  does> @ ['] reg -addr !op16 ;
: reg32   create ,  does> @ ['] reg -addr !op32 ;
: reg:    dup reg8 dup reg16 dup reg32 1+ ;

\ Register names.
0
reg: al ax eax   reg: cl cx ecx   reg: dl dx edx   reg: bl bx ebx
reg: ah sp esp   reg: ch bp ebp   reg: dh si esi   reg: bh di edi
drop

\ Runtime for ;CODE.  CODE! is defined elsewhere.
: (;code)   r> code! ;

base !  only forth definitions  also assembler

\ Standard assembler entry points.
: code    parse-name header, ?code, start-code  ;
: ;code   postpone (;code) reveal postpone [ ?csp start-code ; immediate

0asm
previous
