#ifndef Version
    #include "Version.h"
#endif

; it shouldn't have any problem with apostrophes in comments
; or "quotation marks" or more ;s

        GET     otherfile.s

        ^       0,r12 ; typical data structure layout
offset1 #       4
        #       4 ; sometimes no label assigned
offset2 #       0
size    *       :INDEX:@

 IF ?offset1 = 4
        ! 1, "This is an assembly-time assert"
 ELIF -1<>:NOT:0
        ! 1, "This is another assert"
 ELSE
        ! 0, "But this is a warning", 1
 ENDIF

my_r0   RN      0 ; non-standard register name symbols

sixteen * 4 * 4

        GBLS    VBar
VBar    SETS    "|"

        MACRO
$label  MyMacro$suffix $arg1 = default, $arg2
        LCLS    thing
 [ "$arg2"=""
thing   SETS    "wibble ; this isn't a comment"
 |
thing   SETS    $arg2
 ]
        LCLL    boolean
boolean SETL    {TRUE}
        EXPORT  $arg1
$arg1
$thing  MOV     pc, #0
        MEND

        
        ORG     0

        ARM

        AREA    |Area$$Name|, CODE, READONLY

MyFunc  ROUT

just_a_label
label_and ; comment
        LDR     my_r0, =just_a_statement
label   AND     r0, r1, r2 ; and a comment
        MOV     r0, r1, r2, LSL r3
        MOV.W   r0, r1, r2, ROR #1
        ADDS.N  r0, r1
        IT      MI
        MOVMI   r0, r1, RRX
        UND     #0x10-&10
        PUSH    {r0,r2-r4,ip,lr}
        LDF     F0, =25E-1
        VMOVEQ.F64 d0, #-.7e1

Symbols_may_contain_d161ts_and_underscores
but_must_start_with_a_letter
and_are_case_SenSitIve
01 ; this, by contrast, is a local label

        LDR     r0, here
        B       .+8
here    DATA
        DCD     1
        BNE     %BT01

        DCB 1,2,3,'A',';','"' ; bytes
        = "This is a string with embedded $$ dollar and "" double quote characters", 0
        = "$VBar.not_part_of_variable_name", 0
        DCD     -1 ; words
        &       -2,:INDEX:offset1 ; more words
        %       16
same_as SPACE   16
or      FILL    16, 0
        INCBIN  include.bin

        END
