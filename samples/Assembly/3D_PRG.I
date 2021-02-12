; this file is part of Release, written by Malban in 2017
;
***********************************************************  
; input list in X
; destroys u
; 0 move
; negative use as shift
; positive end
asm_draw_3ds: 
       ldu 2,x
       lda 1,x;
starts:
       sta $d004;
       ldd ,u;
       sta $d001;
       clr $d000;
       lda ,x;
       inc $d000;
       stb $d001;
       sta $d00A;
       clr $d005;
       leax 4,x;
       ldu 2,x;
       lda ,x;
       bgt end1s;
       lda 1,x;
       ldb #$40;
waits: bitb $d00D;
       beq waits;
       ldb #0
       stb $d00A;
       bra starts;
end1s: ldd #$0040;
ends:  bitb $d00D;
       beq ends;
       sta $d00A
 rts

 
asm_draw_3d:
       ldu 1,x
start: ldd ,u;
       sta $d001;
       clr $d000;
       lda ,x;
       inc $d000;
       stb $d001;
       sta $d00A;
       clr $d005;
       leax 3,x;
       ldu 1,x;
       lda ,x;
       bgt end1;
       ldd #$0040;
wait:  bitb $d00D;
       beq wait;
       sta $d00A;
       bra start;
end1:  ldd #$0040;
end:   bitb $d00D;
       beq end;
       sta $d00A
 rts



; Cosinus data
cosinus3d: 
                    DB       63, 62, 61, 60, 58, 55, 52, 48, 43, 39, 34 ; 11 
                    DB       28, 23, 17, 10, 4, -1, -7, -14, -20, -25, -31 ; 22 
                    DB       -36, -41, -46, -50, -53, -56, -59, -61, -62, -62, -62 ; 33 
                    DB       -62, -61, -59, -56, -53, -50, -46, -41, -36, -31, -25 ; 44 
                    DB       -20, -14, -7, -1, 4, 10, 17, 23, 28, 34, 39 ; 55 
                    DB       43, 48, 52, 55, 58, 60, 61, 62, 63 
; Sinus data
sinus3d: 
                    DB       0, 6, 12, 18, 24, 30, 35, 40, 45, 49, 52 ; 11 
                    DB       56, 58, 60, 62, 62, 62, 62, 61, 59, 57, 54 ; 22 
                    DB       51, 47, 42, 38, 32, 27, 21, 15, 9, 3, -3 ; 33 
                    DB       -9, -15, -21, -27, -32, -38, -42, -47, -51, -54, -57 ; 44 
                    DB       -59, -61, -62, -62, -62, -62, -60, -58, -56, -52, -49 ; 55 
                    DB       -45, -40, -35, -30, -24, -18, -12, -6, -3 
