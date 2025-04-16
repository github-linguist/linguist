; Copyright 2011-2018 Frederic Requin
;
; Non regression tests for 68000 (1500+ cases)

    org     0
vectors:
    dc.l    $00006000                      ;  0 : Initial SSP
    dc.l    start                          ;  1 : Initial PC
    dc.l    0                              ;  2
    dc.l    trap_addr                      ;  3 : Address error
    dc.l    trap_ill                       ;  4 : Illegal instruction
    dc.l    0                              ;  5
    dc.l    trap_chk                       ;  6 : CHK instruction
    dc.l    trap_v                         ;  7 : TRAPV instruction
    blk.l   24,0                           ;  8 - 31
    dc.l    trap_0                         ; 32
    dc.l    trap_1                         ; 33
    dc.l    trap_2                         ; 34
    dc.l    trap_3                         ; 35
    dc.l    trap_4                         ; 36
    dc.l    trap_5                         ; 37
    dc.l    trap_6                         ; 38
    dc.l    trap_7                         ; 39
    dc.l    trap_8                         ; 40
    dc.l    trap_9                         ; 41
    dc.l    trap_A                         ; 42
    dc.l    trap_B                         ; 43
    dc.l    trap_C                         ; 44
    dc.l    trap_D                         ; 45
    dc.l    trap_E                         ; 46
    dc.l    trap_F                         ; 47
trap_addr:
    move.w  (SP),D0
    move.l  2(SP),A0
    move.w  6(SP),D1
    move.l  10(SP),D2
    bclr    #0,D2
    move.l  D2,10(SP)
    addq.l  #8,SP
    rte
trap_ill:
    addq.l  #2,2(SP)
    rte
trap_chk:
    rte
trap_v:
    rte
trap_0:
    rte
trap_1:
    rte
trap_2:
    rte
trap_3:
    rte
trap_4:
    rte
trap_5:
    rte
trap_6:
    rte
trap_7:
    rte
trap_8:
    rte
trap_9:
    rte
trap_A:
    rte
trap_B:
    rte
trap_C:
    rte
trap_D:
    rte
trap_E:
    rte
trap_F:
    rte
start:
    ; Exceptions tests
    illegal
    trap    #0
    trap    #15
    
    move    #%00010,CCR
    trapv
    
    bsr.w   movep_test

    pea     return(pc)
    jmp     bcd_test
return:
    jsr     sub_b_test
    jsr     sub_w_test
    jsr     sub_l_test

    bsr.w   add_b_test
    bsr.w   add_w_test
    bsr.w   add_l_test

    bsr     neg_test

    moveq   #0,D0
    tas     D0
    moveq   #5,D0
    moveq   #3,D1
    chk.w   D1,D0
    moveq   #-1,D0
    chk.w   D1,D0

    jsr     movem_test

    lea     $12345678,A5
    link    A5,#-12
    unlk    A5

    move.l  A7,USP
    move.l  USP,A0
    
    exg     A0,A5
    exg     A5,D0
    exg     D0,D1
    
    move.w  #$0080,D0
    ext.w   D0
    move.w  #$FF00,D0
    ext.w   D0
    move.l  #$00008000,D0
    ext.l   D0
    move.w  #$0000,D0
    ext.l   D0
    
    jsr     cmpa_w_test
    jsr     cmpa_l_test
    
    jsr     cmp_b_test
    jsr     cmp_w_test
    jsr     cmp_l_test

    jsr     cmpi_b_test
    jsr     cmpi_w_test
    jsr     cmpi_l_test

    jsr     scc_test
    jsr     bcc_test
    jsr     dbcc_test
    
    jsr     div_test
    jsr     mult_test

    jsr     biti_test
    jsr     bitr_test

    jsr     clr_tst_test
    jsr     not_test
    jsr     neg_test

    jsr     addq_test
    jsr     subq_test

    jsr     adda_w_test
    jsr     adda_l_test

    jsr     suba_w_test
    jsr     suba_l_test

    jsr     move_l_test_0
    jsr     move_l_test_1
    jsr     move_l_test_2
    jsr     move_l_test_3
    jsr     move_l_test_4
    jsr     move_l_test_5
    jsr     move_l_test_6
    jsr     move_l_test_7
    jsr     move_l_test_8
    
    jsr     move_w_test_0
    jsr     move_w_test_1
    jsr     move_w_test_2
    jsr     move_w_test_3
    jsr     move_w_test_4
    jsr     move_w_test_5
    jsr     move_w_test_6
    jsr     move_w_test_7
    jsr     move_w_test_8
    
    jsr     move_b_test_0
    jsr     move_b_test_2
    jsr     move_b_test_3
    jsr     move_b_test_4
    jsr     move_b_test_5
    jsr     move_b_test_6
    jsr     move_b_test_7
    jsr     move_b_test_8
    
    jsr     shifti_test
    jsr     shiftr_test
    jsr     shiftm_test
    
    jsr     addi_b_test
    jsr     addi_w_test
    jsr     addi_l_test
    
    jsr     subi_b_test
    jsr     subi_w_test
    jsr     subi_l_test
    
    bsr.w   log_b_test
    bsr.w   log_w_test
    bsr.w   log_l_test
    
    bsr.w   logi_b_test
    bsr.w   logi_w_test
    bsr.w   logi_l_test
    
    bra.w   start
    
    ; MOVEP tests
    ;------------
movep_test:
    lea     $8000.l,A0
    move.l  #$01234567,(A0)
    move.l  #$89ABCDEF,4(A0)
    moveq   #0,D0
    movep.w 0(A0),D0
    movep.w 1(A0),D0
    movep.l 0(A0),D0
    movep.l 1(A0),D0
    move.l  #$00001122,D0
    move.l  #$00003344,D1
    movep.w D0,0(A0)
    movep.w D1,1(A0)
    move.l  (A0),D0
    move.l  #$11223344,D0
    move.l  #$55667788,D1
    movep.l D0,0(A0)
    movep.l D1,1(A0)
    move.l  (A0)+,D0
    move.l  (A0),D1
    rts
    
    ; ABCD, SBCD, NBCD tests
    ;-----------------------
bcd_test:
    lea     $8000.l,A0
    move    #%10000,CCR
    move.b  #$99,D0
    move.b  #$01,D1
    abcd    D0,D1
    move.l  #$01990099,(A0)+
    move.l  A0,A1
    move.l  #$09010101,(A1)+
    abcd    -(A0),-(A1)
    abcd    -(A0),-(A1)
    abcd    -(A0),-(A1)
    abcd    -(A0),-(A1)
    move.l  (A0),D0
    move.l  (A1),D1
    
    addq.l  #4,A0
    addq.l  #4,A1
    move.b  #$99,D0
    move.b  #$00,D1
    move    #%10000,CCR
    sbcd    D0,D1
    sbcd    -(A0),-(A1)
    sbcd    -(A0),-(A1)
    sbcd    -(A0),-(A1)
    sbcd    -(A0),-(A1)
    move.l  (A0),D0
    move.l  (A1),D1

    moveq   #1,D1
    nbcd    D0
    nbcd    (A0)
    move.b  (A0),D0
    nbcd    (A0)+
    move.b  -1(A0),D0
    nbcd    1(A0)
    move.b  1(A0),D0
    nbcd    -(A0)
    move.b  (A0),D0
    nbcd    0(A0,D1.w)
    move.b  0(A0,d1.w),D0
    nbcd    $8000
    move.b  $8000,D0
    rts

    ; BTST, BCHG, BCLR, BSET tests
    ;-----------------------------
biti_test:
    lea     $8000.l,A0
    move.l  #$55555555,D0
    move.l  D0,(A0)
    move.l  D0,-$1000(A0)
    moveq   #1,D1
    
    btst    #0,D0
    btst    #7,D0
    btst    #8,D0
    btst    #15,D0
    btst    #16,D0
    btst    #23,D0
    btst    #24,D0
    btst    #31,D0
    btst    #0,(A0)
    btst    #7,(A0)+
    btst    #0,-1(A0)
    btst    #7,-(A0)
    btst    #0,1(A0,D1.w)
    btst    #7,$7000.w
    btst    #0,$8000.l
    
    bchg    #0,D0
    bchg    #7,D0
    bchg    #8,D0
    bchg    #15,D0
    bchg    #16,D0
    bchg    #23,D0
    bchg    #24,D0
    bchg    #31,D0
    bchg    #0,(A0)
    bchg    #7,(A0)+
    bchg    #0,-1(A0)
    bchg    #7,-(A0)
    bchg    #0,1(A0,D1.w)
    bchg    #7,$7000.w
    bchg    #0,$8000.l
    
    bclr    #0,D0
    bclr    #7,D0
    bclr    #8,D0
    bclr    #15,D0
    bclr    #16,D0
    bclr    #23,D0
    bclr    #24,D0
    bclr    #31,D0
    bclr    #0,(A0)
    bclr    #7,(A0)+
    bclr    #0,-1(A0)
    bclr    #7,-(A0)
    bclr    #0,1(A0,D1.w)
    bclr    #7,$7000.w
    bclr    #0,$8000.l
    
    bset    #0,D0
    bset    #7,D0
    bset    #8,D0
    bset    #15,D0
    bset    #16,D0
    bset    #23,D0
    bset    #24,D0
    bset    #31,D0
    bset    #0,(A0)
    bset    #7,(A0)+
    bset    #0,-1(A0)
    bset    #7,-(A0)
    bset    #0,1(A0,D1.w)
    bset    #7,$7000.w
    bset    #0,$8000.l
    
    rts
    
bitr_test:
    lea     $8000.l,A0
    move.l  #$55555555,D0
    move.l  D0,(A0)
    move.l  D0,-$1000(A0)
    moveq   #1,D1

    moveq   #0,D2    
    btst    D2,D0
    addq.l  #7,D2
    btst    D2,D0
    addq.l  #1,D2
    btst    D2,D0
    addq.l  #7,D2
    btst    D2,D0
    addq.l  #1,D2
    btst    D2,D0
    addq.l  #7,D2
    btst    D2,D0
    addq.l  #1,D2
    btst    D2,D0
    addq.l  #7,D2
    btst    D2,D0
    
    moveq   #0,D2
    moveq   #7,D3
    btst    D2,(A0)
    btst    D3,(A0)+
    btst    D2,-1(A0)
    btst    D3,-(A0)
    btst    D2,1(A0,D1.w)
    btst    D3,$7000.w
    btst    D2,$8000.l
    
    moveq   #0,D2
    bchg    D2,D0
    addq.l  #7,D2
    bchg    D2,D0
    addq.l  #1,D2
    bchg    D2,D0
    addq.l  #7,D2
    bchg    D2,D0
    addq.l  #1,D2
    bchg    D2,D0
    addq.l  #7,D2
    bchg    D2,D0
    addq.l  #1,D2
    bchg    D2,D0
    addq.l  #7,D2
    bchg    D2,D0
    
    moveq   #0,D2
    moveq   #7,D3
    bchg    D2,(A0)
    bchg    D3,(A0)+
    bchg    D2,-1(A0)
    bchg    D3,-(A0)
    bchg    D2,1(A0,D1.w)
    bchg    D3,$7000.w
    bchg    D2,$8000.l
    
    moveq   #0,D2
    bclr    D2,D0
    addq.l  #7,D2
    bclr    D2,D0
    addq.l  #1,D2
    bclr    D2,D0
    addq.l  #7,D2
    bclr    D2,D0
    addq.l  #1,D2
    bclr    D2,D0
    addq.l  #7,D2
    bclr    D2,D0
    addq.l  #1,D2
    bclr    D2,D0
    addq.l  #7,D2
    bclr    D2,D0
    
    moveq   #0,D2
    moveq   #7,D3
    bclr    D2,(A0)
    bclr    D3,(A0)+
    bclr    D2,-1(A0)
    bclr    D3,-(A0)
    bclr    D2,1(A0,D1.w)
    bclr    D3,$7000.w
    bclr    D2,$8000.l
    
    moveq   #0,D2
    bset    D2,D0
    addq.l  #7,D2
    bset    D2,D0
    addq.l  #1,D2
    bset    D2,D0
    addq.l  #7,D2
    bset    D2,D0
    addq.l  #1,D2
    bset    D2,D0
    addq.l  #7,D2
    bset    D2,D0
    addq.l  #1,D2
    bset    D2,D0
    addq.l  #7,D2
    bset    D2,D0
    
    moveq   #0,D2
    moveq   #7,D3
    bset    D2,(A0)
    bset    D3,(A0)+
    bset    D2,-1(A0)
    bset    D3,-(A0)
    bset    D2,1(A0,D1.w)
    bset    D3,$7000.w
    bset    D2,$8000.l
    
    rts
    
    ; AND.B, EOR.B, OR.B tests
    ;-------------------------
log_b_test:
    lea     $8000.l,A0
    move.w  #$55AA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)+
    moveq   #0,D0
    moveq   #4,D1
    
    move.b  #$FF,D0
    and.b   log_b_test+1(PC),D0
    or.b    log_b_test+1(PC),D0
    and.b   log_b_test-1(PC,D1.w),D0
    or.b    log_b_test-1(PC,D1.l),D0

    move.b  #$FF,D0
    eor.b   D0,(A0)+
    and.b   (A0)+,D0
    or.b    D0,(A0)+
    or.b    (A0)+,D0
    and.b   D0,(A0)+
    eor.b   D0,(A0)
    and.b   (A0),D0
    or.b    D0,(A0)
    or.b    (A0),D0
    and.b   D0,(A0)
    
    move.b  #$FF,D0
    eor.b   D0,-(A0)
    and.b   -(A0),D0
    or.b    D0,-(A0)
    or.b    -(A0),D0
    and.b   D0,-(A0)
    eor.b   D0,1(A0)
    and.b   2(A0),D0
    or.b    D0,2(A0)
    or.b    3(A0),D0
    and.b   D0,3(A0)
    
    move.b  #$FF,D0
    eor.b   D0,-1(A0,D1.w)
    and.b   -2(A0,D1.l),D0
    or.b    D0,-2(A0,D1.l)
    or.b    -3(A0,D1.w),D0
    and.b   D0,-3(A0,D1.w)
    eor.b   D1,D0
    and.b   D1,D0
    or.b    D1,D0

    rts
    
    ; AND.W, EOR.W, OR.W tests
    ;-------------------------
log_w_test:
    lea     $8000.l,A0
    move.w  #$5555,(A0)+
    move.w  #$AAAA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)+
    moveq   #8,D1
    
    move.w  #$FFFF,D0
    and.w   log_w_test+2(PC),D0
    or.w    log_w_test+2(PC),D0
    and.w   log_w_test(PC,D1.w),D0
    or.w    log_w_test(PC,D1.l),D0

    move.w  #$FFFF,D0
    eor.w   D0,(A0)+
    and.w   (A0)+,D0
    or.w    D0,(A0)+
    or.w    (A0)+,D0
    and.w   D0,(A0)+
    eor.w   D0,(A0)
    and.w   (A0),D0
    or.w    D0,(A0)
    or.w    (A0),D0
    and.w   D0,(A0)
    
    move.w  #$FFFF,D0
    eor.w   D0,-(A0)
    and.w   -(A0),D0
    or.w    D0,-(A0)
    or.w    -(A0),D0
    and.w   D0,-(A0)
    eor.w   D0,2(A0)
    and.w   4(A0),D0
    or.w    D0,4(A0)
    or.w    6(A0),D0
    and.w   D0,6(A0)
    
    move.w  #$FFFF,D0
    eor.w   D0,-2(A0,D1.w)
    and.w   -4(A0,D1.l),D0
    or.w    D0,-4(A0,D1.l)
    or.w    -6(A0,D1.w),D0
    and.w   D0,-6(A0,D1.w)
    eor.w   D1,D0
    and.w   D1,D0
    or.w    D1,D0

    rts
    
    ; AND.L, EOR.L, OR.L tests
    ;-------------------------
log_l_test:
    lea     $8000.l,A0
    move.l  #$55555555,(A0)+
    move.l  #$AAAAAAAA,(A0)+
    move.l  #$12345678,(A0)+
    move.l  #$9ABCDEF0,(A0)+
    move.l  #$FFFFFFFF,(A0)+
    moveq   #16,D1

    move.l  #$FFFFFFFF,D0
    and.l   log_l_test+2(PC),D0
    or.l    log_l_test+2(PC),D0
    and.l   log_l_test(PC,D1.w),D0
    or.l    log_l_test(PC,D1.l),D0

    move.l  #$FFFFFFFF,D0
    eor.l   D0,(A0)+
    and.l   (A0)+,D0
    or.l    D0,(A0)+
    or.l    (A0)+,D0
    and.l   D0,(A0)+
    eor.l   D0,(A0)
    and.l   (A0),D0
    or.l    D0,(A0)
    or.l    (A0),D0
    and.l   D0,(A0)
    
    move.l  #$FFFFFFFF,D0
    eor.l   D0,-(A0)
    and.l   -(A0),D0
    or.l    D0,-(A0)
    or.l    -(A0),D0
    and.l   D0,-(A0)
    eor.l   D0,4(A0)
    and.l   8(A0),D0
    or.l    D0,8(A0)
    or.l    12(A0),D0
    and.l   D0,12(A0)
    
    move.l  #$FFFFFFFF,D0
    eor.l   D0,-4(A0,D1.w)
    and.l   -8(A0,D1.l),D0
    or.l    D0,-8(A0,D1.l)
    or.l    -12(A0,D1.w),D0
    and.l   D0,-12(A0,D1.w)
    eor.l   D1,D0
    and.l   D1,D0
    or.l    D1,D0

    rts
    
    ; ANDI, EORI, ORI tests
    ;----------------------
logi_b_test:
    lea     $8000.l,A0
    move.w  #$55AA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)+
    moveq   #0,D0
    moveq   #4,D1
    
    move.b  #$FF,D0
    andi.b  #$12,D0
    eori.b  #$34,D0
    ori.b   #$56,D0

    andi.b  #$12,(A0)+
    eori.b  #$34,(A0)+
    ori.b   #$56,(A0)+
    andi.b  #$78,(A0)
    eori.b  #$9A,(A0)
    ori.b   #$BC,(A0)
    
    andi.b  #$12,-(A0)
    eori.b  #$34,-(A0)
    ori.b   #$56,-(A0)
    andi.b  #$78,1(A0)
    eori.b  #$9A,2(A0)
    ori.b   #$BC,3(A0)
 
    andi.b  #$12,-1(A0,D1.w)
    eori.b  #$34,-2(A0,D1.l)
    ori.b   #$56,-3(A0,D1.w)
    
    andi.b  #$12,$7000.w
    eori.b  #$34,$7000.w
    ori.b   #$56,$7000.w
    andi.b  #$78,$8000.l
    eori.b  #$9A,$8000.l
    ori.b   #$BC,$8000.l

    rts

    ; ANDI.W, EORI.W, ORI.W tests
    ;----------------------------
logi_w_test:
    lea     $8000.l,A0
    move.w  #$5555,(A0)+
    move.w  #$AAAA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)+
    moveq   #0,D0
    moveq   #8,D1
    
    move.w  #$FFFF,D0
    andi.w  #$1212,D0
    eori.w  #$3434,D0
    ori.w   #$5656,D0
    
    andi.w  #$1212,(A0)+
    eori.w  #$3434,(A0)+
    ori.w   #$5656,(A0)+
    andi.w  #$7878,(A0)
    eori.w  #$9A9A,(A0)
    ori.w   #$BCBC,(A0)
    
    andi.w  #$1212,-(A0)
    eori.w  #$3434,-(A0)
    ori.w   #$5656,-(A0)
    andi.w  #$7878,2(A0)
    eori.w  #$9A9A,4(A0)
    ori.w   #$BCBC,6(A0)

    andi.w  #$1212,-2(A0,D1.w)
    eori.w  #$3434,-4(A0,D1.l)
    ori.w   #$5656,-6(A0,D1.w)

    andi.w  #$1212,$7000.w
    eori.w  #$3434,$7000.w
    ori.w   #$5656,$7000.w
    andi.w  #$7878,$8000.l
    eori.w  #$9A9A,$8000.l
    ori.w   #$BCBC,$8000.l

    rts
    
    ; ANDI.L, EORI.L, ORI.L tests
    ;----------------------------
logi_l_test:
    lea     $8000.l,A0
    move.l  #$55555555,(A0)+
    move.l  #$AAAAAAAA,(A0)+
    move.l  #$12345678,(A0)+
    move.l  #$9ABCDEF0,(A0)+
    move.l  #$FFFFFFFF,(A0)+
    moveq   #16,D1

    move.l  #$FFFFFFFF,D0
    andi.l  #$12121212,D0
    eori.l  #$34343434,D0
    ori.l   #$56565656,D0

    andi.l  #$12121212,(A0)+
    eori.l  #$34343434,(A0)+
    ori.l   #$56565656,(A0)+
    andi.l  #$78787878,(A0)
    eori.l  #$9A9A9A9A,(A0)
    ori.l   #$BCBCBCBC,(A0)

    andi.l  #$12121212,-(A0)
    eori.l  #$34343434,-(A0)
    ori.l   #$56565656,-(A0)
    andi.l  #$78787878,4(A0)
    eori.l  #$9A9A9A9A,8(A0)
    ori.l   #$BCBCBCBC,12(A0)

    andi.l  #$12121212,-4(A0,D1.w)
    eori.l  #$34343434,-8(A0,D1.l)
    ori.l   #$56565656,-12(A0,D1.w)

    andi.l  #$12121212,$7000.w
    eori.l  #$34343434,$7000.w
    ori.l   #$56565656,$7000.w
    andi.l  #$78787878,$8000.l
    eori.l  #$9A9A9A9A,$8000.l
    ori.l   #$BCBCBCBC,$8000.l

    rts

    ; ADD tests
    ;----------
add_b_test:
    lea     $8000.l,A0
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    moveq   #$7F,D0
    moveq   #2,D1
    add.b   (A0)+,D0
    add.b   D0,(A0)+
    add.b   (A0),D0
    add.b   D0,(A0)
    add.b   -(A0),D0
    add.b   D0,-(A0)
    add.b   1(A0),D0
    add.b   D0,1(A0)
    add.b   3(A0,D1.w),D0
    add.b   D0,3(A0,D1.w)
    add.b   D1,D0
    add.b   $7000.w,D0
    add.b   $8000.l,D0
    add.b   add_b_test+1(PC),D0
    add.b   add_b_test-1(PC,D1),D0
    add.b   #$12,D0
    rts
    
add_w_test:
    lea     $8000.l,A0
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    move.w  #$7FFF,D0
    moveq   #2,D1
    add.w   (A0)+,D0
    add.w   D0,(A0)+
    add.w   (A0),D0
    add.w   D0,(A0)
    add.w   -(A0),D0
    add.w   D0,-(A0)
    add.w   2(A0),D0
    add.w   D0,2(A0)
    add.w   2(A0,D1.w),D0
    add.w   D0,4(A0,D1.w)
    add.w   D1,D0
    add.w   A0,D0
    add.w   $7000.w,D0
    add.w   $8000.l,D0
    add.w   add_w_test+2(PC),D0
    add.w   add_w_test(PC,D1),D0
    add.w   #$1234,D0
    rts
    
add_l_test:
    lea     $8000.l,A0
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    move.l  #$12345678,8(A0)
    move.l  #$7FFFFFFF,D0
    moveq   #2,D1
    add.l   (A0)+,D0
    add.l   D0,(A0)+
    add.l   (A0),D0
    add.l   D0,(A0)
    add.l   -(A0),D0
    add.l   D0,-(A0)
    add.l   4(A0),D0
    add.l   D0,4(A0)
    add.l   6(A0,D1.w),D0
    add.l   D0,2(A0,D1.w)
    add.l   D1,D0
    add.l   A0,D0
    add.l   $7000.w,D0
    add.l   $8000.l,D0
    add.l   add_l_test+2(PC),D0
    add.l   add_l_test(PC,D1),D0
    add.l   #$12345678,D0
    rts
    
    ; ADDQ tests
    ;-----------
addq_test:
    lea     $7000.w,A0
    lea     $8000.l,A1
    suba.l  A2,A2
    moveq   #0,D0
    moveq   #2,D1

    clr.b   (A0)
    clr.b   (A1)    
    addq.b  #8,D0
    addq.b  #1,(A0)
    addq.b  #7,(A1)+
    addq.b  #8,-1(A1)
    addq.b  #8,-3(A1,D1.w)
    addq.b  #8,-(A1)
    addq.b  #1,$7000.w
    addq.b  #1,$8000.l
    move.b  (A0),D0
    move.b  (A1),D0

    clr.w   (A0)
    move.w  (A0),D0
    clr.w   (A1)
    addq.w  #8,D0
    addq.w  #8,A2
    addq.w  #1,(A0)
    move.w  (A0),D0
    addq.w  #7,(A1)+
    addq.w  #8,-2(A1)
    addq.w  #8,-4(A1,D1.w)
    addq.w  #8,-(A1)
    addq.w  #1,$7000.w
    addq.w  #1,$8000.l
    move.w  (A0),D0
    move.w  (A1),D0

    clr.l   (A0)
    clr.l   (A1)
    addq.l  #8,D0
    addq.l  #8,A2
    addq.l  #1,(A0)
    addq.l  #7,(A1)+
    addq.l  #8,-4(A1)
    addq.l  #8,-6(A1,D1.w)
    addq.l  #8,-(A1)
    addq.l  #1,$7000.w
    addq.l  #1,$8000.l
    move.l  (A0),D0
    move.l  (A1),D0
    
    rts

    ; ADDA.W tests
    ;-------------
adda_w_test:
    lea     $7FF0.w,A0
    lea     $7FF0.w,A1
    lea     $8000.l,A2
    move.l  #$12345678,(A2)
    moveq   #2,D1
    adda.w  A1,A0
    adda.w  D1,A0
    adda.w  (A2)+,A0
    adda.w  (A2),A0
    adda.w  -(A2),A0
    adda.w  2(A2),A0
    adda.w  6(A2,D1.w),A0
    adda.w  $7000.w,A0
    adda.w  $8000.l,A0
    adda.w  adda_w_test+2(PC),A0
    adda.w  adda_w_test(PC,D1.l),A0
    adda.w  #$1234,A0
    rts

    ; ADDA.L tests
    ;-------------
adda_l_test:
    lea     $7FFFFFF0.l,A0
    lea     $7FFFFFF0.l,A1
    lea     $8000.l,A2
    move.l  #$12345678,(A2)
    move.l  #$9ABCDEF0,4(A2)
    moveq   #2,D1
    adda.l  A1,A0
    adda.l  #32,A0
    adda.l  D1,A0
    adda.l  (A2)+,A0
    adda.l  (A2),A0
    adda.l  -(A2),A0
    adda.l  4(A2),A0
    adda.l  6(A2,D1.w),A0
    adda.l  $7000.w,A0
    adda.l  $8000.l,A0
    adda.l  adda_l_test+2(PC),A0
    adda.l  adda_l_test(PC,D1.l),A0
    adda.l  #$12345678,A0
    rts
    
    ; ADDI.B tests
    ;-------------
addi_b_test:
    lea     $8000.l,A0
    move.w  #$55AA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #4,A0
    moveq   #0,D0

    move.b  #$7F,D0
    addi.b  #$7F,D0
    addq.b  #2,D0
    move.b  #$7F,D1
    moveq   #0,D2
    addx.b  D2,D1
    
    moveq   #4,D1
    addi.b  #$12,(A0)+
    addi.b  #$23,(A0)
    addi.b  #$45,-(A0)
    addi.b  #$56,1(A0)
    addi.b  #$78,-3(A0,D1.w)
    addi.b  #$9A,$7000.w
    addi.b  #$BC,$8000.l
    
    rts
    
    ; ADDI.W tests
    ;-------------
addi_w_test:
    lea     $8000.l,A0
    move.w  #$5555,(A0)+
    move.w  #$AAAA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #6,A0
    moveq   #0,D0

    move.w  #$7FFF,D0
    addi.w  #$7FFF,D0
    addq.w  #2,D0
    move.w  #$7FFF,D1
    moveq   #0,D2
    addx.w  D2,D1
    
    moveq   #8,D1
    addi.w  #$1212,(A0)+
    addi.w  #$2323,(A0)
    addi.w  #$4545,-(A0)
    addi.w  #$5656,2(A0)
    addi.w  #$7878,-6(A0,D1.w)
    addi.w  #$9A9A,$7000.w
    addi.w  #$BCBC,$8000.l
    
    rts
    
    ; ADDI.L tests
    ;-------------
addi_l_test:
    lea     $8000.l,A0
    move.l  #$55555555,(A0)+
    move.l  #$AAAAAAAA,(A0)+
    move.l  #$12345678,(A0)+
    move.l  #$9ABCDEF0,(A0)+
    move.l  #$FFFFFFFF,(A0)
    lea     -16(A0),A0

    move.l  #$7FFFFFFF,D0
    addi.l  #$7FFFFFFF,D0
    addq.l  #2,D0
    move.l  #$7FFFFFFF,D1
    moveq   #0,D2
    addx.l  D2,D1

    moveq   #16,D1
    addi.l  #$12121212,(A0)+
    addi.l  #$23232323,(A0)
    addi.l  #$45454545,-(A0)
    addi.l  #$56565656,4(A0)
    addi.l  #$78787878,-12(A0,D1.w)
    addi.l  #$9A9A9A9A,$7000.w
    addi.l  #$BCBCBCBC,$8000.l
    
    rts
    
    ; SUB tests
    ;----------
sub_b_test:
    lea     $8000.l,A0
    moveq   #$7F,D0
    moveq   #2,D1
    sub.b   (A0)+,D0
    sub.b   (A0),D0
    sub.b   -(A0),D0
    sub.b   1(A0),D0
    sub.b   6(A0,D1.w),D0
    sub.b   D1,D0
    sub.b   $7000.w,D0
    sub.b   $8000.l,D0
    sub.b   sub_b_test+1(PC),D0
    sub.b   sub_b_test-1(PC,D1),D0
    sub.b   #$12,D0
    rts
    
sub_w_test:
    lea     $8000.l,A0
    move.w  #$7FFF,D0
    moveq   #2,D1
    sub.w   (A0)+,D0
    sub.w   (A0),D0
    sub.w   -(A0),D0
    sub.w   2(A0),D0
    sub.w   6(A0,D1.w),D0
    sub.w   D1,D0
    sub.w   A0,D0
    sub.w   $7000.w,D0
    sub.w   $8000.l,D0
    sub.w   sub_w_test+2(PC),D0
    sub.w   sub_w_test(PC,D1),D0
    sub.w   #$1234,D0
    rts
    
sub_l_test:
    lea     $8000.l,A0
    move.l  #$7FFFFFFF,D0
    moveq   #2,D1
    sub.l   (A0)+,D0
    sub.l   (A0),D0
    sub.l   -(A0),D0
    sub.l   4(A0),D0
    sub.l   6(A0,D1.w),D0
    sub.l   D1,D0
    sub.l   A0,D0
    sub.l   $7000.w,D0
    sub.l   $8000.l,D0
    sub.l   sub_l_test+2(PC),D0
    sub.l   sub_l_test(PC,D1),D0
    sub.l   #$12345678,D0
    rts

    ; CMP tests
    ;----------
cmp_b_test:
    lea     $8000.l,A0
    move.l  #$7F7F7F7F,(A0)
    moveq   #$7F,D0
    moveq   #1,D1
    cmp.b   (A0)+,D0
    cmp.b   (A0),D0
    cmp.b   -(A0),D0
    cmp.b   1(A0),D0
    cmp.b   6(A0,D1.w),D0
    cmp.b   D1,D0
    cmp.b   $7000.w,D0
    cmp.b   $8000.l,D0
    cmp.b   cmp_b_test+1(PC),D0
    cmp.b   cmp_b_test-1(PC,D1),D0
    cmp.b   #$12,D0
    rts
    
cmp_w_test:
    lea     $8000.l,A0
    move.l  #$7FFF7FFF,(A0)
    move.w  #$7FFF,D0
    moveq   #2,D1
    cmp.w   (A0)+,D0
    cmp.w   (A0),D0
    cmp.w   -(A0),D0
    cmp.w   2(A0),D0
    cmp.w   6(A0,D1.w),D0
    cmp.w   D1,D0
    cmp.w   A0,D0
    cmp.w   $7000.w,D0
    cmp.w   $8000.l,D0
    cmp.w   cmp_w_test+2(PC),D0
    cmp.w   cmp_w_test(PC,D1),D0
    cmp.w   #$1234,D0
    rts
    
cmp_l_test:
    lea     $8000.l,A0
    move.l  #$7FFFFFFF,(A0)
    move.l  #$7FFFFFFF,D0
    moveq   #2,D1
    cmp.l   (A0)+,D0
    cmp.l   (A0),D0
    cmp.l   -(A0),D0
    cmp.l   4(A0),D0
    cmp.l   6(A0,D1.w),D0
    cmp.l   D1,D0
    cmp.l   A0,D0
    cmp.l   $7000.w,D0
    cmp.l   $8000.l,D0
    cmp.l   cmp_l_test+2(PC),D0
    cmp.l   cmp_l_test(PC,D1),D0
    cmp.l   #$12345678,D0
    rts

    ; CMPA.W tests
    ;-------------
cmpa_w_test:
    lea     $7FFF.w,A0
    lea     $7FFF.w,A1
    lea     $8000.l,A2
    move.l  #12345678,(A2)
    moveq   #2,D1
    cmpa.w  A1,A0
    cmpa.w  D1,A0
    cmpa.w  (A2)+,A0
    cmpa.w  (A2),A0
    cmpa.w  -(A2),A0
    cmpa.w  2(A2),A0
    cmpa.w  6(A2,D1.w),A0
    cmpa.w  $7000.w,A0
    cmpa.w  $8000.l,A0
    cmpa.w  cmpa_w_test+2(PC),A0
    cmpa.w  cmpa_w_test(PC,D1.l),A0
    cmpa.w  #$1234,A0
    rts

    ; CMPA.L tests
    ;-------------
cmpa_l_test:
    lea     $7FFFFFFF.l,A0
    lea     $7FFFFFFF.l,A1
    lea     $8000.l,A2
    move.l  #$12345678,(A2)
    move.l  #$9ABCDEF0,4(A2)
    moveq   #2,D1
    cmpa.l  A1,A0
    cmpa.l  D1,A0
    cmpa.l  (A2)+,A0
    cmpa.l  (A2),A0
    cmpa.l  -(A2),A0
    cmpa.l  4(A2),A0
    cmpa.l  6(A2,D1.w),A0
    cmpa.l  $7000.w,A0
    cmpa.l  $8000.l,A0
    cmpa.l  cmpa_l_test+2(PC),A0
    cmpa.l  cmpa_l_test(PC,D1.l),A0
    cmpa.l  #$12345678,A0
    
    move.w  #$FFFF,-(SP)
    rtr

    ; SUBQ tests
    ;-----------
subq_test:
    lea     $7000.w,A0
    lea     $8000.l,A1
    suba.l  A2,A2
    moveq   #0,D0
    moveq   #2,D1
    
    clr.b   (A0)
    clr.b   (A1)
    subq.b  #8,D0
    subq.b  #1,(A0)
    subq.b  #7,(A1)+
    subq.b  #8,-1(A1)
    subq.b  #8,-3(A1,D1.w)
    subq.b  #8,-(A1)
    subq.b  #1,$7000.w
    subq.b  #1,$8000.l
    move.b  (A0),D0
    move.b  (A1),D0

    clr.w   (A0)
    move.w  (A0),D0
    clr.w   (A1)
    subq.w  #8,D0
    subq.w  #8,A2
    subq.w  #1,(A0)
    move.w  (A0),D0
    subq.w  #7,(A1)+
    subq.w  #8,-2(A1)
    subq.w  #8,-4(A1,D1.w)
    subq.w  #8,-(A1)
    subq.w  #1,$7000.w
    subq.w  #1,$8000.l
    move.w  (A0),D0
    move.w  (A1),D0

    clr.l   (A0)
    clr.l   (A1)
    subq.l  #8,D0
    subq.l  #8,A2
    subq.l  #1,(A0)
    subq.l  #7,(A1)+
    subq.l  #8,-4(A1)
    subq.l  #8,-6(A1,D1.w)
    subq.l  #8,-(A1)
    subq.l  #1,$7000.w
    subq.l  #1,$8000.l
    move.l  (A0),D0
    move.l  (A1),D0

    rts

    ; SUBA.W tests
    ;-------------
suba_w_test:
    lea     $7FFF.w,A0
    lea     $7FFF.w,A1
    lea     $8000.l,A2
    move.l  #$12345678,(A2)
    moveq   #2,D1
    suba.w  A1,A0
    suba.w  D1,A0
    suba.w  (A2)+,A0
    suba.w  (A2),A0
    suba.w  -(A2),A0
    suba.w  2(A2),A0
    suba.w  6(A2,D1.w),A0
    suba.w  $7000.w,A0
    suba.w  $8000.l,A0
    suba.w  suba_w_test+2(PC),A0
    suba.w  suba_w_test(PC,D1.l),A0
    suba.w  #$1234,A0
    rts

    ; SUBA.L tests
    ;-------------
suba_l_test:
    lea     $7FFFFFFF.l,A0
    lea     $7FFFFFFF.l,A1
    lea     $8000.l,A2
    move.l  #$12345678,(A2)
    move.l  #$9ABCDEF0,4(A2)
    moveq   #2,D1
    suba.l  A1,A0
    suba.l  D1,A0
    suba.l  (A2)+,A0
    suba.l  (A2),A0
    suba.l  -(A2),A0
    suba.l  4(A2),A0
    suba.l  6(A2,D1.w),A0
    suba.l  $7000.w,A0
    suba.l  $8000.l,A0
    suba.l  suba_l_test+2(PC),A0
    suba.l  suba_l_test(PC,D1.l),A0
    suba.l  #$12345678,A0
    rts

    ; SUBI.B tests
    ;-------------
subi_b_test:
    lea     $8000.l,A0
    move.w  #$55AA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #4,A0
    moveq   #0,D0
    moveq   #4,D1

    move.b  #$7F,D0
    subi.b  #$7F,D0
    subq.b  #1,D0
    move.b  #$7F,D1
    moveq   #0,D2
    subx.b  D2,D1
    subi.b  #$12,(A0)+
    subi.b  #$23,(A0)
    subi.b  #$45,-(A0)
    subi.b  #$56,1(A0)
    subi.b  #$78,-3(A0,D1.w)
    subi.b  #$9A,$7000.w
    subi.b  #$BC,$8000.l
    
    rts
    
    ; SUBI.W tests
    ;-------------
subi_w_test:
    lea     $8000.l,A0
    move.w  #$5555,(A0)+
    move.w  #$AAAA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #6,A0
    moveq   #0,D0
    moveq   #8,D1

    move.w  #$7FFF,D0
    subi.w  #$7FFF,D0
    subq.w  #1,D0
    move.w  #$7FFF,D1
    moveq   #0,D2
    subx.w  D2,D1
    subi.w  #$1212,(A0)+
    subi.w  #$2323,(A0)
    subi.w  #$4545,-(A0)
    subi.w  #$5656,2(A0)
    subi.w  #$7878,-6(A0,D1.w)
    subi.w  #$9A9A,$7000.w
    subi.w  #$BCBC,$8000.l
    
    rts
    
    ; SUBI.L tests
    ;-------------
subi_l_test:
    lea     $8000.l,A0
    move.l  #$55555555,(A0)+
    move.l  #$AAAAAAAA,(A0)+
    move.l  #$12345678,(A0)+
    move.l  #$9ABCDEF0,(A0)+
    move.l  #$FFFFFFFF,(A0)
    lea     -16(A0),A0
    moveq   #16,D1

    move.l  #$7FFFFFFF,D0
    subi.l  #$7FFFFFFF,D0
    subq.l  #1,D0
    move.l  #$7FFFFFFF,D1
    moveq   #0,D2
    subx.l  D2,D1
    subi.l  #$12121212,(A0)+
    subi.l  #$23232323,(A0)
    subi.l  #$45454545,-(A0)
    subi.l  #$56565656,4(A0)
    subi.l  #$78787878,-12(A0,D1.w)
    subi.l  #$9A9A9A9A,$7000.w
    subi.l  #$BCBCBCBC,$8000.l
    
    rts
    
     ; CMPI.B tests
    ;-------------
cmpi_b_test:
    lea     $8000.l,A0
    move.w  #$55AA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #4,A0
    moveq   #0,D0
    moveq   #4,D1

    move.b  #$7F,D0
    cmpi.b  #$7F,D0
    cmpi.b  #$12,(A0)+
    cmpi.b  #$23,(A0)
    cmpi.b  #$45,-(A0)
    cmpi.b  #$56,1(A0)
    cmpi.b  #$78,-3(A0,D1.w)
    cmpi.b  #$9A,$7000.w
    cmpi.b  #$BC,$8000.l
    
    rts
    
    ; CMPI.W tests
    ;-------------
cmpi_w_test:
    lea     $8000.l,A0
    move.w  #$5555,(A0)+
    move.w  #$AAAA,(A0)+
    move.w  #$1234,(A0)+
    move.w  #$5678,(A0)
    subq.l  #6,A0
    moveq   #0,D0
    moveq   #8,D1

    move.w  #$7FFF,D0
    cmpi.w  #$7FFF,D0
    cmpi.w  #$1212,(A0)+
    cmpi.w  #$2323,(A0)
    cmpi.w  #$4545,-(A0)
    cmpi.w  #$5656,2(A0)
    cmpi.w  #$7878,-6(A0,D1.w)
    cmpi.w  #$9A9A,$7000.w
    cmpi.w  #$BCBC,$8000.l
    
    rts
    
    ; CMPI.L tests
    ;-------------
cmpi_l_test:
    lea     $8000.l,A0
    move.l  #$55555555,(A0)+
    move.l  #$AAAAAAAA,(A0)+
    move.l  #$12345678,(A0)+
    move.l  #$9ABCDEF0,(A0)+
    move.l  #$FFFFFFFF,(A0)
    lea     -16(A0),A0
    moveq   #16,D1

    move.l  #$7FFFFFFF,D0
    cmpi.l  #$7FFFFFFF,D0
    cmpi.l  #$12121212,(A0)+
    cmpi.l  #$23232323,(A0)
    cmpi.l  #$45454545,-(A0)
    cmpi.l  #$56565656,4(A0)
    cmpi.l  #$78787878,-12(A0,D1.w)
    cmpi.l  #$9A9A9A9A,$7000.w
    cmpi.l  #$BCBCBCBC,$8000.l
    
    rts
    
   ; CLR and TST tests
    ;------------------
clr_tst_test:
    lea     $8000.l,A0
    moveq   #1,D1
    move.b  #$55,D0
    clr.b   D0
    tst.b   D0
    move.b  #$55,(A0)
    clr.b   (A0)
    tst.b   (A0)
    move.b  #$55,(A0)
    clr.b   (A0)+
    tst.b   -(A0)
    move.b  #$55,(A0)+
    clr.b   -(A0)
    tst.b   (A0)+
    move.b  #$55,1(A0)
    clr.b   1(A0)
    tst.b   1(A0)
    move.b  #$55,1(A0,D1.w)
    clr.b   1(A0,D1.w)
    tst.b   1(A0,D1.w)
    move.b  #$55,$7000.w
    clr.b   $7000.w
    tst.b   $7000.w
    move.b  #$55,$8000.l
    clr.b   $8000.l
    tst.b   $8000.l

    lea     $8000.l,A0
    moveq   #2,D1
    move.w  #$5555,D0
    clr.w   D0
    tst.w   D0
    move.w  #$5555,(A0)
    clr.w   (A0)
    tst.w   (A0)
    move.w  #$5555,(A0)
    clr.w   (A0)+
    tst.w   -(A0)
    move.w  #$5555,(A0)+
    clr.w   -(A0)
    tst.w   (A0)+
    move.w  #$5555,2(A0)
    clr.w   2(A0)
    tst.w   2(A0)
    move.w  #$5555,2(A0,D1.w)
    clr.w   2(A0,D1.w)
    tst.w   2(A0,D1.w)
    move.w  #$55,$7000.w
    clr.w   $7000.w
    tst.w   $7000.w
    move.w  #$5555,$8000.l
    clr.w   $8000.l
    tst.w   $8000.l

    lea     $8000.l,A0
    moveq   #4,D1
    move.l  #$55555555,D0
    clr.l   D0
    tst.l   D0
    move.l  #$55555555,(A0)
    clr.l   (A0)
    tst.l   (A0)
    move.l  #$55555555,(A0)
    clr.l   (A0)+
    tst.l   -(A0)
    move.l  #$55555555,(A0)+
    clr.l   -(A0)
    tst.l   (A0)+
    move.l  #$55555555,4(A0)
    clr.l   4(A0)
    tst.l   4(A0)
    move.l  #$55555555,4(A0,D1.w)
    clr.l   4(A0,D1.w)
    tst.l   4(A0,D1.w)
    move.l  #$55555555,$7000.w
    clr.l   $7000.w
    tst.l   $7000.w
    move.l  #$55555555,$8000.l
    clr.l   $8000.l
    tst.l   $8000.l

    rts

    ; NEG and NEGX tests
    ;-------------------
neg_test:
    lea     $8000.l,A0
    
    move.l  #$12345678,(A0)
    move.b  #$89,-$1000(A0)
    moveq   #-128,D0
    moveq   #127,D1
    moveq   #0,D2
    moveq   #1,D3
    
    neg.b   D0
    negx.b   D1
    negx.b   D2
    neg.b   (A0)
    move.b  (A0),D0
    negx.b  1(A0)
    move.b  1(A0),D0
    neg.b   (A0)+
    move.b  (A0),D0
    negx.b  (A0)+
    move.b  (A0),D0
    neg.b   -2(A0)
    move.b  -2(A0),D0
    negx.b  -1(A0)
    move.b  -1(A0),D0
    neg.b   -(A0)
    move.b  (A0),D0
    negx.b  -(A0)
    move.b  (A0),D0
    neg.b   1(A0,D3)
    move.b  1(A0,D3),D0
    negx.b  2(A0,D3)
    move.b  2(A0,D3),D0
    neg.b   $7000.w
    move.b  $7000.w,D0
    negx.b  $7001.w
    move.b  $7001.w,D0
    neg.b   $8000.l
    move.b  $8000.l,D0
    negx.b  $8001.l
    move.b  $8001.l,D0
    
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    move.w  #$89AB,-$1000(A0)
    move.w  #$8000,D0
    move.w  #$7FFF,D1
    moveq   #0,D2
    moveq   #2,D3
    
    neg.w   D0
    negx.w  D1
    negx.w  D2
    neg.w   (A0)
    move.w  (A0),D0
    negx.w  2(A0)
    move.w  2(A0),D0
    neg.w   (A0)+
    move.w  (A0),D0
    negx.w  (A0)+
    move.w  (A0),D0
    neg.w   -4(A0)
    move.w  -4(A0),D0
    negx.w  -2(A0)
    move.w  -2(A0),D0
    neg.w   -(A0)
    move.w  (A0),D0
    negx.w  -(A0)
    move.w  (A0),D0
    neg.w   2(A0,D3)
    move.w  2(A0,D3),D0
    negx.w  4(A0,D3)
    move.w  4(A0,D3),D0
    neg.w   $7000.w
    move.w  $7000.w,D0
    negx.w  $7002.w
    move.w  $7002.w,D0
    neg.w   $8000.l
    move.w  $8000.l,D0
    negx.w  $8002.l
    move.w  $8002.l,D0
    
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    move.l  #$12345678,8(A0)
    move.l  #$9ABCDEF0,12(A0)
    move.l  #$01234567,-$1000(A0)
    move.l  #$89ABCDEF,-$0FFC(A0)
    move.l  #$80000000,D0
    move.l  #$7FFFFFFF,D1
    moveq   #0,D2
    moveq   #4,D3
    
    neg.l   D0
    negx.l   D1
    negx.l   D2
    neg.l   (A0)
    move.l  (A0),D0
    negx.l  4(A0)
    move.l  4(A0),D0
    neg.l   (A0)+
    move.l  (A0),D0
    negx.l  (A0)+
    move.l  (A0),D0
    neg.l   -8(A0)
    move.l  -8(A0),D0
    negx.l  -4(A0)
    move.l  -4(A0),D0
    neg.l   -(A0)
    move.l  (A0),D0
    negx.l  -(A0)
    move.l  (A0),D0
    neg.l   0(A0,D3)
    move.l  0(A0,D3),D0
    negx.l  4(A0,D3)
    move.l  4(A0,D3),D0
    neg.l   $7000.w
    move.l  $7000.w,D0
    negx.l  $7004.w
    move.l  $7004.w,D0
    neg.l   $8000.l
    move.l  $8000.l,D0
    negx.l  $8004.l
    move.l  $8004.l,D0
    
    rts
    
    ; NOT tests
    ;----------
not_test:
    lea     $8000.l,A0
    
    move.l  #$12345678,(A0)
    move.b  #$89,-$1000(A0)
    moveq   #-128,D0
    moveq   #127,D1
    moveq   #0,D2
    moveq   #1,D3
    not.b   D0
    not.b   D1
    not.b   D2
    not.b   (A0)
    move.b  (A0),D0
    not.b   (A0)+
    move.b  (A0),D0
    not.b   -1(A0)
    move.b  -1(A0),D0
    not.b   -(A0)
    move.b  (A0),D0
    not.b   1(A0,D3)
    move.b  1(A0,D3),D0
    not.b   $7000.w
    move.b  $7000.w,D0
    not.b   $8000.l
    move.b  $8000.l,D0
    
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,4(A0)
    move.w  #$89AB,-$1000(A0)
    move.w  #$8000,D0
    move.w  #$7FFF,D1
    moveq   #0,D2
    moveq   #2,D3
    not.w   D0
    not.w   D1
    not.w   D2
    not.w   (A0)
    move.w  (A0),D0
    not.w   (A0)+
    move.w  (A0),D0
    not.w   -2(A0)
    move.w  -2(A0),D0
    not.w   -(A0)
    move.w  (A0),D0
    not.w   2(A0,D3)
    move.w  2(A0,D3),D0
    not.w   $7000.w
    move.w  $7000.w,D0
    not.w   $8000.l
    move.w  $8000.l,D0
    
    move.l  #$12345678,(A0)
    move.l  #$9ABCDEF0,8(A0)
    move.l  #$89ABCDEF,-$1000(A0)
    move.l  #$80000000,D0
    move.l  #$7FFFFFFF,D1
    moveq   #0,D2
    moveq   #4,D3
    not.l   D0
    not.l   D1
    not.l   D2
    not.l   (A0)
    move.l  (A0),D0
    not.l   (A0)+
    move.l  (A0),D0
    not.l   -4(A0)
    move.l  -4(A0),D0
    not.l   -(A0)
    move.l  (A0),D0
    not.l   4(A0,D3)
    move.l  4(A0,D3),D0
    not.l   $7000.w
    move.l  $7000.w,D0
    not.l   $8000.l
    move.l  $8000.l,D0
    
    rts

    ; MULU, MULS tests
    ;-----------------
mult_test:
    move.w  #$5555,D0
    move.w  #$AAAA,D1
    lea     $8000.l,A0
    moveq   #2,D2    
    move.w  D0,(A0)
    move.w  D1,4(A0)
    move.w  D1,-$1000(A0)
    
    mulu.w  D1,D0
    mulu.w  (A0),D0
    mulu.w  (A0)+,D0
    mulu.w  -2(A0),D0
    mulu.w  -(A0),D0
    mulu.w  2(A0,D2.w),D0
    mulu.w  $7000.w,D0
    mulu.w  $8000.l,D0
    mulu.w  mult_test+2(PC),D0
    mulu.w  mult_test+4(PC,D2.w),D0
    mulu.w  #$1234,D0
    
    muls.w  D1,D0
    muls.w  (A0),D0
    muls.w  (A0)+,D0
    muls.w  -2(A0),D0
    muls.w  -(A0),D0
    muls.w  2(A0,D2.w),D0
    muls.w  $7000.w,D0
    muls.w  $8000.l,D0
    muls.w  mult_test+2(PC),D0
    muls.w  mult_test+4(PC,D2.w),D0
    muls.w  #$1234,D0
    
    rts

    ; DIVU, DIVS tests
    ;-----------------
div_test:
    move.l  D0,D0
    move.w  #$AAAA,D1
    move.l  #$12345678,D2
    lea     $8000.l,A0
    moveq   #2,D3
    move.w  D1,(A0)
    move.w  D2,4(A0)
    move.w  D1,-$1000(A0)
    
    move.l  D2,D0
    divu    div_test+2(PC),D0
    move.l  D2,D0
    divu    div_test+4(PC,D3.w),D0
    
    move.l  D2,D0
    divs    div_test+2(PC),D0
    move.l  D2,D0
    divs    div_test+4(PC,D3.w),D0
    
    move.l  D2,D0
    divu    D1,D0
    move.l  D2,D0
    divu    (A0),D0
    move.l  D2,D0
    divu    (A0)+,D0
    move.l  D2,D0
    divu    -2(A0),D0
    move.l  D2,D0
    divu    -(A0),D0
    move.l  D2,D0
    divu    2(A0,D3.w),D0
    move.l  D2,D0
    divu    $7000.w,D0
    move.l  D2,D0
    divu    $8000.l,D0
    move.l  D2,D0
    divu    #$1234,D0
    
    move.l  D2,D0
    divs    D1,D0
    move.l  D2,D0
    divs    (A0),D0
    move.l  D2,D0
    divs    (A0)+,D0
    move.l  D2,D0
    divs    -2(A0),D0
    move.l  D2,D0
    divs    -(A0),D0
    move.l  D2,D0
    divs    2(A0,D3.w),D0
    move.l  D2,D0
    divs    $7000.w,D0
    move.l  D2,D0
    divs    $8000.l,D0
    move.l  D2,D0
    divs    #$1234,D0
    
    move.l  #$12345678,D0
    divu    #$1234,D0
    move.l  #$55555555,D0
    divu    #$AAAA,D0
    move.l  #$AAAAAAAA,D0
    divu    #$5555,D0
    move.l  #$2AAAAAAA,D0
    divu    #$5555,D0
    move.l  #$12345678,D0
    divs    #$1235,D0
    move.l  #$12345678,D0
    divs    #$1234,D0
    move.l  #$55555555,D0
    divs    #$AAAA,D0
    move.l  #$AAAAAAAA,D0
    divs    #$5555,D0
    move.l  #$2AAAAAAA,D0
    divs    #$5555,D0
    rts

    ; MOVE.B tests
    ;-------------
move_b_test_0:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,D3
    move.b  D1,D3
    move.b  (A0),D3
    move.b  (A0)+,D3
    move.b  -(A0),D3
    move.b  1(A0),D3
    move.b  -3(A0,D2.w),D3
    move.b  $7000.w,D3
    move.b  $8000.l,D3
    move.b  move_b_test_0(PC),D3
    move.b  move_b_test_0(PC,D2.w),D3
    move.b  #$12,D3
    
    rts
    
move_b_test_2:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,(A1)
    move.b  D1,(A1)
    move.b  (A0),(A1)
    move.b  (A0)+,(A1)
    move.b  -(A0),(A1)
    move.b  1(A0),(A1)
    move.b  -3(A0,D2.w),(A1)
    move.b  $7000.w,(A1)
    move.b  $8000.l,(A1)
    move.b  move_b_test_2(PC),(A1)
    move.b  move_b_test_2(PC,D2.w),(A1)
    move.b  #$12,(A1)
    
    rts
 
move_b_test_3:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,(A1)+
    move.b  D1,(A1)+
    move.b  (A0),(A1)+
    move.b  (A0)+,(A1)+
    move.b  -(A0),(A1)+
    move.b  1(A0),(A1)+
    move.b  -3(A0,D2.w),(A1)+
    move.b  $7000.w,(A1)+
    move.b  $8000.l,(A1)+
    move.b  move_b_test_3(PC),(A1)+
    move.b  move_b_test_3(PC,D2.w),(A1)+
    move.b  #$12,(A1)+
    
    rts
 
move_b_test_4:
    lea     $8000.l,A0
    lea     12(A0),A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,-(A1)
    move.b  D1,-(A1)
    move.b  (A0),-(A1)
    move.b  (A0)+,-(A1)
    move.b  -(A0),-(A1)
    move.b  1(A0),-(A1)
    move.b  -3(A0,D2.w),-(A1)
    move.b  $7000.w,-(A1)
    move.b  $8000.l,-(A1)
    move.b  move_b_test_4(PC),-(A1)
    move.b  move_b_test_4(PC,D2.w),-(A1)
    move.b  #$12,-(A1)
    
    rts
 
move_b_test_5:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,1(A1)
    move.b  D1,2(A1)
    move.b  (A0),3(A1)
    move.b  (A0)+,4(A1)
    move.b  -(A0),5(A1)
    move.b  1(A0),6(A1)
    move.b  -3(A0,D2.w),7(A1)
    move.b  $7000.w,8(A1)
    move.b  $8000.l,9(A1)
    move.b  move_b_test_5(PC),10(A1)
    move.b  move_b_test_5(PC,D2.w),11(A1)
    move.b  #$12,12(A1)
    
    rts

move_b_test_6:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,1(A1,D2.w)
    move.b  D1,2(A1,D2.w)
    move.b  (A0),3(A1,D2.w)
    move.b  (A0)+,4(A1,D2.w)
    move.b  -(A0),5(A1,D2.w)
    move.b  1(A0),6(A1,D2.w)
    move.b  -3(A0,D2.w),7(A1,D2.w)
    move.b  $7000.w,8(A1,D2.w)
    move.b  $8000.l,9(A1,D2.w)
    move.b  move_b_test_6(PC),10(A1,D2.w)
    move.b  move_b_test_6(PC,D2.w),11(A1,D2.w)
    move.b  #$12,12(A1,D2.w)
    
    rts

move_b_test_7:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,$7001.w
    move.b  D1,$7002.w
    move.b  (A0),$7003.w
    move.b  (A0)+,$7004.w
    move.b  -(A0),$7005.w
    move.b  1(A0),$7006.w
    move.b  -3(A0,D2.w),$7007.w
    move.b  $7000.w,$7008.w
    move.b  $8000.l,$7009.w
    move.b  move_b_test_7(PC),$700A.w
    move.b  move_b_test_7(PC,D2.w),$700B.w
    move.b  #$12,$700C.w
    
    rts

move_b_test_8:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #2,D2
    move.b  D0,$8001.l
    move.b  D1,$8002.l
    move.b  (A0),$8003.l
    move.b  (A0)+,$8004.l
    move.b  -(A0),$8005.l
    move.b  1(A0),$8006.l
    move.b  -3(A0,D2.w),$8007.l
    move.b  $7000.w,$8008.l
    move.b  $8000.l,$8009.l
    move.b  move_b_test_8(PC),$800A.l
    move.b  move_b_test_8(PC,D2.w),$800B.l
    move.b  #$12,$800C.l
    
    rts

    ; MOVE.W tests
    ;-------------
move_w_test_0:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,D3
    move.w  D1,D3
    move.w  A0,D3
    move.w  (A0),D3
    move.w  (A0)+,D3
    move.w  -(A0),D3
    move.w  2(A0),D3
    move.w  -4(A0,D2.w),D3
    move.w  $7000.w,D3
    move.w  $8000.l,D3
    move.w  move_w_test_0(PC),D3
    move.w  move_w_test_0(PC,D2.w),D3
    move.w  #$1234,D3
    
    rts
    
move_w_test_1:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    movea.w D0,A3
    movea.w D1,A3
    movea.w A0,A3
    movea.w (A0),A3
    movea.w (A0)+,A3
    movea.w -(A0),A3
    movea.w 2(A0),A3
    movea.w -4(A0,D2.w),A3
    movea.w $7000.w,A3
    movea.w $8000.l,A3
    movea.w move_w_test_0(PC),A3
    movea.w move_w_test_0(PC,D2.w),A3
    movea.w #$1234,A3
    
    rts
    
move_w_test_2:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,(A1)
    move.w  D1,(A1)
    move.w  A0,(A1)
    move.w  (A0),(A1)
    move.w  (A0)+,(A1)
    move.w  -(A0),(A1)
    move.w  2(A0),(A1)
    move.w  -4(A0,D2.w),(A1)
    move.w  $7000.w,(A1)
    move.w  $8000.l,(A1)
    move.w  move_w_test_2(PC),(A1)
    move.w  move_w_test_2(PC,D2.w),(A1)
    move.w  #$1234,(A1)
    
    rts
 
move_w_test_3:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,(A1)+
    move.w  D1,(A1)+
    move.w  A0,(A1)+
    move.w  (A0),(A1)+
    move.w  (A0)+,(A1)+
    move.w  -(A0),(A1)+
    move.w  2(A0),(A1)+
    move.w  -4(A0,D2.w),(A1)+
    move.w  $7000.w,(A1)+
    move.w  $8000.l,(A1)+
    move.w  move_w_test_3(PC),(A1)+
    move.w  move_w_test_3(PC,D2.w),(A1)+
    move.w  #$1234,(A1)+
    
    rts
 
move_w_test_4:
    lea     $8000.l,A0
    lea     26(A0),A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,-(A1)
    move.w  D1,-(A1)
    move.w  A0,-(A1)
    move.w  (A0),-(A1)
    move.w  (A0)+,-(A1)
    move.w  -(A0),-(A1)
    move.w  2(A0),-(A1)
    move.w  -4(A0,D2.w),-(A1)
    move.w  $7000.w,-(A1)
    move.w  $8000.l,-(A1)
    move.w  move_w_test_4(PC),-(A1)
    move.w  move_w_test_4(PC,D2.w),-(A1)
    move.w  #$1234,-(A1)
    
    rts
 
move_w_test_5:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,2(A1)
    move.w  D1,4(A1)
    move.w  A0,6(A1)
    move.w  (A0),8(A1)
    move.w  (A0)+,10(A1)
    move.w  -(A0),12(A1)
    move.w  2(A0),14(A1)
    move.w  -4(A0,D2.w),16(A1)
    move.w  $7000.w,18(A1)
    move.w  $8000.l,20(A1)
    move.w  move_w_test_5(PC),22(A1)
    move.w  move_w_test_5(PC,D2.w),24(A1)
    move.w  #$1234,26(A1)
    
    rts

move_w_test_6:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,2(A1,D2.w)
    move.w  D1,4(A1,D2.w)
    move.w  A0,6(A1,D2.w)
    move.w  (A0),8(A1,D2.w)
    move.w  (A0)+,10(A1,D2.w)
    move.w  -(A0),12(A1,D2.w)
    move.w  2(A0),14(A1,D2.w)
    move.w  -4(A0,D2.w),16(A1,D2.w)
    move.w  $7000.w,18(A1,D2.w)
    move.w  $8000.l,20(A1,D2.w)
    move.w  move_w_test_6(PC),22(A1,D2.w)
    move.w  move_w_test_6(PC,D2.w),24(A1,D2.w)
    move.w  #$1234,26(A1,D2.w)
    
    rts

move_w_test_7:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,$7002.w
    move.w  D1,$7004.w
    move.w  A0,$7006.w
    move.w  (A0),$7008.w
    move.w  (A0)+,$700A.w
    move.w  -(A0),$700C.w
    move.w  2(A0),$700E.w
    move.w  -4(A0,D2.w),$7010.w
    move.w  $7000.w,$7012.w
    move.w  $8000.l,$7014.w
    move.w  move_w_test_7(PC),$7016.w
    move.w  move_w_test_7(PC,D2.w),$7018.w
    move.w  #$1234,$701A.w
    
    rts
    
move_w_test_8:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #6,D2
    move.w  D0,$8002.l
    move.w  D1,$8004.l
    move.w  A0,$8006.l
    move.w  (A0),$8008.l
    move.w  (A0)+,$800A.l
    move.w  -(A0),$800C.l
    move.w  2(A0),$800E.l
    move.w  -4(A0,D2.w),$8010.l
    move.w  $7000.w,$8012.l
    move.w  $8000.l,$8014.l
    move.w  move_w_test_8(PC),$8016.l
    move.w  move_w_test_8(PC,D2.w),$8018.l
    move.w  #$1234,$801A.l
    
    rts
    
    ; MOVE.L tests
    ;-------------
move_l_test_0:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,D3
    move.l  D1,D3
    move.l  A0,D3
    move.l  (A0),D3
    move.l  (A0)+,D3
    move.l  -(A0),D3
    move.l  4(A0),D3
    move.l  -6(A0,D2.w),D3
    move.l  $7000.w,D3
    move.l  $8000.l,D3
    move.l  move_l_test_0(PC),D3
    move.l  move_l_test_0(PC,D2.w),D3
    move.l  #$12345678,D3
    
    rts
    
move_l_test_1:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    movea.l D0,A3
    movea.l D1,A3
    movea.l A0,A3
    movea.l (A0),A3
    movea.l (A0)+,A3
    movea.l -(A0),A3
    movea.l 4(A0),A3
    movea.l -6(A0,D2.w),A3
    movea.l $7000.w,A3
    movea.l $8000.l,A3
    movea.l move_l_test_0(PC),A3
    movea.l move_l_test_0(PC,D2.w),A3
    movea.l #$12345678,A3
    
    rts
    
move_l_test_2:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,(A1)
    move.l  D1,(A1)
    move.l  A0,(A1)
    move.l  (A0),(A1)
    move.l  (A0)+,(A1)
    move.l  -(A0),(A1)
    move.l  4(A0),(A1)
    move.l  -6(A0,D2.w),(A1)
    move.l  $7000.w,(A1)
    move.l  $8000.l,(A1)
    move.l  move_l_test_2(PC),(A1)
    move.l  move_l_test_2(PC,D2.w),(A1)
    move.l  #$12345678,(A1)
    
    rts
 
move_l_test_3:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,(A1)+
    move.l  D1,(A1)+
    move.l  A0,(A1)+
    move.l  (A0),(A1)+
    move.l  (A0)+,(A1)+
    move.l  -(A0),(A1)+
    move.l  4(A0),(A1)+
    move.l  -6(A0,D2.w),(A1)+
    move.l  $7000.w,(A1)+
    move.l  $8000.l,(A1)+
    move.l  move_l_test_3(PC),(A1)+
    move.l  move_l_test_3(PC,D2.w),(A1)+
    move.l  #$12345678,(A1)+
    
    rts
 
move_l_test_4:
    lea     $8000.l,A0
    lea     52(A0),A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,-(A1)
    move.l  D1,-(A1)
    move.l  A0,-(A1)
    move.l  (A0),-(A1)
    move.l  (A0)+,-(A1)
    move.l  -(A0),-(A1)
    move.l  4(A0),-(A1)
    move.l  -6(A0,D2.w),-(A1)
    move.l  $7000.w,-(A1)
    move.l  $8000.l,-(A1)
    move.l  move_l_test_4(PC),-(A1)
    move.l  move_l_test_4(PC,D2.w),-(A1)
    move.l  #$12345678,-(A1)
    
    rts
 
move_l_test_5:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,4(A1)
    move.l  D1,8(A1)
    move.l  A0,12(A1)
    move.l  (A0),16(A1)
    move.l  (A0)+,20(A1)
    move.l  -(A0),24(A1)
    move.l  4(A0),28(A1)
    move.l  -6(A0,D2.w),32(A1)
    move.l  $7000.w,36(A1)
    move.l  $8000.l,40(A1)
    move.l  move_l_test_5(PC),44(A1)
    move.l  move_l_test_5(PC,D2.w),48(A1)
    move.l  #$12345678,52(A1)
    
    rts

move_l_test_6:
    lea     $8000.l,A0
    move.l  A0,A1
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,4(A1,D2.w)
    move.l  D1,8(A1,D2.w)
    move.l  A0,12(A1,D2.w)
    move.l  (A0),16(A1,D2.w)
    move.l  (A0)+,20(A1,D2.w)
    move.l  -(A0),24(A1,D2.w)
    move.l  4(A0),28(A1,D2.w)
    move.l  -6(A0,D2.w),32(A1,D2.w)
    move.l  $7000.w,36(A1,D2.w)
    move.l  $8000.l,40(A1,D2.w)
    move.l  move_l_test_6(PC),44(A1,D2.w)
    move.l  move_l_test_6(PC,D2.w),48(A1,D2.w)
    move.l  #$12345678,52(A1,D2.w)
    
    rts

move_l_test_7:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,$7002.w
    move.l  D1,$7004.w
    move.l  A0,$7006.w
    move.l  (A0),$7008.w
    move.l  (A0)+,$700A.w
    move.l  -(A0),$700C.w
    move.l  4(A0),$700E.w
    move.l  -6(A0,D2.w),$7010.w
    move.l  $7000.w,$7012.w
    move.l  $8000.l,$7014.w
    move.l  move_l_test_7(PC),$7016.w
    move.l  move_l_test_7(PC,D2.w),$7018.w
    move.l  #$12345678,$701A.w
    
    rts
    
move_l_test_8:
    lea     $8000.l,A0
    moveq   #0,D0
    moveq   #-128,D1
    moveq   #8,D2
    move.l  D0,$8002.l
    move.l  D1,$8004.l
    move.l  A0,$8006.l
    move.l  (A0),$8008.l
    move.l  (A0)+,$800A.l
    move.l  -(A0),$800C.l
    move.l  4(A0),$800E.l
    move.l  -6(A0,D2.w),$8010.l
    move.l  $7000.w,$8012.l
    move.l  $8000.l,$8014.l
    move.l  move_l_test_8(PC),$8016.l
    move.l  move_l_test_8(PC,D2.w),$8018.l
    move.l  #$12345678,$801A.l
    
    rts

movem_test:
    moveq   #-1,D0
    moveq   #-2,D1
    moveq   #-3,D2
    moveq   #-4,D3
    moveq   #0,D4
    moveq   #1,D5
    moveq   #2,D6
    moveq   #3,D7

    movem.l D0-D7,-(SP)
    movem.l 16(SP),A0-A3
    movem.l D0-D7,(SP)
    movem.l (SP),A0-A3
    movem.l 16(SP),A0-A3
    movem.l D4-D7,16(SP)
    movem.l 16(SP),A0-A3
    movem.l D0-D3,16(SP,D4)
    movem.l 16(SP),A0-A3
    movem.l (SP)+,D0-D7
    
    moveq   #-1,D0
    moveq   #-2,D1
    moveq   #-3,D2
    moveq   #-4,D3
    moveq   #0,D4
    moveq   #1,D5
    moveq   #2,D6
    moveq   #3,D7

    movem.w D0-D7,-(SP)
    movem.w 8(SP),A0-A3
    movem.w D0-D7,(SP)
    movem.w (SP),A0-A3
    movem.w 8(SP),A0-A3
    movem.w D4-D7,8(SP)
    movem.w 8(SP),A0-A3
    movem.w D0-D3,8(SP,D4)
    movem.w 8(SP),A0-A3
    movem.w (SP)+,D0-D7
    
    rts
    
    ; Shifts immediate tests
    ;-----------------------
shifti_test:
    moveq   #$55,D0
    asr.b   #3,D0
    moveq   #-1,D0
    lsr.b   #3,D0
    moveq   #-10,D0
    ror.b   #8,D0
    moveq   #-128,D0
    roxr.b  #3,D0
    
    moveq   #$55,D0
    asr.w   #3,D0
    moveq   #-1,D0
    lsr.w   #3,D0
    moveq   #-10,D0
    ror.w   #8,D0
    moveq   #-128,D0
    roxr.w  #3,D0
    
    moveq   #$55,D0
    asr.l   #3,D0
    moveq   #-1,D0
    lsr.l   #3,D0
    moveq   #-10,D0
    ror.l   #8,D0
    moveq   #-128,D0
    roxr.l  #3,D0

    moveq   #$55,D0
    asl.b   #3,D0
    moveq   #-1,D0
    lsl.b   #3,D0
    moveq   #-10,D0
    rol.b   #8,D0
    moveq   #-128,D0
    roxl.b  #3,D0
    
    moveq   #$55,D0
    asl.w   #3,D0
    moveq   #-1,D0
    lsl.w   #3,D0
    moveq   #-10,D0
    rol.w   #8,D0
    moveq   #-128,D0
    roxl.w  #3,D0
    
    moveq   #$55,D0
    asl.l   #3,D0
    moveq   #-1,D0
    lsl.l   #3,D0
    moveq   #-10,D0
    rol.l   #8,D0
    moveq   #-128,D0
    roxl.l  #3,D0
    rts

    ; Shifts register tests
    ;----------------------
shiftr_test:
    moveq   #$55,D0
    moveq   #1,D1
    asr.b   D1,D0
    moveq   #-1,D0
    moveq   #3,D1
    lsr.b   D1,D0
    moveq   #-10,D0
    moveq   #5,D1
    ror.b   D1,D0
    moveq   #-128,D0
    moveq   #7,D1
    roxr.b  D1,D0
    
    moveq   #$55,D0
    moveq   #9,D1
    asr.w   D1,D0
    moveq   #-1,D0
    moveq   #11,D1
    lsr.w   D1,D0
    moveq   #-10,D0
    moveq   #13,D1
    ror.w   D1,D0
    moveq   #-128,D0
    moveq   #15,D1
    roxr.w  D1,D0
    
    moveq   #$55,D0
    moveq   #17,D1
    asr.l   D1,D0
    moveq   #-1,D0
    moveq   #19,D1
    lsr.l   D1,D0
    moveq   #-10,D0
    moveq   #21,D1
    ror.l   D1,D0
    moveq   #-128,D0
    moveq   #23,D1
    roxr.l  D1,D0

    moveq   #$55,D0
    moveq   #1,D1
    asl.b   D1,D0
    moveq   #-1,D0
    moveq   #3,D1
    lsl.b   D1,D0
    moveq   #-10,D0
    moveq   #5,D1
    rol.b   D1,D0
    moveq   #-128,D0
    moveq   #7,D1
    roxl.b  D1,D0
    
    moveq   #$55,D0
    moveq   #9,D1
    asl.w   D1,D0
    moveq   #-1,D0
    moveq   #11,D1
    lsl.w   D1,D0
    moveq   #-10,D0
    moveq   #13,D1
    rol.w   D1,D0
    moveq   #-128,D0
    moveq   #15,D1
    roxl.w  D1,D0
    
    moveq   #$55,D0
    moveq   #17,D1
    asl.l   D1,D0
    moveq   #-1,D0
    moveq   #19,D1
    lsl.l   D1,D0
    moveq   #-10,D0
    moveq   #21,D1
    rol.l   D1,D0
    moveq   #-128,D0
    moveq   #23,D1
    roxl.l  D1,D0
    rts

    ; Shifts memory tests
    ;--------------------
shiftm_test:
    lea     $8000.l,A0
    moveq   #8,D1

    move.l  #$5555AAAA,(A0)
    asr     (A0)+
    asr     (A0)
    asr     -(A0)
    asr     2(A0)
    asr     -6(A0,D1.w)
    asr     $7000.w
    asr     $8000.l

    move.l  #$5555AAAA,(A0)
    lsr     (A0)+
    lsr     (A0)
    lsr     -(A0)
    lsr     2(A0)
    lsr     -6(A0,D1.w)
    lsr     $7000.w
    lsr     $8000.l

    move.l  #$5555AAAA,(A0)
    ror     (A0)+
    ror     (A0)
    ror     -(A0)
    ror     2(A0)
    ror     -6(A0,D1.w)
    ror     $7000.w
    ror     $8000.l

    move.l  #$5555AAAA,(A0)
    roxr    (A0)+
    roxr    (A0)
    roxr    -(A0)
    roxr    2(A0)
    roxr    -6(A0,D1.w)
    roxr    $7000.w
    roxr    $8000.l

    move.l  #$5555AAAA,(A0)
    asl     (A0)+
    asl     (A0)
    asl     -(A0)
    asl     2(A0)
    asl     -6(A0,D1.w)
    asl     $7000.w
    asl     $8000.l

    move.l  #$5555AAAA,(A0)
    lsl     (A0)+
    lsl     (A0)
    lsl     -(A0)
    lsl     2(A0)
    lsl     -6(A0,D1.w)
    lsl     $7000.w
    lsl     $8000.l

    move.l  #$5555AAAA,(A0)
    rol     (A0)+
    rol     (A0)
    rol     -(A0)
    rol     2(A0)
    rol     -6(A0,D1.w)
    rol     $7000.w
    rol     $8000.l

    move.l  #$5555AAAA,(A0)
    roxl    (A0)+
    roxl    (A0)
    roxl    -(A0)
    roxl    2(A0)
    roxl    -6(A0,D1.w)
    roxl    $7000.w
    roxl    $8000.l
    
    rts
    ; DBcc tests
    ;-----------
dbcc_test:
    moveq   #3,d0
.loop1
    dbra    d0,.loop1
    
    moveq   #3,d0
.loop2
    move    #%0101,CCR
    dbhi    d0,.loop2
    
    moveq   #3,d0
.loop3
    move    #%0000,CCR
    dbhi    d0,.loop3
    
    moveq   #3,d0
.loop4
    move    #%0101,CCR
    dbls    d0,.loop4
    
    moveq   #3,d0
.loop5
    move    #%0000,CCR
    dbls    d0,.loop5
    
    moveq   #3,d0
.loop6
    move    #%0001,CCR
    dbcc    d0,.loop6
    
    moveq   #3,d0
.loop7
    move    #%0000,CCR
    dbcc    d0,.loop7
    
    moveq   #3,d0
.loop8
    move    #%0001,CCR
    dbcs    d0,.loop8
    
    moveq   #3,d0
.loop9
    move    #%0000,CCR
    dbcs    d0,.loop9
    
    moveq   #3,d0
.loop10
    move    #%0100,CCR
    dbne    d0,.loop10

    moveq   #3,d0
.loop11
    move    #%0000,CCR
    dbne    d0,.loop11

    moveq   #3,d0
.loop12
    move    #%0100,CCR
    dbeq    d0,.loop12

    moveq   #3,d0
.loop13
    move    #%0000,CCR
    dbeq    d0,.loop13

    moveq   #3,d0
.loop14
    move    #%0001,CCR
    dbvc    d0,.loop14

    moveq   #3,d0
.loop15
    move    #%0000,CCR
    dbvc    d0,.loop15

    moveq   #3,d0
.loop16
    move    #%0001,CCR
    dbvs    d0,.loop16

    moveq   #3,d0
.loop17
    move    #%0000,CCR
    dbvs    d0,.loop17

    moveq   #3,d0
.loop18
    move    #%1000,CCR
    dbpl    d0,.loop18

    moveq   #3,d0
.loop19
    move    #%0000,CCR
    dbpl    d0,.loop19

    moveq   #3,d0
.loop20
    move    #%1000,CCR
    dbmi    d0,.loop20

    moveq   #3,d0
.loop21
    move    #%0000,CCR
    dbmi    d0,.loop21

    moveq   #3,d0
.loop22
    move    #%1010,CCR
    dbge    d0,.loop22

    moveq   #3,d0
.loop23
    move    #%0000,CCR
    dbge    d0,.loop23

    moveq   #3,d0
.loop24
    move    #%1000,CCR
    dblt    d0,.loop24

    moveq   #3,d0
.loop25
    move    #%0000,CCR
    dblt    d0,.loop25

    moveq   #3,d0
.loop26
    move    #%1010,CCR
    dbgt    d0,.loop26

    moveq   #3,d0
.loop27
    move    #%0000,CCR
    dbgt    d0,.loop27

    moveq   #3,d0
.loop28
    move    #%0100,CCR
    dble    d0,.loop28

    moveq   #3,d0
.loop29
    move    #%0000,CCR
    dble    d0,.loop29

    rts
    ; Bcc tests
    ;-----------
bcc_test:
    move    #%0000,CCR
    bhi     .next0
    rts
.next0
    move    #%0001,CCR
    bls     .next1
    rts
.next1
    move    #%0000,CCR
    bcc     .next2
    rts
.next2
    move    #%0001,CCR
    bcs     .next3
    rts
.next3
    move    #%0000,CCR
    bne     .next4
    rts
.next4
    move    #%0100,CCR
    beq     .next5
    rts
.next5
    move    #%0000,CCR
    bvc     .next6
    rts
.next6
    move    #%0010,CCR
    bvs     .next7
    rts
.next7
    move    #%0000,CCR
    bpl     .next8
    rts
.next8
    move    #%1000,CCR
    bmi     .next9
    rts
.next9
    move    #%1010,CCR
    bge     .next10
    rts
.next10
    move    #%1000,CCR
    blt     .next11
    rts
.next11
    move    #%1010,CCR
    bgt     .next12
    rts
.next12
    move    #%0100,CCR
    ble     .next13
    rts
.next13
    rts
    ; Scc tests
    ;-----------
scc_test:
    lea     $8000.l,A0
    moveq   #1,D1
    
    st      d0
    
    move    #%0101,CCR
    shi     (a0)
    move.b  (a0),d0
    
    andi    #%1010,CCR  ;%0000
    shi     (a0)+
    move.b  -1(a0),d0
    
    ori     #%0101,CCR  ;%0101
    sls     -1(a0)
    move.b  -1(a0),d0
    
    eori    #%0101,CCR  ;%0000
    sls     -(a0)
    move.b  (a0),d0
    
    move    #%0001,CCR
    scc     1(a0,d1.w)
    
    andi    #%1110,CCR  ;%0000
    scc     $8000.l
    
    ori     #%0001,CCR  ;%0001
    scs     d0

    eori    #%0001,CCR  ;%0000
    scs     d0

    move    #%0100,CCR
    sne     d0

    move    #%0000,CCR
    sne     d0

    move    #%0100,CCR
    seq     d0

    move    #%0000,CCR
    seq     d0

    move    #%0001,CCR
    svc     d0

    move    #%0000,CCR
    svc     d0

    move    #%0001,CCR
    svs     d0

    move    #%0000,CCR
    svs     d0

    move    #%1000,CCR
    spl     d0

    move    #%0000,CCR
    spl     d0

    move    #%1000,CCR
    smi     d0

    move    #%0000,CCR
    smi     d0

    move    #%1010,CCR
    sge     d0

    move    #%0000,CCR
    sge     d0

    move    #%1000,CCR
    slt     d0

    move    #%0000,CCR
    slt     d0

    move    #%1010,CCR
    sgt     d0

    move    #%0000,CCR
    sgt     d0

    move    #%0100,CCR
    sle     d0

    move    #%0000,CCR
    sle     d0

    rts
