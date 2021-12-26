!ifndef is_main !eof

; ###########################
save_game:
    lda score_87            ; \
    sta hiscore_87          ;  |
    lda score_65            ;  |- transfer score
    sta hiscore_65          ;  |  to high score
    lda score_43            ;  |
    sta hiscore_43          ;  |
    lda score_21            ;  |
    sta hiscore_21          ; /
    lda #SAVE_FILE          ; \
    ldx #IO_DEVICE          ;  |- prepare logical file
    ldy #FILE_WRITE         ;  |
    jsr SETLFS              ; /
    lda #file_save_len+2 ; \  )- size of filename
    ldx #<file_save_w    ;  | )- address of
    ldy #>file_save_w    ;  | )- file name
    jsr SETNAM              ; /  )- set filename
    jsr OPEN                ; )- open file
    bcs .save_failed        ; )- or fail
    jsr check_disk_error    ; )- check file status
    bcc +                   ; \
.save_failed:               ;  |
    lda #SAVE_FILE          ;  |
    jsr CLOSE               ;  |- if error we skip
    jmp .save_game_end     ;  |
+                           ; /
    ldx #SAVE_FILE          ; \_ set file as current channel
    jsr CHKOUT              ; /
    lda score_87            ; \
    jsr CHROUT              ;  |
    lda score_65            ;  |- write score
    jsr CHROUT              ;  |
    lda score_43            ;  |
    jsr CHROUT              ;  |
    lda score_21            ;  |
    jsr CHROUT              ; /
    lda music_volume        ; \_ write volume
    jsr CHROUT              ; /
    lda game_status         ; \_ write game status
    jsr CHROUT              ; /
    jsr CLRCHN              ; \
    lda #SAVE_FILE          ;  |- close the file and reset channels
    jsr CLOSE               ; /
.save_game_end:
    rts
; ###########################

; ###########################
check_disk_error:
    lda #IOCHECK_FILE       ; \
    ldx #IO_DEVICE          ;  |- prepare logical file
    ldy #IOCHECK            ;  |- with a special secondary address
    jsr SETLFS              ; /
    lda #0                  ; \
    jsr SETNAM              ;  |- open a file with no name (=read status)
    jsr OPEN                ; /
    ldx #IOCHECK_FILE       ; \_ open channel for iocheck
    jsr CHKIN               ; /
    jsr CHRIN               ; \
    tax                     ;  |- load the first two bytes
    jsr CHRIN               ;  |  from the status string
    tay                     ; /
-   jsr CHRIN               ; \
    cmp #PET_REGULAR_RTN    ;  |- load the rest of the string
    bne -                   ; /
    cpx #"0"                ; \
    bne +                   ;  |- figure out if there was an error
    cpy #"0"                ;  |  petscii "00" = OK
    bne +                   ; /
    jsr CLRCHN              ; \
    lda #IOCHECK_FILE       ;  |- close the checking file
    jsr CLOSE               ; /
    rts                     ; )- return carry cleared = good to go
+   jsr CLRCHN              ; \
    lda #IOCHECK_FILE       ;  |- close the checking file
    jsr CLOSE               ; /
    sec                     ; \_ return carry set = an error occured
    rts                     ; /
; ###########################

; ###########################
load_game:
    ; LOAD SCORE
    lda #SAVE_FILE          ; \
    ldx #IO_DEVICE          ;  |- prepare logical file
    ldy #FILE_READ          ;  |
    jsr SETLFS              ; /
    lda #file_save_len   ; \  )- size of filename
    ldx #<file_save_r    ;  | )- address of
    ldy #>file_save_r    ;  | )- file name
    jsr SETNAM              ; /  )- set filename
    jsr OPEN                ; )- open file
    bcs .load_failed        ; )- or fail
    jsr check_disk_error    ; )- check file status
    bcc +                   ; \
.load_failed:               ;  |
    stz hiscore_87          ;  |
    stz hiscore_65          ;  |- if error we set a default value else we continue
    stz hiscore_43          ;  |
    stz hiscore_21          ;  |
    stz game_status         ;  |
    lda #$3F                ;  |
    sta saved_volume        ;  |
    sta music_volume        ;  |
    sta sfx_volume          ;  |
    lda #SAVE_FILE          ;  |
    jsr CLOSE               ;  |
    jsr save_game           ;  |
    jmp .load_game_end      ;  |
+                           ; /
    ldx #SAVE_FILE          ; \_ set file as current channel
    jsr CHKIN               ; /
    jsr GETIN               ; \_ get first byte
    sta hiscore_87          ; /
    jsr GETIN               ; \
    sta hiscore_65          ;  |
    jsr GETIN               ;  |- load the rest of the score
    sta hiscore_43          ;  |
    jsr GETIN               ;  |
    sta hiscore_21          ; /
    jsr GETIN               ; \
    and #$3F                ;  |- load the volume
    sta saved_volume        ;  |
    sta music_volume        ;  |
    sta sfx_volume          ; /
    jsr GETIN               ;  |
    sta game_status         ; /
    jsr CLRCHN              ; \
    lda #SAVE_FILE          ;  |- close the file and reset channels
    jsr CLOSE               ; /
.load_game_end:
    rts
; ###########################

; ###########################
init_game_screen:
    stz scroll_speed
    +fn_locate 0, 0, str_ui_full_row
    +fn_print str_ui_game_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_game_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_game_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_game_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_empty_slot_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_game_row
    +fn_print str_ui_full_row

    +fn_locate 29,2, str_ui_hiscore_lbl
    +fn_locate 29,5, str_ui_score_lbl
    +fn_locate 29,8, str_ui_lives_lbl
    +fn_locate 29,11, str_ui_panics_lbl
    rts
; ###########################

; ###########################
init_game:                  ;
    stz invincibility_cnt   ; )- reset invincibility
    stz score_over_time     ; )- reset score over time
    lda #<choregraphy_start ; \
    sta choregraphy_pc_l    ;  |- reset choregraphy
    lda #>choregraphy_start ;  |
    sta choregraphy_pc_h    ; /
    lda #<music_idle        ; \
    sta composer_pc_l       ;  |- init music engine pc
    lda #>music_idle        ;  |
    sta composer_pc_h       ; /
    lda #$3C                ; \
    sta composer_rythm      ;  |- init music engine vars
    lda #$3C                ;  |
    sta composer_delay      ; /
    lda #$05                ; \
    sta lives               ;  |- reset lives & panics
    lda #$03                ;  |
    sta panics              ; /
    lda #GRAZE_BONUS_NORMAL ; \_ set graze bonus
    sta graze_bonus         ; /
    stz score_87            ; \
    stz score_65            ;  |- reset score
    stz score_43            ;  |
    stz score_21            ; /
    jsr reset_objects       ; )- reset objects
    lda #player_spid        ; \_ reset player sprite
    jsr change_player_sprite; /
    ldx #$01                ; \
    ldy #$01                ;  |- reset virus's sprite
    lda #virus1_spid        ;  |
    jsr change_obj_sprite   ; /
    ldx #$02                ; \
    ldy #$7D                ;  |- reset bullet sprite
    lda #bullet_spid        ;  |
    jsr change_obj_sprite   ; /
    lda difficulty          ; \
    cmp #DIFFICULTY_EASY    ;  |- handle EASY parameters
    bne +                   ;  |
    lda #$09                ;  |
    sta lives               ;  |
    lda #$08                ;  |
    sta panics              ;  |
    lda #GRAZE_BONUS_EASY   ;  |
    sta graze_bonus         ; /
+   cmp #DIFFICULTY_HARD    ; \
    bne +                   ;  |- handle HARD parameters
    lda #GRAZE_BONUS_HARD   ;  |
    sta graze_bonus         ; /
+
init_game_ui:
    jsr refresh_hiscore     ; \
    jsr refresh_score       ;  |- reset ui
    jsr refresh_lives       ;  |
    jsr refresh_panics      ; /
    rts                     ;
; ###########################

; ###########################
insert_object:              ; insert a new object in the list
    lda #<obj_table         ; \
    sta r_obj_a             ;  |
    lda #>obj_table         ;  |- object lookup loop init
    sta r_obj_a+1           ;  |
    ldy obj_count           ;  |
    iny                     ;  |
    phy                     ; /
-   
    ply                     ; \
    dey                     ;  |- for y=0 to obj_count
    beq .insert_obj_not_found; |
    phy                     ; /
;   ldy #obj_idx_type       ; \_ load movement type
    lda (r_obj_a);,Y        ; /
    beq .insert_obj_insert  ; )- jmp if type is null
    clc                     ; \_ obj_addr += obj_size
    +adc16 r_obj_a, obj_size; /
    bra -                   ; )- next
   
.insert_obj_not_found:      ; no free slot found
    inc obj_count           ; )- increment counter and insert at the end
    lda obj_count
    cmp #$80
    bne +
    ; overflow, skip insert
    dec obj_count
    rts

.insert_obj_insert          ; free slot found
    ply
+   ldy #$01                ; \
    lda r_obj_t             ;  |- insert type
    sta (r_obj_a)           ; /
    lda r_obj_p             ; \_ insert parameter
    sta (r_obj_a),Y         ; /
    iny                     ; \
    lda r_obj_x             ;  |- insert x pos
    sta (r_obj_a),Y         ; /
    iny                     ; \
    lda r_obj_y             ;  |- insert y pos
    sta (r_obj_a),Y         ; /
    rts                     ; )- end of routine
; ###########################

; ###########################
reset_objects:
    lda #<obj_table         ; \
    sta r_obj_a             ;  |
    lda #>obj_table         ;  |- object lookup loop init
    sta r_obj_a+1           ;  |
    ldy #$7F                ;  |
    sty obj_count           ;  |
    iny                     ;  |
    phy                     ; /
-
    ply                     ; \
    dey                     ;  |- for y=0 to obj_count
    beq +                   ;  |
    phy                     ; /
    lda #$01
    sta (r_obj_a)           ; \
    lda #$00                ;  |
    ldy #obj_idx_param      ;  |
    sta (r_obj_a),Y         ;  |- hard reset everything
    iny                     ;  |
    sta (r_obj_a),Y         ;  |
    iny                     ;  |
    lda #$FF                ;  | \_ pos_y = FF
    sta (r_obj_a),Y         ; /  /
    clc                     ; \_ obj_addr += obj_size
    +adc16 r_obj_a, obj_size; /
    jmp -                   ; )- next
+   
    rts
; ###########################

; ###########################
optimize_object_count:
    lda #<obj_table         ; \
    sta r_obj_a             ;  |
    lda #>obj_table         ;  |- object lookup loop init
    sta r_obj_a+1           ; /
    ldy #$00                ; )- total count
    ldx #$00                ; )- last found
-                           ; 
    iny                     ; )- count item
    lda (r_obj_a)           ; )- load object type
    beq +                   ; )- if object is 0, skip to the next iteration
    tya                     ; \_ x = y
    tax                     ; /
+   cpy #$7F                ; \_ check if we reached the end
    beq +                   ; /
    clc                     ; \_ obj_addr += obj_size
    +adc16 r_obj_a, obj_size; /
    jmp -                   ; )- next
+                           ;
    stx obj_count           ; )- update object count
    rts                     ; )- end of routine
; ###########################

; ###########################
handle_touched:
    lda (r_obj_a)
    beq +
    cmp #id_mov_reset
    beq +
    bra .touched_start
+   clc
    rts
.touched_start
    ldy #obj_idx_pos_x      ; \
    lda (r_obj_a),Y         ;  |- load object's x position
    sec                     ; <
    sbc plyr_x              ;  |- compare with player's x position
    bcc +                   ; /
    ; positive side         ; \
    cmp #$03                ;  |
    bcs .touched_end        ;  |
    jmp .touched_x          ;  |- check if distance (-+) 3
+   ;negative side          ;  |
    cmp #$FD                ;  |
    bcc .touched_end        ; /
.touched_x:                 ; \
    ldy #obj_idx_pos_y      ;  |- load object's y position
    lda (r_obj_a),Y         ; /
    sec                     ; \
    sbc plyr_y              ;  |- compare with player's y position
    bcc +                   ; /
    ; positive side         ; \
    cmp #$03                ;  |
    bcs .touched_end        ;  |
    jmp .touched_xy         ;  |- check if distance (-+) 3
+   ;negative side          ;  |
    cmp #$FD                ;  |
    bcc .touched_end        ; /
.touched_xy:                ; \
    sec                     ;  |- return with carry set (touched!)
    rts                     ; /
.touched_end:               ; \
    clc                     ;  |- return with carry clear (no touchy)
    rts                     ; /
; ###########################

; ###########################
; returns carry set         ;
;   if game over            ;
player_touched:             ;
    lda #INVINCIBILITY_FRAMES;\_ set invincibility frames
    sta invincibility_cnt   ; /
    lda #touched_player_spid; \_ set player's sprite
    jsr change_player_sprite; /
    lda #<game_sfx_touched  ; \
    sta x16_r0_l            ;  |- play the "touched" sfx
    lda #>game_sfx_touched  ;  |
    sta x16_r0_h            ;  |
    jsr play_sfx            ; /
    dec lives               ;
    bne +                   ;
    sec                     ; \_ return with GAMEOVER
    rts                     ; /
+   jsr refresh_lives       ; )- refresh lives counter 
    lda panics              ; \
    cmp #$03                ;  |- if panics < 3 reset them to 3
    bpl +                   ;  |
    lda #$03                ;  |
    sta panics              ;  |
    jsr refresh_panics      ; /
+   clc                     ; \_ return
    rts                     ; /
; ###########################

; ###########################
; r0 = sfx addr
; byte order:
;   sfx_duration
;   sfx_freq_l
;   sfx_freq_h
;   sfx_wave
;   sfx_change
play_sfx:
    ldy #$01
    lda (x16_r0_l),y
    sta sfx_freq_l
    iny
    lda (x16_r0_l),y
    sta sfx_freq_h
    iny
    lda (x16_r0_l),y
    sta sfx_wave
    iny
    lda (x16_r0_l),y
    sta sfx_change
    lda (x16_r0_l)          ; \_ duration is set last because of IRQ
    sta sfx_duration        ; /
    rts
; ###########################


; ###########################
; a = spid
change_player_sprite:
    pha
    +fn_vera_set_address $10 + vera_mem_sprite_bank, vera_mem_sprite
    pla
    sta vera_data_0
    rts
; ###########################

; ###########################
; x = from
; y = count
; a = spid
change_obj_sprite:
    pha
    +fn_vera_set_address $40 + vera_mem_sprite_bank, vera_mem_sprite
    cpx #$00
    beq .chg_obj_from
-   lda vera_data_0
    dex
    bne -
.chg_obj_from:
    pla
-   sta vera_data_0
    dey
    bne -
    rts
; ###########################

; ###########################
handle_graze:
    lda (r_obj_a)
    beq +
    cmp #id_mov_reset
    beq +
    bra .grazed_start
+   clc
    rts
.grazed_start
+   ldy #obj_idx_pos_x      ; \
    lda (r_obj_a),Y         ;  |- load object's x position
    sec                     ; <
    sbc plyr_x              ;  |- compare with player's x position
    bcc +                   ; /
    ; positive side         ; \
    cmp #$09                ;  |
    bcs .grazed_end         ;  |
    jmp .grazed_x           ;  |- check if distance (-+) 9
+   ;negative side          ;  |
    cmp #$F7                ;  |
    bcc .grazed_end         ; /
.grazed_x:                  ; \
    ldy #obj_idx_pos_y      ;  |- load object's y position
    lda (r_obj_a),Y         ; /
    sec                     ; \
    sbc plyr_y              ;  |- compare with player's y position
    bcc +                   ; /
    ; positive side         ; \
    cmp #$09                ;  |
    bcs .grazed_end         ;  |
    jmp .grazed_xy          ;  |- check if distance (-+) 9
+   ;negative side          ;  |
    cmp #$F7                ;  |
    bcc .grazed_end         ; /
.grazed_xy:                 ;
    lda graze_bonus         ; \
    ldx #$00                ;  |- graze scoring
    ldy #$00                ;  |
    jsr add_to_score        ; /
.grazed_end:                ;
    rts                     ; )- return
; ###########################

; ###########################
; Adds $YYXXAA to the score
add_to_score:
    sed                     ; \_ set decimal and reset carry
    clc                     ; /
    adc score_21            ; \_ increase first 2 digit
    sta score_21            ; /
    txa                     ; \
    adc score_43            ;  |- follow the 2 next digit
    sta score_43            ; /
    tya                     ; \
    adc score_65            ;  |- follow the 2 next digit
    sta score_65            ; /
    bcc +                   ;
    jsr inc_lives           ; )- grant a life when hitting X000000 points
    inc score_87            ; )- finally follow the last 2 digit
+   cld                     ; )- reset back to binary
    jsr refresh_score       ; )- refresh scores on screen
    rts
; ###########################

; ###########################
inc_lives:
    lda #$09                    ; \
    cmp lives                   ;  |- max 9
    beq +                       ; /
    inc lives                   ;
    jsr refresh_lives           ;)- refresh lives on screen
+   rts
; ###########################

; ###########################
inc_panics:
    lda #$08                    ; \
    cmp panics                  ;  |- max 8
    beq +                       ; /
    inc panics                  ;
    jsr refresh_panics          ;)- refresh panics on screen
+   rts
; ###########################

; ###########################
refresh_score:
    lda score_87
    +get_left
    ora #$30
    sta dynamic_string
    lda score_87
    +get_right
    ora #$30
    sta dynamic_string+1

    lda score_65
    +get_left
    ora #$30
    sta dynamic_string+2
    lda score_65
    +get_right
    ora #$30
    sta dynamic_string+3

    lda score_43
    +get_left
    ora #$30
    sta dynamic_string+4
    lda score_43
    +get_right
    ora #$30
    sta dynamic_string+5

    lda score_21
    +get_left
    ora #$30
    sta dynamic_string+6
    lda score_21
    +get_right
    ora #$30
    sta dynamic_string+7

    lda PET_NULL
    sta dynamic_string+8
    +fn_print str_color_score
    +fn_locate 30, 6, dynamic_string
    rts
; ###########################

; ###########################
refresh_hiscore:
    lda hiscore_87
    +get_left
    ora #$30
    sta dynamic_string
    lda hiscore_87
    +get_right
    ora #$30
    sta dynamic_string+1

    lda hiscore_65
    +get_left
    ora #$30
    sta dynamic_string+2
    lda hiscore_65
    +get_right
    ora #$30
    sta dynamic_string+3

    lda hiscore_43
    +get_left
    ora #$30
    sta dynamic_string+4
    lda hiscore_43
    +get_right
    ora #$30
    sta dynamic_string+5

    lda hiscore_21
    +get_left
    ora #$30
    sta dynamic_string+6
    lda hiscore_21
    +get_right
    ora #$30
    sta dynamic_string+7

    lda PET_NULL
    sta dynamic_string+8
    +fn_print str_color_score
    +fn_locate 30, 3, dynamic_string
    rts
; ###########################

; ###########################
refresh_lives:
    +fn_locate 29, 9, str_ui_empty_slot
    +fn_plot 30,9
    lda #PET_COLOR_LRED
    jsr CHROUT
    lda chr_live
    ldx lives
    beq +
-
    dex
    beq +
    jsr CHROUT
    jmp -
+   rts
; ###########################

; ###########################
refresh_panics:
    +fn_locate 29, 12, str_ui_empty_slot
    +fn_plot 30,12
    lda #PET_COLOR_LRED
    jsr CHROUT
    lda chr_panic
    ldx panics
    beq +
-
    jsr CHROUT
    dex
    beq +
    jmp -
+   rts
; ###########################