is_main = 1

!src "lib/x16.asm"
!src "lib/irq.asm"
!src "lib/vera.asm"
!src "lib/math.asm"
!src "lib/utils.asm"

!src "paper.asm"

; =======================================================================================================
; ###  #####   ######  ######  ######  ######  ######  #####   ######  ######  ##########################
; ###  ##  ##  ##  ##  ##  ##    ##    ##        ##    ##  ##  ##  ##  ##  ##  ##########################
; ###  ####    ##  ##  ##  ##    ##    ######    ##    ####    ######  ######  ##########################
; ###  ##  ##  ##  ##  ##  ##    ##        ##    ##    ##  ##  ##  ##  ##      ##########################
; ###  #####   ######  ######    ##    ######    ##    ##  ##  ##  ##  ##      ##########################
; =======================================================================================================
prg_boot:
    +basic_startup 
    +fn_vera_init_subroutines

    +game_video_init 

    +game_init

    jsr change_gamemode
    rts

; =======================================================================================================
; ###  ######  ######  ##     ##  ######      ##      ######  ######  ######  ###########################
; ###  ##      ##  ##  #### ####  ##          ##      ##  ##  ##  ##  ##  ##  ###########################
; ###  ## ###  ######  ## ### ##  #####       ##      ##  ##  ##  ##  ######  ###########################
; ###  ##  ##  ##  ##  ##  #  ##  ##          ##      ##  ##  ##  ##  ##      ###########################
; ###  ######  ##  ##  ##     ##  ######      ######  ######  ######  ##      ###########################
; =======================================================================================================

change_gamemode:
    lda game_mode           ;
                            ;
    cmp #gamemode_quit      ; \
    bne +                   ;  |- gamemode QUIT
        jmp quit_game       ; /
                            ;
+   cmp #gamemode_title     ; \
    bne +                   ;  |- gamemode TITLE
        jmp title_screen    ; /
                            ;
+   cmp #gamemode_game_init ; \
    bne +                   ;  |
        jsr init_game_screen;  |- gamemode INIT GAME
        jsr init_game       ;  |
        lda #gamemode_game  ;  |
        sta game_mode       ;  |
        jmp change_gamemode ; /
                            ;
+   cmp #gamemode_game      ; \
    bne +                   ;  |- gamemode GAME
        jmp game_loop       ; /
                            ;
+   cmp #gamemode_gameover  ; \
    bne +                   ;  |- gamemode GAMEOVER
        jmp game_over       ; /
                            ;
+   jmp quit_game           ; )- gamemode not found! quit game


gamemode_quit = $FF
quit_game:
    +fn_irq_restore kernal_irq      ; \_ restore IRQ and quit
    rts                             ; /


gamemode_title = $00
title_screen:                       ;
    lda hiscore_21                  ; \
    bne .score_loaded               ;  |
    lda hiscore_43                  ;  |- check if the hiscore
    bne .score_loaded               ;  |  is already loaded.
    lda hiscore_65                  ;  |  if not, load from the
    bne .score_loaded               ;  |  file
    lda hiscore_87                  ;  |
    bne .score_loaded               ; /

    jsr load_game                   ; )- load the score file
.score_loaded:                      ;
    lda #t_empty_id                 ; \
    jsr fill_layer0                 ;  |- reset game screen
    jsr init_game_screen            ;  |  and remove all objects
    jsr refresh_hiscore             ;  |
    jsr reset_objects               ; /
    lda #$FF                        ; \
    sta r_obj_t                     ;  |- insert a hidden object 
    stz r_obj_p                     ;  |  the first object has the
    stz r_obj_x                     ;  |  player sprite
    stz r_obj_y                     ;  |
    jsr insert_object               ; /
    lda #$6D                        ; \
    sta r_obj_x                     ;  |- insert the virus object
    lda #$50                        ;  |  second object has his sprite
    sta r_obj_y                     ;  |
    jsr insert_object               ; /
    jsr sleep_one_frame             ; )- wait one frame to update the object positions
    +fn_locate 2, 2, str_title_0    ; \
    +fn_locate 2, 3, str_title_1    ;  |- display the title
    +fn_locate 2, 4, str_title_2    ;  |
    +fn_locate 2, 5, str_title_3    ;  |
    +fn_locate 2, 6, str_title_4    ; /
-   jsr sleep_one_frame             ; \
    lda #0                          ;  |
    jsr joystick_get                ;  |- check release start button
    and #joystick_mask_start        ;  |
    beq -                           ; /

    +fn_locate 9,16, str_press_start; )- display "press start"

    +fn_locate 8,24, str_volume_ctrl; )- display volume controls

    +fn_locate 6,21, str_difficulty ; )- display difficulty setting
    
    stz x16_r0                      ; )- clear r0
refresh_ui:
    lda difficulty                  ; \
    cmp #DIFFICULTY_EASY            ;  |- check current difficulty setting
    bne +                           ;  |
    +fn_locate 11,22,str_diff_easy  ;  | )- write "easy"
    bra refresh_volume
+   cmp #DIFFICULTY_HARD            ;  |
    bne +                           ;  |
    +fn_locate 11,22,str_diff_hard  ;  | )- write "hard"
    bra refresh_volume
+   +fn_locate 11,22,str_diff_normal; /  )- write "normal"
refresh_volume:
    +fn_locate 1,25,str_ui_volume   ; \_ draw the volume UI
    +fn_plot 10,25                  ; /
    lda music_volume                ; \
    inc                             ;  |- load the volume divided by 8
    lsr                             ;  |  so we have 8 segments of volume
    lsr                             ;  |
    lsr                             ;  |
    tax                             ; /
    beq title_wait_start            ; \
    lda #PET_COLOR_WHITE            ;  |- if we have segments to draw,
    jsr CHROUT                      ;  |  print them
    lda chr_volume                  ;  |
-   jsr CHROUT                      ;  |
    dex                             ;  |
    bne -                           ;  |
    lda #PET_COLOR_BLACK            ;  |
    jsr CHROUT                      ; /
title_wait_start:
    lda wait_frame                  ; \
    bne title_wait_start            ;  |- wait for next frame
    inc wait_frame                  ; /
    lda frame_count                 ; \
    and #$01                        ;  |- on every odd frame:
    bne +                           ;  |
    jsr rng                         ;  | \
    and #$01                        ;  |  |
    adc #$6D                        ;  |  |- set the virus's position
    sta obj_table+6                 ;  |  |  directly via the table
    jsr rng                         ;  |  |  moves it randomly 1 pixel
    and #$01                        ;  |  |  right or down
    adc #$50                        ;  |  |
    sta obj_table+7                 ;  | /
    lda volume_cooldown             ;  | \
    beq +                           ;  |  |- decrease volume countdown
    dec volume_cooldown             ; /  /
+   lda #0                          ; \
    jsr joystick_get                ;  |- MAIN MENU
    tay                             ;  |
    and #joystick_mask_up           ;  | \
    bne +                           ;  |  |- check for joystick UP
    lda music_volume                ;  |  |  increase the game's volume
    clc                             ;  |  |
    adc #$08                        ;  |  |
    bra title_update_volume         ;  | /
+   tya                             ;  | \
    and #joystick_mask_down         ;  |  |- check for joystick DOWN
    bne +                           ;  |  |  decrease the game's volume
    lda music_volume                ;  |  |
    sec                             ;  |  |
    sbc #$08                        ;  |  |
    bra title_update_volume         ;  | /
+   tya
    and #joystick_mask_left
    bne +
    lda difficulty
    dec
    bra title_update_difficulty
+   tya
    and #joystick_mask_right
    bne +
    lda difficulty
    inc
    bra title_update_difficulty
+   stz volume_cooldown             ;  | \
    tya                             ;  |  |- check for joystick START
    and #joystick_mask_start        ;  |  |  start the game
    bne title_wait_start            ;  | /
    bra title_started               ; /
title_update_volume:
    tax                             ; \
    lda volume_cooldown             ;  |
    bne title_wait_start            ;  |- updates volume, uses r2 as temporary cooldown
    txa                             ;  |  
    bpl +                           ;  | \_ lower limit ($00)
    lda #$00                        ;  | /
+   cmp #$40                        ;  | \
    bmi +                           ;  |  |- upper limit ($3F)
    lda #$3F                        ;  | /
+   sta music_volume                ;  | \
    sta sfx_volume                  ;  |  |- store the new volume 
    lda #$05                        ;  |  |  and set the cooldown
    sta volume_cooldown             ;  | /
    lda #<game_sfx_pause            ;  | \
    sta x16_r0_l                    ;  |  |- play the "pause" sfx
    lda #>game_sfx_pause            ;  |  |  to give a sample to the user
    sta x16_r0_h                    ;  |  |
    jsr play_sfx                    ;  | /
    jmp refresh_volume              ; /
title_update_difficulty:
    tax                             ; \
    lda volume_cooldown             ;  |
    beq +                           ;  |- updates difficulty, uses r2 as temporary cooldown
    jmp title_wait_start            ;  |
+   txa                             ;  |
    bpl +                           ;  | \_ lower limit ($00)
    lda #$00                        ;  | /
+   cmp #$03                        ;  | \
    bmi +                           ;  |  |- upper limit ($02)
    lda #$02                        ;  | /
+   tax
    lda game_status
    and #STATUS_FINISHED_NORMAL
    bne +
    cpx #$02
    bne +
    ldx #$01
+   stx difficulty                  ;  |
    lda #$05                        ;  |
    sta volume_cooldown             ;  |
    lda #<game_sfx_pause            ;  | \
    sta x16_r0_l                    ;  |  |- play the "pause" sfx
    lda #>game_sfx_pause            ;  |  |  to give a sample to the user
    sta x16_r0_h                    ;  |  |
    jsr play_sfx                    ;  | /
+   jmp refresh_ui                  ; /

title_started:
    ; game started
    +fn_locate 1,2, str_ui_game     ; \
    +fn_locate 1,3, str_ui_game     ;  |- clear the title screen
    +fn_locate 1,4, str_ui_game     ;  |
    +fn_locate 1,5, str_ui_game     ;  |
    +fn_locate 1,6, str_ui_game     ;  |
    +fn_locate 1,16,str_ui_game     ;  |
    +fn_locate 1,21,str_ui_game     ;  |
    +fn_locate 1,22,str_ui_game     ;  |
    +fn_locate 1,24,str_ui_game     ;  |
    +fn_locate 1,25,str_ui_game     ; /

-   jsr sleep_one_frame             ; \
    dec obj_table+7                 ;  |- move the virus to the
    beq +                           ;  |  top of the screen
    dec obj_table+7                 ;  |  at 2px / frame
    bne -                           ; /
+                                   ; \
    ldx #$04                        ;  |
-   jsr sleep_one_frame             ;  |- then move it 8 more pixels 
    dec obj_table+7                 ;  |  to hide it completely
    dec obj_table+7                 ;  |
    dex                             ;  |
    bne -                           ; /
 
    lda music_volume                ; \
    cmp saved_volume                ;  |- save volume setting
    beq +                           ;  |
    jsr save_game                   ; /

+
    ldx #$3C                        ; \
-   jsr sleep_one_frame             ;  |- wait 1 second
    dex                             ;  |
    bne -                           ; /

    lda #gamemode_game_init         ; \
    sta game_mode                   ;  |- change gamemode to INIT GAME
    jmp change_gamemode             ; /


gamemode_gameover = $FE
game_over:
    lda #<game_sfx_gameover         ; \
    sta x16_r0_l                    ;  |- play the "gameover" sfx
    lda #>game_sfx_gameover         ;  |
    sta x16_r0_h                    ;  |
    jsr play_sfx                    ; /
    lda #t_empty_id                 ; \
    jsr fill_layer0                 ;  |- clear the game screen
    jsr reset_objects               ;  |  and display "game over"
    jsr sleep_one_frame             ;  |
    jsr music_clear                 ;  |
    +fn_locate 10,10,str_game_over  ; /
    ; compare hiscore with score
    lda hiscore_87                  ; \
    cmp score_87                    ;  |
    bcc .save_score                 ;  |- compare current score
    bne +                           ;  |  with current high score
    lda hiscore_65                  ;  |  
    cmp score_65                    ;  |  if score is higher, 
    bcc .save_score                 ;  |  save it (see .save_score)
    bne +                           ;  |  else skip the save
    lda hiscore_43                  ;  |
    cmp score_43                    ;  |
    bcc .save_score                 ;  |
    bne +                           ;  |
    lda hiscore_21                  ;  |
    cmp score_21                    ;  |
    bcc .save_score                 ;  |
+   jmp .gameover_sleep             ; /

.save_score:
    jsr save_game                   ; )- save the score

    ldx #$3C                        ; \
-   jsr sleep_one_frame             ;  |- wait 1 second
    dex                             ;  |
    bne -                           ; /
    +fn_locate 7, 12, str_new_hiscore;)- display "new hiscore!"

.gameover_sleep
    ldx #$3C                        ; \
    ldy #GAMEOVER_WAIT_S            ;  |
-                                   ;  |- sleeping loop
    jsr sleep_one_frame             ;  |
    dex                             ;  |
    bne -                           ;  |
    ldx #$3C                        ;  |
    dey                             ;  |
    bne -                           ; /

    lda #gamemode_title             ; \
    sta game_mode                   ;  |- back to title screen
    jmp change_gamemode             ; /


gamemode_game_init = $01
gamemode_game = $02
game_loop:
    lda #0                          ; \
    jsr joystick_get                ;  |- check for start button
    and #joystick_mask_start        ;  |
    bne +                           ; /
    jmp game_paused
+
    lda #0                          ; \
    jsr joystick_get                ;  |- check for B button
    and #joystick_mask_B            ;  |
    bne +                           ; /
    lda panics                      ; \_ check if can panic
    beq +                           ; /
    jmp throw_panic
+

!src "components/objects.asm"
!src "components/music.asm"
!src "components/choregraphy.asm"

    jmp game_loop



game_paused:
    lda #<game_sfx_pause            ; \
    sta x16_r0_l                    ;  |- play the "pause" sfx
    lda #>game_sfx_pause            ;  |
    sta x16_r0_h                    ;  |
    jsr play_sfx                    ; /
    lda scroll_speed                ; \
    pha                             ;  |- backup & clear scrolling speed
    stz scroll_speed                ; /
    lda music_volume                ; \
    pha                             ;  |- backup & clear global volume
    stz music_volume                ; /
-   jsr sleep_one_frame             ; \
    lda #0                          ;  |
    jsr joystick_get                ;  |- check release start button
    and #joystick_mask_start        ;  |
    beq -                           ; /
-   jsr sleep_one_frame             ; \
    lda frame_count                 ;  |
    and #$20                        ;  |- display a blinking "paused"
    bne +                           ;  |
    +fn_locate 31,27, str_paused    ;  |
    jmp .paused_text_printed        ;  |
+   +fn_locate 31,27, str_paused_clr;  |
.paused_text_printed:               ; /
    lda #0                          ; \
    jsr joystick_get                ;  |- check for start button
    and #joystick_mask_start        ;  |
    bne -                           ; /
    lda #<game_sfx_pause            ; \
    sta x16_r0_l                    ;  |- play the "pause" sfx
    lda #>game_sfx_pause            ;  |
    sta x16_r0_h                    ;  |
    jsr play_sfx                    ; /
-   jsr sleep_one_frame             ; \
    lda #0                          ;  |
    jsr joystick_get                ;  |- check release start button
    and #joystick_mask_start        ;  |
    beq -                           ; /
    +fn_locate 31,27, str_paused_clr; )- remove "paused" text
    pla                             ; \_ restore global volume
    sta music_volume                ; /
    pla                             ; \_ restore the scrolling speed
    sta scroll_speed                ; /
    jmp game_loop


throw_panic:
    lda scroll_speed                ; \
    pha                             ;  |- backup & clear scrolling speed
    stz scroll_speed                ; /
    lda music_volume                ; \
    pha                             ;  |- backup & clear global volume
    stz music_volume                ; /
    dec panics                      ; \_ update panics counter
    jsr refresh_panics              ; /
    lda #<game_sfx_panic
    sta x16_r0_l
    lda #>game_sfx_panic
    sta x16_r0_h
    jsr play_sfx
    ldx #$02                        ; \
    ldy #$7D                        ;  |- change bullet sprite
    lda #bullet_panic_spid          ;  |
    jsr change_obj_sprite           ; /
    lda obj_count                   ; \ - add obj_count to score
    dec                             ;  | \_ remove player & virus from count
    dec                             ;  | /
    sta x16_r0                      ;  |
    jsr hex_to_dec                  ;  | )- convert obj_count to decimal #0123
    ldy #$00                        ;  | )- Y should contain #00
    lda x16_r0                      ;  | \
    asl                             ;  |  |- shift hundreds
    asl                             ;  |  |
    asl                             ;  |  |
    asl                             ;  |  |
    sta x16_r0                      ;  | /
    lda x16_r1_l                    ;  | \
    lsr                             ;  |  |- X should contain #12
    lsr                             ;  |  |
    lsr                             ;  |  |
    lsr                             ;  |  |
    ora x16_r0                      ;  |  |
    tax                             ;  | /
    lda x16_r1_l                    ;  | \
    asl                             ;  |  |- Y should contain #30
    asl                             ;  |  |
    asl                             ;  |  |
    asl                             ;  | /
    jsr add_to_score                ; /  )- should add #001230 to score
    ldx #$3C                        ; \
-   jsr sleep_one_frame             ;  |- sleep 1 second
    dex                             ;  |
    bne -                           ; /
    ldx #$08                        ; \
-   lda obj_table,x                 ;  |
    sta x16_r0,x                    ;  |- backup the first two objects
    dex                             ;  |  (player and virus)
    cpx #$FF                        ;  |
    bne -                           ; /
    jsr reset_objects               ; \
    lda #$02                        ;  |- reset objects and set count to 2
    sta obj_count                   ; /
    ldx #$08                        ; \
-   lda x16_r0,x                    ;  |
    sta obj_table,x                 ;  |- restore the first two objects
    dex                             ;  |
    cpx #$FF                        ;  |
    bne -                           ; /
    jsr sleep_one_frame
    ldx #$02                        ; \
    ldy #$7D                        ;  |- restore bullet sprite
    lda #bullet_spid                ;  |
    jsr change_obj_sprite           ; /
    lda #<game_sfx_panic2
    sta x16_r0_l
    lda #>game_sfx_panic2
    sta x16_r0_h
    jsr play_sfx
    pla                             ; \_ restore global volume
    sta music_volume                ; /
    pla                             ; \_ restore the scrolling speed
    sta scroll_speed                ; /
    jmp game_loop

; =======================================================================================================
; ###  ######  ##  ##  #####   #####   ######  ##  ##  ######  ##  ##    ##  ######  ######  ############
; ###  ##      ##  ##  ##  ##  ##  ##  ##  ##  ##  ##    ##    ##  ####  ##  ##      ##      ############
; ###  ######  ##  ##  ####    ####    ##  ##  ##  ##    ##    ##  ## ## ##  #####   ######  ############
; ###      ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##    ##    ##  ##  ####  ##          ##  ############
; ###  ######  ######  #####   ##  ##  ######  ######    ##    ##  ##    ##  ######  ######  ############
; =======================================================================================================


!src "routines/screen.asm"

; ###########################
sleep_one_frame:
    lda #$FF                ; \
    sta wait_frame          ;  |- wait vsync
-   lda wait_frame          ;  |
    bne -                   ; /
    rts
; ###########################

; ###########################
; Linear feedback shift register (see https://wiki.nesdev.com/w/index.php/Random_number_generator)
; returns a random byte to A (0-255)
; resets Y to 0
rng:
    ldy #8                  ; iteration count (generates 8 bits)
	lda rng_seed_0
-
	asl                     ; shift the register
	rol rng_seed_1
	bcc +
	eor #$39                ; apply XOR feedback whenever a 1 bit is shifted out
+
	dey
	bne -
	sta rng_seed_0
	cmp #0                  ; reload flags
	rts
; ###########################

; ###########################
; Convert input into a BCD value (see http://6502.org/source/integers/hex2dec.htm)
; input: r0
; return: r1
.htd_in = x16_r0
.htd_out= x16_r1
.htd_table: 
    !word $01, $02, $04, $08, $10, $20, $40, $80
                ; (Word directive puts low byte first.)

hex_to_dec:
    sed                 ; output gets added up in decimal.
    stz .htd_out        ; inititalize output word as 0.
    stz .htd_out+1

    ldx #$0E            ; $e is 14 for 2x7 bits.  (0-7 is 8 positions.)
-  
    asl .htd_in         ; look at next high bit.  if it's 0,
    bcc +               ; don't add anything to the output for this bit.
    lda .htd_out        ; otherwise get the running output sum
    clc
    adc .htd_table,x    ; and add the appropriate value for this bit
    sta .htd_out        ; from the table, and store the new sum.
    lda .htd_out+1      ; after low byte, do high byte.
    adc .htd_table+1,x
    sta .htd_out+1

+   dex                 ; go down to next bit value to loop again.
    dex
    bpl -               ; if still not done, go back for another loop.

    cld
    rts
; ###########################

!src "routines/game.asm"

; =======================================================================================================
; ###  ##  ##  ######  ##  ##  ##    ##  ######      ##      ######  ######  ######  ####################
; ###  ##  ##  ##      ##  ##  ####  ##  ##          ##      ##  ##  ##  ##  ##  ##  ####################
; ###  ##  ##  ######   ####   ## ## ##  ##          ##      ##  ##  ##  ##  ######  ####################
; ###   ####       ##    ##    ##  ####  ##          ##      ##  ##  ##  ##  ##      ####################
; ###    ##    ######    ##    ##    ##  ######      ######  ######  ######  ##      ####################
; =======================================================================================================

vsync_loop:
    lda vera_isr                ; \
    and #vera_isr_mask_vsync    ;  |- work only on vsync
    bne +                       ;  |
    jmp irq_done                ; /
+

backup_vera_addr:
    lda vera_high_addr          ; \
    pha                         ;  |
    lda vera_low_addr           ;  |- backup VERA addresses
    pha                         ;  |
    lda vera_stride_bank        ;  |
    pha                         ; /

draw_sprites:
    lda #%00010001              ; \_ set vera stride & bank values
    sta vera_stride_bank        ; /
    lda #$FC                    ; \
    sta vera_high_addr          ;  |- set vera to the sprite address
    lda #$02                    ;  |
    sta vera_low_addr           ; /
    lda #<obj_table-obj_size    ; \
    sta unsafe_addr_l           ;  |- init object address
    lda #>obj_table-obj_size    ;  |
    sta unsafe_addr_h           ; /
    ldy obj_count               ; \
    iny                         ;  |
    phy                         ;  |
draw_sprites_lp:                ;  |
    clc                         ;  |- for x=obj_count to 0
    +adc16 unsafe_addr,obj_size ;  |- addr += obj_size
    ply                         ;  |
    dey                         ;  |
    beq draw_sprites_done       ; /
    phy
    ldy #obj_idx_pos_x          ; \
    lda (unsafe_addr),Y         ;  |- update pos x
    sta vera_data_0             ;  |
    stz vera_data_0             ; /
    ldy #obj_idx_pos_y          ; \
    lda (unsafe_addr),Y         ;  |- update pos y
    sta vera_data_0             ;  |
    stz vera_data_0             ; /

    lda vera_data_0             ; \
    lda vera_data_0             ;  |- sprite data padding
    lda vera_data_0             ;  |
    lda vera_data_0             ; /
    jmp draw_sprites_lp
draw_sprites_done:

autoscroll:
    lda scroll_speed            ; \
    beq autoscroll_done         ;  |
    tax                         ;  |- extract bit 7 - 2
    clc                         ;  |  and store into tmp var
    ror                         ;  |
    clc                         ;  |
    ror                         ;  |
    sta unsafe_addr_l           ; /
    txa                         ; \
    and #$03                    ;  |- extract bit 1-0
    beq +                       ;  |
    sta unsafe_addr_h           ; /
    and frame_count             ; \
    cmp unsafe_addr_h           ;  |- handle 1/4 movements
    bne +                       ;  |
    dec vera_layer0_vscroll_L   ; /
+   lda vera_layer0_vscroll_L   ; \
    sec                         ;  |- handle integer movements
    sbc unsafe_addr_l           ;  |
    sta vera_layer0_vscroll_L   ; /
autoscroll_done:

psg_upload:
    ldy #$00
    +fn_vera_set_address $10|vera_mem_psg_bank, vera_mem_psg
-   
    ldx psg_vo_note,y           ; \
    lda note_l,x                ;  |- pick voice note
    sta vera_data_0             ;  |  from note table
    lda note_h,x                ;  |
    sta vera_data_0             ; /

    lda psg_vo_instr,y          ; \
    asl                         ;  |- get instrument index
    asl                         ;  |
    asl                         ;  |
    tax                         ; /
    lda psg_vo_volumeHi,y                       ; \
    and music_volume                            ;  |- set instrument waveform
    ora instrument_def + instr_idx_direction,x  ;  |  volume and direction
    sta vera_data_0                             ;  |  based on instrument table
    lda instrument_def + instr_idx_waveform,x   ;  |
    ora #$3F                                    ;  |
    sta vera_data_0                             ; /
    iny                         ; \
    cpy #$08                    ;  |- loop through 8 voices
    bne -                       ; /

psg_upload_sfx:
    lda sfx_duration            ; \
    bne +                       ;  |- check if should play a sfx
    stz vera_data_0             ;  |
    stz vera_data_0             ;  |
    stz vera_data_0             ;  |
    stz vera_data_0             ;  |
    bra psg_upload_end          ; /
+                               ;
    lda sfx_freq_l              ; \
    sta vera_data_0             ;  |- upload SFX to PSG
    lda sfx_freq_h              ;  |
    sta vera_data_0             ;  |
    lda sfx_volume              ;  |
    ora #vera_psg_left|vera_psg_right
    sta vera_data_0             ;  |
    lda sfx_wave                ;  |
    sta vera_data_0             ; /

    lda sfx_change              ; \
    beq psg_upload_sfx_end      ;  |- update frequency and duration
    bmi +                       ;  |
    and #$7F                    ;  | \
    clc                         ;  |  |
    adc sfx_freq_l              ;  |  |- add to frequency
    bcc psg_upload_sfx_end      ;  |  |
    inc sfx_freq_h              ;  |  |
    bra psg_upload_sfx_end      ;  | /
+   and #$7F                    ;  | \
    sta unsafe_addr_l           ;  |  |
    lda sfx_freq_l              ;  |  |- substract to frequency
    sec                         ;  |  |
    sbc sfx_change              ;  |  |
    bcs psg_upload_sfx_end      ;  |  |
    dec sfx_freq_h              ;  |  |
    bra psg_upload_sfx_end      ;  | /
psg_upload_sfx_end:             ;  | \
    sta sfx_freq_l              ;  |  |- save the calculated values
    dec sfx_duration            ; /  /
psg_upload_end:

restore_vera_addr:
    pla                         ; \
    sta vera_stride_bank        ;  |
    pla                         ;  |- restore VERA addresses
    sta vera_low_addr           ;  |
    pla                         ;  |
    sta vera_high_addr          ; /

    stz wait_frame              ; \
    inc frame_count             ;  |- increment frame_count and clear wait_frame flag
irq_done:                       ;  |  and hand over to the kernal's IRQ handler
    jmp (kernal_irq)            ; /

; =======================================================================================================
; ###  #####   ######  ######  ######  ##  ##  #####   ######  ######  ######  ##########################
; ###  ##  ##  ##      ##      ##  ##  ##  ##  ##  ##  ##      ##      ##      ##########################
; ###  ####    #####   ######  ##  ##  ##  ##  ####    ##      #####   ######  ##########################
; ###  ##  ##  ##          ##  ##  ##  ##  ##  ##  ##  ##      ##          ##  ##########################
; ###  ##  ##  ######  ######  ######  ######  ##  ##  ######  ######  ######  ##########################
; =======================================================================================================

!src "components/movements.asm"

!src "resources/strings.asm"
!src "resources/sprites.asm"
!src "resources/tiles.asm"

!src "resources/sfx.asm"

music_idle:
    !byte N_GSP
    !byte N_VOI,1
    !byte N_RTM, 10
music_idle_lp:
    !byte N_GNP
    !byte N_JMP, <music_idle_lp, >music_idle_lp

!src "resources/musics/coward_menace.asm"
!src "resources/musics/coward_menace2.asm"
!src "resources/musics/coward_menace3.asm"
!src "resources/musics/seeking.asm"
!src "resources/musics/data_rain.asm"
!src "resources/musics/moving_bytes.asm"

choregraphy_start:
!byte CHOR_OP_SPS, $6F, $C7
!byte CHOR_OP_INS, id_mov_plyr, $00
!byte CHOR_OP_SPS, $00, $FF
!byte CHOR_OP_INS, id_mov_incr, $00
!src "resources/levels/1-filesystem.asm"
!src "resources/levels/2-high-ram.asm"
!src "resources/levels/3-low-ram.asm"

; force virus out
!byte CHOR_OP_SPS, $00, $FF
!byte CHOR_OP_INS, id_mov_incr, $00
; end loop, kill the player
!byte CHOR_OP_SPS, $01, $01
!pet CHOR_OP_PRD, 7, 2, "- end of game -", PET_NULL
.choregraphy_end:
!byte CHOR_OP_SLP, $02
!byte CHOR_OP_FPS, $60
!byte CHOR_OP_INS, id_mov_incr, $04
!byte CHOR_OP_JMP, <.choregraphy_end, >.choregraphy_end
