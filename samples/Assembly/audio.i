resetAudio      LDWI    0x0000
                STW     midiCommand
                STW     midiDelay
                STW     midiNote
                LDWI    giga_soundChan1
                STW     midiScratch
                LDWI    title_screenMidi00  ; midi score
                STW     midiStreamPtr

                LDI     0x04
                ST      ii

resetA_loop     LDI     giga_soundChan1     ; reset low byte
                ST      midiScratch
                LDWI    0x0200              
                DOKE    midiScratch         ; wavA and wavX
                INC     midiScratch
                INC     midiScratch    
                LDWI    0x0000
                DOKE    midiScratch         ; keyL and keyH
                INC     midiScratch
                INC     midiScratch
                DOKE    midiScratch         ; oscL and oscH
                INC     midiScratch + 1     ; increment high byte
                LoopCounter ii resetA_loop
                RET


playMidi        LDI     0x05                ; keep pumping soundTimer, so that global sound stays alive
                ST      giga_soundTimer
                LD      giga_frameCount
                SUBW    midiDelay
                BEQ     playM_start
                RET

playM_start     PUSH
playM_process   LDW     midiStreamPtr
                PEEK                        ; get midi stream byte
                STW     midiCommand
                LDW     midiStreamPtr
                ADDI    0x01
                STW     midiStreamPtr
                LDI     0xF0
                ANDW    midiCommand
                XORI    0x90                ; check for start note
                BNE     playM_endnote

                CALL    midiStartNote       ; start note
                BRA     playM_process
                
playM_endnote   XORI    0x10                ; check for end note
                BNE     playM_segment

                CALL    midiEndNote         ; end note
                BRA     playM_process


playM_segment   XORI    0x50                ; check for new segment
                BNE     playM_delay

                LDW     midiStreamPtr       ; midi score
                DEEK
                STW     midiStreamPtr       ; 0xD0 new midi segment address
                BRA     playM_process

playM_delay     LD      giga_frameCount     ; midiDelay = (midiCommand + peek(frameCount)) & 0x00FF 
                ADDW    midiCommand
                ST      midiDelay
                POP
                RET


midiStartNote   LDWI    giga_notesTable     ; note table in ROM
                STW     midiScratch
                LDW     midiStreamPtr       ; midi score
                PEEK
                SUBI    11
                LSLW
                ADDW    midiScratch
                STW     midiScratch
                LUP     0x00                ; get ROM midi note low byte
                ST      midiNote
                LDW     midiScratch
                LUP     0x01                ; get ROM midi note high byte
                ST      midiNote + 1
                LDW     midiCommand
                ANDI    0x03                ; get channel
                ADDI    0x01                
                ST      midiScratch + 1
                LDI     0xFC
                ST      midiScratch         ; channels address 0x01FC <-> 0x04FC
                LDW     midiNote
                DOKE    midiScratch         ; set note
                LDW     midiStreamPtr
                ADDI    0x01                ; midiStreamPtr++
                STW     midiStreamPtr
                RET


midiEndNote     LDW     midiCommand
                ANDI    0x03                ; get channel
                ADDI    0x01                
                ST      midiScratch + 1
                LDI     0xFC
                ST      midiScratch         ; channels address 0x01FC <-> 0x04FC
                LDWI    0x0000
                DOKE    midiScratch         ; end note
                RET