{{
┌───────────────────────────────────────────┬────────────────┬───────────────────────────────────┬─────────────────┐
│ Vocal Tract v1.1                          │ by Chip Gracey │ Copyright (c) 2006 Parallax, Inc. │ 28 October 2006 │
├───────────────────────────────────────────┴────────────────┴───────────────────────────────────┴─────────────────┤
│                                                                                                                  │
│ This object synthesizes a human vocal tract in real-time. It requires one cog and at least 80 MHz.               │
│                                                                                                                  │
│ The vocal tract is controlled via 13 single-byte parameters which must reside in the parent object:              │
│                                                                                                                  │
│ VAR byte aa,ga,gp,vp,vr,f1,f2,f3,f4,na,nf,fa,ff    'vocal tract parameters                                       │
│                                                                                                                  │
│                                                                                                                  │
│                        aa                                                                                        │
│                  ┌────────────┐                                                                                  │
│                  │ ASPIRATION ├──┐                                                                              │
│                  └────────────┘   │     f1       f2       f3       f4      na   nf                               │
│                                      ┌────┐   ┌────┐   ┌────┐   ┌────┐   ┌───────┐                              │
│                                  +┣──┤ F1 ├──┤ F2 ├──┤ F3 ├──┤ F4 ├──┤ NASAL ├──┐                          │
│                       ga   gp        └────┘   └────┘   └────┘   └────┘   └───────┘   │                          │
│                     ┌─────────┐   │                                                                             │
│                     │ GLOTTAL ├──┘                                                  +┣── OUTPUT                │
│                     └────┬────┘                                          fa   ff                                │
│                                                                      ┌───────────┐   │                          │
│                       vp │ vr                                         │ FRICATION ├──┘                          │
│                     ┌────┴────┐                                       └───────────┘                              │
│                     │ VIBRATO │                                                                                  │
│                     └─────────┘                                                                                  │
│                                                                                                                  │
│                                                                                                                  │
│       ┌───────────┬──────────────────────┬─────────────┬────────────────────────────────────────────────┐        │
│       │ parameter │ description          │ unit        │ notes                                          │        │
│       ├───────────┼──────────────────────┼─────────────┼────────────────────────────────────────────────┤        │
│       │    aa     │ aspiration amplitude │ 0..255      │ breath volume: silent..loud, linear            │        │
│       │    ga     │ glottal amplitude    │ 0..255      │ voice volume: silent..loud, linear             │        │
│       │    gp     │ glottal pitch        │ 1/48 octave │ voice pitch: 100 ─ 110.00Hz (musical note A2) │        │
│       │    vp     │ vibrato pitch        │ 1/48 octave │ voice vibrato pitch: 48 ─ ± 1/2 octave swing  │        │
│       │    vr     │ vibrato rate         │ 0.0763 Hz   │ voice vibrato rate: 52 ─ 4 Hz                 │        │
│       │    f1     │ formant1 frequency   │ 19.53 Hz    │ 1st resonator frequency: 40 ─ 781 Hz          │        │
│       │    f2     │ formant2 frequency   │ 19.53 Hz    │ 2nd resonator frequency: 56 ─ 1094 Hz         │        │
│       │    f3     │ formant3 frequency   │ 19.53 Hz    │ 3rd resonator frequency: 128 ─ 2500 Hz        │        │
│       │    f4     │ formant4 frequency   │ 19.53 Hz    │ 4th resonator frequency: 179 ─ 3496 Hz        │        │
│       │    na     │ nasal amplitude      │ 0..255      │ anti-resonator level: off..on, linear          │        │
│       │    nf     │ nasal frequency      │ 19.53 Hz    │ anti-resonator frequency: 102 ─ 1992 Hz       │        │
│       │    fa     │ frication amplitude  │ 0..255      │ white noise volume: silent..loud, linear       │        │
│       │    ff     │ frication frequency  │ 39.06 Hz    │ white noise frequency: 60 ─ 2344 Hz ("Sh")    │        │
│       └───────────┴──────────────────────┴─────────────┴────────────────────────────────────────────────┘        │
│                                                                                                                  │
│ The parent object alternately modifies one or more of these parameters and then calls the go(time) method to     │
│ queue the entire 13-parameter frame for feeding to the vocal tract. The vocal tract will load one queued frame   │
│ after another and smoothly interpolate between them over specified amounts of time without interruption. Up to   │
│ eight frames will be queued in order to relax the frame-generation timing requirement of the parent object. If   │
│ eight frames are queued, the parent must then wait to queue another frame. If the vocal tract runs out of        │
│ frames, it will continue generating samples based on the last frame. When a new frame is queued, it will         │
│ immediately load it and begin inter-polating towards it.                                                         │                                    
│                                                                                                                  │
│ The vocal tract generates audio samples at a continuous rate of 20KHz. These samples can be output to pins via   │
│ delta-modulation for RC filtering or direct transducer driving. An FM aural subcarrier can also be generated for │
│ inclusion into a TV broadcast controlled by another cog. Regardless of any output mode, samples are always       │
│ streamed into a special variable so that other objects can access them in real-time.                             │                                                           
│                                                                                                                  │
│ In order to achieve optimal sound quality, it is worthwhile to maximize amplitudes such as 'ga' to the point     │
│ just shy of numerical overflow. Numerical overflow results in high-amplitude noise bursts which are quite        │
│ disruptive. The closeness of 'f1'-'f4' and their relationship to 'gp' can greatly influence the amount of 'ga'   │
│ that can be applied before overflow occurs. You must determine through experimentation what the limits are. By   │
│ pushing 'ga' close to the overflow point, you will maximize the signal-to-noise ratio of the vocal tract,        │
│ resulting in the highest quality sound. Once your vocal tract programming is complete, the attenuation level     │
│ can then be used to reduce the overall output in 3dB steps while preserving the signal-to-noise ratio.           │                                       
│                                                                                                                  │
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│ Revision History                                                                   v1.0 released 26 October 2006 │
│                                                                                                                  │
│ v1.1  If the vocal tract runs out of frames, its internal parameters will now be brought all the way to the      │
│       last frame's values. Before, they were left one interpolation point shy, and then set to the last frame's  │
│       values at the start of the next frame. For continuous frames this was trivial, but it posed a problem      │
│       during frame gaps because the internal parameters would get stalled at transition points just shy of the   │
│       last frame's values. This change makes the vocal tract behave more sensibly during frame gaps.             │                                                                        
│                                                                                                                  │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘                                     
                                                                                                                                            
}}                                                                                                                                          
CON                                                                                                                                         
                                                                                                                                            
  frame_buffers = 8                                     'frame buffers (2n)                                                                
                                                                                                                                            
  frame_bytes = 3 {for stepsize} + 13 {for aa..ff}      '16 bytes per frame                                                                 
  frame_longs = frame_bytes / 4                         '4 longs per frame                                                                  
                                                                                                                                            
  frame_buffer_bytes = frame_bytes * frame_buffers                                                                                          
  frame_buffer_longs = frame_longs * frame_buffers                                                                                          
                                                                                                                                            
                                                                                                                                            
VAR                                                                                                                                         
                                                                                                                                            
  long  cog, tract, pace                                                                                                                    
                                                                                                                                            
  long  index, attenuation, sample                      '3 longs       ...must                                                              
  long  dira_, dirb_, ctra_, ctrb_, frqa_, cnt_         '6 longs       ...be                                                                
  long  frames[frame_buffer_longs]                      'many longs    ...contiguous                                                        
                                                                                                                                            
                                                                                                                                            
PUB start(tract_ptr, pos_pin, neg_pin, fm_offset) : okay                                                                                    
                                                                                                                                            
'' Start vocal tract driver - starts a cog                                                                                                  
'' returns false if no cog available                                                                                                        
''                                                                                                                                          
''   tract_ptr = pointer to vocal tract parameters (13 bytes)                                                                               
''     pos_pin = positive delta-modulation pin (-1 to disable)                                                                              
''     neg_pin = negative delta-modulation pin (pos_pin must also be enabled, -1 to disable)                                                
''   fm_offset = offset frequency for fm aural subcarrier generation (-1 to disable, 4_500_000 for NTSC)                                    
                                                                                                                                            
  'Reset driver                                                                                                                             
  stop                                                                                                                                      
                                                                                                                                            
  'Remember vocal tract parameters pointer                                                                                                  
  tract := tract_ptr                                                                                                                        
                                                                                                                                            
  'Initialize pace to 100%                                                                                                                  
  pace := 100                                                                                                                               
                                                                                                                                            
  'If delta-modulation pin(s) enabled, ready output(s) and ready ctrb for duty mode                                                         
  if pos_pin > -1                                                                                                                           
    dira_[pos_pin >> 5 & 1] |= |< pos_pin                                                                                                   
    ctrb_ := $18000000 + pos_pin & $3F                                                                                                      
    if neg_pin > -1                                                                                                                         
      dira_[neg_pin >> 5 & 1] |= |< neg_pin                                                                                                 
      ctrb_ += $04000000 + (neg_pin & $3F) << 9                                                                                             
                                                                                                                                            
  'If fm offset is valid, ready ctra for pll mode with divide-by-16 (else disabled)                                                         
  if fm_offset > -1                                                                                                                         
    ctra_ := $05800000                                                                                                                      
                                                                                                                                            
  'Ready frqa value for fm offset                                                                                                           
  repeat 33                                                                                                                                 
    frqa_ <<= 1                                                                                                                             
    if fm_offset => clkfreq                                                                                                                 
      fm_offset -= clkfreq                                                                                                                  
      frqa_++                                                                                                                               
    fm_offset <<= 1                                                                                                                         
                                                                                                                                            
  'Ready 20KHz sample period                                                                                                                
  cnt_ := clkfreq / 20_000                                                                                                                  
                                                                                                                                            
  'Launch vocal tract cog                                                                                                                   
  return cog := cognew(@entry, @attenuation) + 1                                                                                            
                                                                                                                                            
                                                                                                                                            
PUB stop                                                                                                                                    
                                                                                                                                            
'' Stop vocal tract driver - frees a cog                                                                                                    
                                                                                                                                            
  'If already running, stop vocal tract cog                                                                                                 
  if cog                                                                                                                                    
    cogstop(cog~ -  1)                                                                                                                      
                                                                                                                                            
  'Reset variables and buffers
  longfill(@index, 0, constant(3 + 6 + frame_buffer_longs))


PUB set_attenuation(level)

'' Set master attenuation level (0..7, initially 0)

  attenuation := level


PUB set_pace(percentage)

'' Set pace to some percentage (initially 100)

  pace := percentage


PUB go(time)

'' Queue current parameters to transition over time
''
''   actual time = integer(time * 100 / pace) #> 2 * 700µs (at least 1400µs, see set_pace)

  'Wait until frame available (first long will be zeroed)
  repeat while frames[index]

  'Load parameters into frame
  bytemove(@frames[index] + 3, tract, 13)

  'Write stepsize into frame (non-0 alerts vocal tract that frame is ready)
  frames[index] |= $01000000 / (time * 100 / pace #> 2)

  'Increment frame index
  index := (index + frame_longs) & constant(frame_buffer_longs - 1)


PUB full : status

'' Returns true if the parameter queue is full
'' (useful for checking if "go" would have to wait) 

  return frames[index]


PUB empty : status | i

'' Returns true if the parameter queue is empty
'' (useful for detecting when the vocal tract is finished)

  repeat i from 0 to constant(frame_buffers - 1)
    if frames[i * frame_longs]
      return {false}
  return true
  
 
PUB sample_ptr : ptr

'' Returns the address of the long which receives the audio samples in real-time
'' (signed 32-bit values updated at 20KHz)

  return @sample
  

PUB aural_id : id

'' Returns the id of the cog executing the vocal tract algorithm
'' (for connecting a broadcast tv driver with the aural subcarrier)

  return cog - 1


DAT

' ┌──────────────────┐
' │  Initialization  │
' └──────────────────┘

entry                   org

:zero                   mov     reserves,#0             'zero all reserved data
                        add     :zero,d0
                        djnz    clear_cnt,#:zero

                        mov     t1,#2*15                'assemble 15 multiply steps into reserves
:minst                  mov     mult_steps,mult_step    '(saves hub memory)
                        add     :minst,d0s0             
                        test    t1,#1           wc
        if_c            sub     :minst,#2
                        djnz    t1,#:minst
                        mov     mult_ret,antilog_ret    'write 'ret' after last instruction

                        mov     t1,#13                  'assemble 13 cordic steps into reserves
:cstep                  mov     t2,#8                   '(saves hub memory)
:cinst                  mov     cordic_steps,cordic_step
                        add     :cinst,d0s0
                        djnz    t2,#:cinst
                        sub     :cinst,#8
                        add     cordic_dx,#1
                        add     cordic_dy,#1
                        add     cordic_a,#1
                        djnz    t1,#:cstep                       
                        mov     cordic_ret,antilog_ret  'write 'ret' over last instruction

                        mov     t1,par                  'get dira/dirb/ctra/ctrb
                        add     t1,#2*4
                        mov     t2,#4
:regs                   rdlong  dira,t1
                        add     t1,#4
                        add     :regs,d0
                        djnz    t2,#:regs

                        rdlong  frqa_center,t1          'get frqa center
                        
                        add     t1,#4                   'get cnt ticks
                        rdlong  cnt_ticks,t1
                        
                        mov     cnt_value,cnt           'prepare for initial waitcnt
                        add     cnt_value,cnt_ticks


' ┌────────────────────┐
' │  Vocal Tract Loop  │
' └────────────────────┘

' Wait for next sample period, then output sample

loop                    waitcnt cnt_value,cnt_ticks     'wait for sample period

                        rdlong  t1,par                  'perform master attenuation
                        sar     x,t1                    

                        mov     t1,x                    'update fm aural subcarrier for tv broadcast
                        sar     t1,#10
                        add     t1,frqa_center
                        mov     frqa,t1

                        mov     t1,x                    'update duty cycle output for pin driving
                        add     t1,h80000000
                        mov     frqb,t1

                        mov     t1,par                  'update sample receiver in main memory
                        add     t1,#1*4
                        wrlong  x,t1

' White noise source

                        test    lfsr,lfsr_taps  wc      'iterate lfsr three times
                        rcl     lfsr,#1
                        test    lfsr,lfsr_taps  wc
                        rcl     lfsr,#1
                        test    lfsr,lfsr_taps  wc
                        rcl     lfsr,#1

' Aspiration

                        mov     t1,aa                   'aspiration amplitude
                        mov     t2,lfsr
                        call    #mult
                        
                        sar     t1,#8                   'set x
                        mov     x,t1

' Vibrato

                        mov     t1,vr                   'vibrato rate
                        shr     t1,#10
                        add     vphase,t1

                        mov     t1,vp                   'vibrato pitch
                        mov     t2,vphase
                        call    #sine
                        
                        add     t1,gp                   'sum glottal pitch (+) into vibrato pitch (+/-)

' Glottal pulse

                        shr     t1,#2                   'divide final pitch by 3 to mesh with                                                                   
                        mov     t2,t1                   '...12 notes/octave musical scale
                        shr     t2,#2                   '(multiply by %0.0101010101010101)
                        add     t1,t2                                                             
                        mov     t2,t1
                        shr     t2,#4
                        add     t1,t2
                        mov     t2,t1
                        shr     t2,#8
                        add     t1,t2

                        add     t1,tune                 'tune scale so that gp=100 produces 110.00Hz (A2)

                        call    #antilog                'convert pitch (log frequency) to phase delta
                        add     gphase,t2              

                        mov     t1,gphase               'convert phase to glottal pulse sample
                        call    #antilog    
                        sub     t2,h40000000
                        mov     t1,ga
                        call    #sine

                        sar     t1,#6                   'add to x
                        add     x,t1

' Vocal tract formants

                        mov     y,#0                    'reset y

                        mov     a,f1                    'formant1, sum and rotate (x,y) 
                        add     x,f1x                   
                        add     y,f1y
                        call    #cordic
                        mov     f1x,x
                        mov     f1y,y

                        mov     a,f2                    'formant2, sum and rotate (x,y) 
                        add     x,f2x                   
                        add     y,f2y
                        call    #cordic
                        mov     f2x,x
                        mov     f2y,y

                        mov     a,f3                    'formant3, sum and rotate (x,y) 
                        add     x,f3x                  
                        add     y,f3y
                        call    #cordic
                        mov     f3x,x
                        mov     f3y,y
               
                        mov     a,f4                    'formant4, sum and rotate (x,y)    
                        add     x,f4x                  
                        add     y,f4y
                        call    #cordic
                        mov     f4x,x
                        mov     f4y,y

' Nasal anti-formant

                        add     nx,x                    'subtract from x (nx negated)

                        mov     a,nf                    'nasal frequency
                        call    #cordic

                        mov     t1,na                   'nasal amplitude
                        mov     t2,x
                        call    #mult
                        
                        mov     x,nx                    'restore x
                        neg     nx,t1                   'negate nx
                        
' Frication

                        mov     t1,lfsr                 'phase noise
                        sar     t1,#3
                        add     fphase,t1
                        sar     t1,#1
                        add     fphase,t1

                        mov     t1,ff                   'frication frequency
                        shr     t1,#1
                        add     fphase,t1

                        mov     t1,fa                   'frication amplitude
                        mov     t2,fphase
                        call    #sine
                        
                        add     x,t1                    'add to x

' Handle frame

                        jmp     :ret                    'run segment of frame handler, return to loop


' ┌─────────────────┐
' │  Frame Handler  │
' └─────────────────┘

:ret                    long    :wait                   'pointer to next frame handler routine


:wait                   jmpret  :ret,#loop              '(6 or 17.5 cycles)
                        mov     frame_ptr,par           'check for next frame
                        add     frame_ptr,#8*4          'point past miscellaneous data
                        add     frame_ptr,frame_index   'point to start of frame
                        rdlong  step_size,frame_ptr     'get stepsize
                        and     step_size,h00FFFFFF  wz 'isolate stepsize and check if not 0
        if_nz           jmp     #:next                  'if not 0, next frame ready
        

                        mov     :final1,:finali         'no frame ready, ready to finalize parameters
                        mov     frame_cnt,#13           'iterate aa..ff

:final                  jmpret  :ret,#loop              '(13.5 or 4 cycles)
:final1                 mov     par_curr,par_next       'current parameter = next parameter
                        add     :final1,d0s0            'update pointers
                        djnz    frame_cnt,#:final       'another parameter?
                        
                        jmp     #:wait                  'check for next frame


:next                   add     step_size,#1            'next frame ready, insure accurate accumulation
                        mov     step_acc,step_size      'initialize step accumulator                    


                        movs    :set1,#par_next         'ready to get parameters and steps for aa..ff
                        movd    :set2,#par_curr   
                        movd    :set3,#par_next
                        movd    :set4,#par_step
                        add     frame_ptr,#3            'point to first parameter
                        mov     frame_cnt,#13           'iterate aa..ff

:set                    jmpret  :ret,#loop              '(19.5 or 46.5 cycles)
                        rdbyte  t1,frame_ptr            'get new parameter
                        shl     t1,#24                  'msb justify
:set1                   mov     t2,par_next             'get next parameter
:set2                   mov     par_curr,t2             'current parameter = next parameter
:set3                   mov     par_next,t1             'next parameter = new parameter
                        sub     t1,t2           wc      'get next-current delta with sign in c            
                        negc    t1,t1                   'make delta absolute (by c, not msb)
                        rcl     vscl,#1         wz, nr  'save sign into nz (vscl unaffected)

                        mov     t2,#8                   'multiply delta by step size
:mult                   shl     t1,#1           wc
        if_c            add     t1,step_size
                        djnz    t2,#:mult

:set4                   negnz    par_step,t1            'set signed step

                        add     :set1,#1                'update pointers for next parameter+step
                        add     :set2,d0
                        add     :set3,d0
                        add     :set4,d0
                        add     frame_ptr,#1
                        djnz    frame_cnt,#:set         'another parameter?


:stepframe              jmpret  :ret,#loop              '(47.5 or 8 cycles)
                        mov     :step1,:stepi           'ready to step parameters
                        mov     frame_cnt,#13           'iterate aa..ff
                        
:step                   jmpret  :ret,#loop              '(3 or 4 cycles)
:step1                  add     par_curr,par_step       'step parameter
                        add     :step1,d0s0             'update pointers for next parameter+step
                        djnz    frame_cnt,#:step        'another parameter?
                        
                        add     step_acc,step_size      'accumulate frame steps
                        test    step_acc,h01000000  wc  'check for frame steps done
        if_nc           jmp     #:stepframe             'another frame step?

        
                        sub     frame_ptr,#frame_bytes  'zero stepsize in frame to signal frame done
                        wrlong  vscl,frame_ptr

                        add     frame_index,#frame_bytes'point to next frame
                        and     frame_index,#frame_buffer_bytes - 1
                        
                        jmp     #:wait                  'check for next frame


:finali                 mov     par_curr,par_next       'instruction used to finalize parameters                 
:stepi                  add     par_curr,par_step       'instruction used to step parameters               


' ┌────────────────────┐
' │  Math Subroutines  │
' └────────────────────┘

' Antilog
'
'   in:         t1 = log (top 4 bits = whole number, next 11 bits = fraction)
'
'   out:        t2 = antilog ($00010000..$FFEA0000)

antilog                 mov     t2,t1                   
                        shr     t2,#16                  'position 11-bit fraction
                        shr     t1,#16+12               'position 4-bit whole number
                        and     t2,h00000FFE            'get table offset
                        or      t2,h0000D000            'get table base
                        rdword  t2,t2                   'lookup fractional antilog
                        or      t2,h00010000            'insert leading bit
                        shl     t2,t1                   'shift up by whole number

antilog_ret             ret


' Scaled sine
'
'   in:         t1 = unsigned scale (15 top bits used)
'               t2 = angle (13 top bits used)
'
'   out:        t1 = 17-bit * 15-bit scaled sine ($80014000..$7FFEC000)

sine                    shr     t2,#32-13               'get 13-bit angle
                        test    t2,h00001000    wz      'get sine quadrant 3|4 into nz
                        test    t2,h00000800    wc      'get sine quadrant 2|4 into c
                        negc    t2,t2                   'if sine quadrant 2|4, negate table offset
                        or      t2,h00007000            'insert sine table base address >> 1
                        shl     t2,#1                   'shift left to get final word address
                        rdword  t2,t2                   'read sine word from table
                        negnz   t2,t2                   'if quadrant 3|4, negate word
                        shl     t2,#15                  'msb-justify result
                                                        'multiply follows...

' Multiply
'                                                                                             
'   in:         t1 = unsigned multiplier (15 top bits used)
'               t2 = signed multiplicand (17 top bits used)
'
'   out:        t1 = 32-bit signed product

mult                    shr     t1,#32-15               'position unsigned multiplier

                        sar     t2,#15                  'position signed multiplicand
                        shl     t2,#15-1

                        jmp     #mult_steps             'do multiply steps


mult_step               sar     t1,#1           wc      'multiply step that gets assembled into reserves (x15)
        if_c            add     t1,t2                   


' Cordic rotation
'
'   in:          a = 0 to <90 degree angle (~13 top bits used)
'              x,y = signed coordinates     
'
'   out:       x,y = scaled and rotated signed coordinates

cordic                  sar     x,#1                    'multiply (x,y) by %0.10011001 (0.60725 * 0.984)                   
                        mov     t1,x                    '...for cordic pre-scaling and slight damping         
                        sar     t1,#3
                        add     x,t1
                        mov     t1,x
                        sar     t1,#4
                        add     x,t1
                        
                        sar     y,#1
                        mov     t1,y
                        sar     t1,#3
                        add     y,t1
                        mov     t1,y
                        sar     t1,#4
                        add     y,t1

                        mov     t1,x                    'do first cordic step
                        sub     x,y
                        add     y,t1
                        sub     a,h80000000     wc

                        jmp     #cordic_steps+1         'do subsequent cordic steps (skip first instruction)


cordic_step             mov     a,a             wc      'cordic step that gets assembled into reserves (x13)
                        mov     t1,y                    
cordic_dx               sar     t1,#1                   '(source incremented for each step)
                        mov     t2,x
cordic_dy               sar     t2,#1                   '(source incremented for each step)
                        sumnc   x,t1
                        sumc    y,t2
cordic_a                sumnc   a,cordic_delta          '(source incremented for each step)


' ┌────────────────┐
' │  Defined Data  │
' └────────────────┘

tune                    long    $66920000               'scale tuned to 110.00Hz at gp=100 (manually calibrated)

lfsr                    long    1                       'linear feedback shift register for noise generation
lfsr_taps               long    $80061000

cordic_delta            long    $4B901476               'cordic angle deltas (first is h80000000)
                        long    $27ECE16D
                        long    $14444750
                        long    $0A2C350C
                        long    $05175F85
                        long    $028BD879                                       
                        long    $0145F154
                        long    $00A2F94D
                        long    $00517CBB
                        long    $0028BE60
                        long    $00145F30
                        long    $000A2F98

h80000000               long    $80000000               'miscellaneous constants greater than 9 bits
h40000000               long    $40000000
h01000000               long    $01000000
h00FFFFFF               long    $00FFFFFF
h00010000               long    $00010000        
h0000D000               long    $0000D000
h00007000               long    $00007000
h00001000               long    $00001000
h00000FFE               long    $00000FFE
h00000800               long    $00000800

d0                      long    $00000200               'destination/source field increments
d0s0                    long    $00000201

clear_cnt               long    $1F0 - reserves         'number of reserved registers to clear on startup


' ┌──────────────────────────────────────────────────┐
' │  Undefined Data (zeroed by initialization code)  │
' └──────────────────────────────────────────────────┘

reserves

frqa_center             res     1                       'reserved registers that get cleared on startup

cnt_ticks               res     1
cnt_value               res     1

frame_index             res     1
frame_ptr               res     1
frame_cnt               res     1

step_size               res     1
step_acc                res     1
                                                
vphase                  res     1
gphase                  res     1
fphase                  res     1

f1x                     res     1
f1y                     res     1
f2x                     res     1
f2y                     res     1
f3x                     res     1
f3y                     res     1
f4x                     res     1
f4y                     res     1
nx                      res     1

a                       res     1
x                       res     1
y                       res     1

t1                      res     1
t2                      res     1

par_curr                                                '*** current parameters
aa                      res     1                       'aspiration amplitude
ga                      res     1                       'glottal amplitude       
gp                      res     1                       'glottal pitch
vp                      res     1                       'vibrato pitch           
vr                      res     1                       'vibrato rate            
f1                      res     1                       'formant1 frequency      
f2                      res     1                       'formant2 frequency
f3                      res     1                       'formant3 frequency
f4                      res     1                       'formant4 frequency
na                      res     1                       'nasal amplitude
nf                      res     1                       'nasal frequency
fa                      res     1                       'frication amplitude
ff                      res     1                       'frication frequency

par_next                res     13                      '*** next parameters
par_step                res     13                      '*** parameter steps


mult_steps              res     2 * 15                  'assembly area for multiply steps w/ret
mult_ret
sine_ret                res     1

cordic_steps            res     8 * 13 - 1              'assembly area for cordic steps w/ret
cordic_ret              res     1

{{

┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}