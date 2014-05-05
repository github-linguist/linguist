''***************************************
''*  TV Driver v1.1                     *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2004 Parallax, Inc.  *               
''*  See end of file for terms of use.  *               
''***************************************

' v1.0 - 01 May 2006 - original version
' v1.1 - 17 May 2006 - pixel tile size can now be 16 x 32 to enable more efficient
'                       character displays utilizing the internal font - see 'tv_mode'


CON

  fntsc         = 3_579_545     'NTSC color frequency
  lntsc         = 3640          'NTSC color cycles per line * 16
  sntsc         = 624           'NTSC color cycles per sync * 16

  fpal          = 4_433_618     'PAL color frequency
  lpal          = 4540          'PAL color cycles per line * 16
  spal          = 848           'PAL color cycles per sync * 16

  paramcount    = 14
  colortable    = $180          'start of colortable inside cog
  

VAR

  long  cog


PUB start(tvptr) : okay

'' Start TV driver - starts a cog
'' returns false if no cog available
''
''   tvptr = pointer to TV parameters

  stop
  okay := cog := cognew(@entry, tvptr) + 1


PUB stop

'' Stop TV driver - frees a cog

  if cog
    cogstop(cog~ - 1)


DAT

'*******************************
'* Assembly language TV driver *
'*******************************

                        org
'
'
' Entry
'
entry                   mov     taskptr,#tasks          'reset tasks

                        mov     x,#10                   'perform task sections initially
:init                   jmpret  taskret,taskptr
                        djnz    x,#:init
'
'
' Superfield
'
superfield              mov     taskptr,#tasks          'reset tasks

                        test    _mode,#%0001    wc      'if ntsc, set phaseflip
        if_nc           mov     phaseflip,phasemask

                        test    _mode,#%0010    wz      'get interlace into nz
'
'
' Field
'
field                   mov     x,vinv                  'do invisible back porch lines
:black                  call    #hsync                  'do hsync
                        waitvid burst,sync_high2        'do black
                        jmpret  taskret,taskptr         'call task section (z undisturbed)
                        djnz    x,#:black               'another black line?

                        wrlong  visible,par             'set status to visible

                        mov     x,vb                    'do visible back porch lines
                        call    #blank_lines

                        mov     screen,_screen          'point to first tile (upper-leftmost)
                        mov     y,_vt                   'set vertical tiles
:line                   mov     vx,_vx                  'set vertical expand
:vert   if_z            xor     interlace,#1            'interlace skip?
        if_z            tjz     interlace,#:skip

                        call    #hsync                  'do hsync

                        mov     vscl,hb                 'do visible back porch pixels
                        xor     tile,colortable
                        waitvid tile,#0

                        mov     x,_ht                   'set horizontal tiles
                        mov     vscl,hx                 'set horizontal expand

:tile                   rdword  tile,screen             'read tile
                        or      tile,line               'set pointer bits into tile
                        rol     tile,#6                 'read tile pixels
                        rdlong  pixels,tile             '(2 instructions between reads)
                        shr     tile,#10+6              'set tile colors
                        movs    :color,tile
                        add     screen,#2               'point to next tile
                        mov     tile,phaseflip
:color                  xor     tile,colortable
                        waitvid tile,pixels             'pass colors and pixels to video
                        djnz    x,#:tile                'another tile?

                        sub     screen,hc2x             'repoint to first tile in same line

                        mov     vscl,hf                 'do visible front porch pixels
                        mov     tile,phaseflip
                        xor     tile,colortable
                        waitvid tile,#0

:skip                   djnz    vx,#:vert               'vertical expand?
                        ror     line,linerot            'set next line
                        add     line,lineadd    wc
                        rol     line,linerot      
        if_nc           jmp     #:line
                        add     screen,hc2x             'point to first tile in next line
                        djnz    y,#:line                'another tile line?

        if_z            xor     interlace,#1    wz      'get interlace and field1 into z

                        test    _mode,#%0001    wc      'do visible front porch lines
                        mov     x,vf
        if_nz_and_c     add     x,#1
                        call    #blank_lines

        if_nz           wrlong  invisible,par           'unless interlace and field1, set status to invisible

        if_z_eq_c       call    #hsync                  'if required, do short line
        if_z_eq_c       mov     vscl,hrest
        if_z_eq_c       waitvid burst,sync_high2
        if_z_eq_c       xor     phaseflip,phasemask

                        call    #vsync_high             'do high vsync pulses

                        movs    vsync1,#sync_low1       'do low vsync pulses
                        movs    vsync2,#sync_low2
                        call    #vsync_low

                        call    #vsync_high             'do high vsync pulses

        if_nz           mov     vscl,hhalf              'if odd frame, do half line
        if_nz           waitvid burst,sync_high2

        if_z            jmp     #field                  'if interlace and field1, display field2
                        jmp     #superfield             'else, new superfield
'
'
' Blank lines
'
blank_lines             call    #hsync                  'do hsync

                        xor     tile,colortable         'do background
                        waitvid tile,#0

                        djnz    x,#blank_lines

blank_lines_ret         ret
'
'
' Horizontal sync
'
hsync                   test    _mode,#%0001    wc      'if pal, toggle phaseflip
        if_c            xor     phaseflip,phasemask

                        mov     vscl,sync_scale1        'do hsync       
                        mov     tile,phaseflip
                        xor     tile,burst
                        waitvid tile,sync_normal

                        mov     vscl,hvis               'setup in case blank line
                        mov     tile,phaseflip

hsync_ret               ret
'
'
' Vertical sync
'
vsync_high              movs    vsync1,#sync_high1      'vertical sync
                        movs    vsync2,#sync_high2

vsync_low               mov     x,vrep

vsyncx                  mov     vscl,sync_scale1
vsync1                  waitvid burst,sync_high1

                        mov     vscl,sync_scale2
vsync2                  waitvid burst,sync_high2

                        djnz    x,#vsyncx
vsync_low_ret
vsync_high_ret          ret
'
'
' Tasks - performed in sections during invisible back porch lines
'
tasks                   mov     t1,par                  'load parameters
                        movd    :par,#_enable           '(skip _status)
                        mov     t2,#paramcount - 1
:load                   add     t1,#4
:par                    rdlong  0,t1
                        add     :par,d0
                        djnz    t2,#:load               '+119

                        mov     t1,_pins                'set video pins and directions
                        test    t1,#$08         wc
        if_nc           mov     t2,pins0
        if_c            mov     t2,pins1
                        test    t1,#$40         wc
                        shr     t1,#1
                        shl     t1,#3
                        shr     t2,t1
                        movs    vcfg,t2
                        shr     t1,#6
                        movd    vcfg,t1
                        shl     t1,#3
                        and     t2,#$FF
                        shl     t2,t1
        if_nc           mov     dira,t2
        if_nc           mov     dirb,#0
        if_c            mov     dira,#0
        if_c            mov     dirb,t2                 '+18

                        tjz     _enable,#disabled       '+2, disabled?

                        jmpret  taskptr,taskret         '+1=140, break and return later

                        movs    :rd,#wtab               'load ntsc/pal metrics from word table
                        movd    :wr,#hvis
                        mov     t1,#wtabx - wtab
                        test    _mode,#%0001    wc
:rd                     mov     t2,0
                        add     :rd,#1
        if_nc           shl     t2,#16
                        shr     t2,#16
:wr                     mov     0,t2
                        add     :wr,d0
                        djnz    t1,#:rd                 '+54

        if_nc           movs    :ltab,#ltab             'load ntsc/pal metrics from long table
        if_c            movs    :ltab,#ltab+1
                        movd    :ltab,#fcolor
                        mov     t1,#(ltabx - ltab) >> 1
:ltab                   mov     0,0
                        add     :ltab,d0s1
                        djnz    t1,#:ltab               '+17

                        rdlong  t1,#0                   'get CLKFREQ
                        shr     t1,#1                   'if CLKFREQ < 16MHz, cancel _broadcast
                        cmp     t1,m8           wc
        if_c            mov     _broadcast,#0
                        shr     t1,#1                   'if CLKFREQ < color frequency * 4, disable
                        cmp     t1,fcolor       wc
        if_c            jmp     #disabled               '+11

                        jmpret  taskptr,taskret         '+1=83, break and return later

                        mov     t1,fcolor               'set ctra pll to fcolor * 16
                        call    #divide                 'if ntsc, set vco to fcolor * 32 (114.5454 MHz)
                        test    _mode,#%0001    wc      'if pal, set vco to fcolor * 16 (70.9379 MHz)
        if_c            movi    ctra,#%00001_111        'select fcolor * 16 output (ntsc=/2, pal=/1)
        if_nc           movi    ctra,#%00001_110
        if_nc           shl     t2,#1
                        mov     frqa,t2                 '+147

                        jmpret  taskptr,taskret         '+1=148, break and return later

                        mov     t1,_broadcast           'set ctrb pll to _broadcast
                        mov     t2,#0                   'if 0, turn off ctrb
                        tjz     t1,#:off
                        min     t1,m8                   'limit from 8MHz to 128MHz
                        max     t1,m128
                        mov     t2,#%00001_100          'adjust _broadcast to be within 4MHz-8MHz
:scale                  shr     t1,#1                   '(vco will be within 64MHz-128MHz)
                        cmp     m8,t1           wc
        if_c            add     t2,#%00000_001
        if_c            jmp     #:scale
:off                    movi    ctrb,t2
                        call    #divide
                        mov     frqb,t2                 '+165

                        jmpret  taskptr,taskret         '+1=166, break and return later

                        mov     t1,#%10100_000          'set video configuration
                        test    _pins,#$01      wc      '(swap broadcast/baseband output bits?)
        if_c            or      t1,#%01000_000
                        test    _mode,#%1000    wc      '(strip chroma from broadcast?)
        if_nc           or      t1,#%00010_000
                        test    _mode,#%0100    wc      '(strip chroma from baseband?)
        if_nc           or      t1,#%00001_000
                        and     _auralcog,#%111         '(set aural cog)
                        or      t1,_auralcog
                        movi    vcfg,t1                 '+10

                        mov     hx,_hx                  'compute horizontal metrics
                        shl     hx,#8
                        or      hx,_hx
                        shl     hx,#4

                        mov     hc2x,_ht
                        shl     hc2x,#1

                        mov     t1,_ht
                        mov     t2,_hx
                        call    #multiply
                        mov     hf,hvis
                        sub     hf,t1
                        shr     hf,#1           wc
                        mov     hb,_ho
                        addx    hb,hf
                        sub     hf,_ho                  '+52

                        mov     t1,_vt                  'compute vertical metrics
                        mov     t2,_vx
                        call    #multiply
                        test    _mode,#%10000   wc      'consider tile size
                        muxc    linerot,#1
                        mov     lineadd,lineinc
        if_c            shr     lineadd,#1
        if_c            shl     t1,#1
                        test    _mode,#%0010    wc      'consider interlace
        if_c            shr     t1,#1
                        mov     vf,vvis
                        sub     vf,t1
                        shr     vf,#1           wc
                        neg     vb,_vo
                        addx    vb,vf
                        add     vf,_vo                  '+53

                        xor     _mode,#%0010            '+1, flip interlace bit for display

:colors                 jmpret  taskptr,taskret         '+1=117/160, break and return later

                        mov     t1,#13                  'load next 13 colors into colortable
:colorloop              mov     t2,:colorreg            '5 times = 65 (all 64 colors loaded)
                        shr     t2,#9-2
                        and     t2,#$FC
                        add     t2,_colors
:colorreg               rdlong  colortable,t2
                        add     :colorreg,d0
                        andn    :colorreg,d6
                        djnz    t1,#:colorloop          '+158

                        jmp     #:colors                '+1, keep loading colors
'
'
' Divide t1/CLKFREQ to get frqa or frqb value into t2
'
divide                  rdlong  m1,#0                   'get CLKFREQ

                        mov     m2,#32+1
:loop                   cmpsub  t1,m1           wc
                        rcl     t2,#1
                        shl     t1,#1
                        djnz    m2,#:loop

divide_ret              ret                             '+140
'
'
' Multiply t1 * t2 * 16 (t1, t2 = bytes)
'
multiply                shl     t2,#8+4-1

                        mov     m1,#8
:loop                   shr     t1,#1           wc
        if_c            add     t1,t2
                        djnz    m1,#:loop

multiply_ret            ret                             '+37
'
'
' Disabled - reset status, nap ~4ms, try again
'
disabled                mov     ctra,#0                 'reset ctra
                        mov     ctrb,#0                 'reset ctrb
                        mov     vcfg,#0                 'reset video

                        wrlong  outa,par                'set status to disabled

                        rdlong  t1,#0                   'get CLKFREQ
                        shr     t1,#8                   'nap for ~4ms
                        min     t1,#3
                        add     t1,cnt
                        waitcnt t1,#0

                        jmp     #entry                  'reload parameters
'
'
' Initialized data
'
m8                      long    8_000_000
m128                    long    128_000_000
d0                      long    1 << 9 << 0
d6                      long    1 << 9 << 6
d0s1                    long    1 << 9 << 0 + 1 << 1
interlace               long    0
invisible               long    1
visible                 long    2
phaseflip               long    $00000000
phasemask               long    $F0F0F0F0
line                    long    $00060000
lineinc                 long    $10000000
linerot                 long    0
pins0                   long    %11110000_01110000_00001111_00000111
pins1                   long    %11111111_11110111_01111111_01110111
sync_high1              long    %0101010101010101010101_101010_0101
sync_high2              long    %01010101010101010101010101010101       'used for black
sync_low1               long    %1010101010101010101010101010_0101
sync_low2               long    %01_101010101010101010101010101010
'
'
' NTSC/PAL metrics tables
'                               ntsc                    pal
'                               ----------------------------------------------
wtab                    word    lntsc - sntsc,          lpal - spal     'hvis
                        word    lntsc / 2 - sntsc,      lpal / 2 - spal 'hrest
                        word    lntsc / 2,              lpal / 2        'hhalf
                        word    243,                    286             'vvis
                        word    10,                     18              'vinv
                        word    6,                      5               'vrep
                        word    $02_8A,                 $02_AA          'burst
wtabx
ltab                    long    fntsc                                   'fcolor
                        long    fpal
                        long    sntsc >> 4 << 12 + sntsc                'sync_scale1
                        long    spal >> 4 << 12 + spal
                        long    67 << 12 + lntsc / 2 - sntsc            'sync_scale2
                        long    79 << 12 + lpal / 2 - spal
                        long    %0101_00000000_01_10101010101010_0101   'sync_normal
                        long    %010101_00000000_01_101010101010_0101
ltabx
'
'
' Uninitialized data
'
taskptr                 res     1                       'tasks
taskret                 res     1
t1                      res     1
t2                      res     1
m1                      res     1
m2                      res     1

x                       res     1                       'display
y                       res     1
hf                      res     1
hb                      res     1
vf                      res     1
vb                      res     1
hx                      res     1
vx                      res     1
hc2x                    res     1
screen                  res     1
tile                    res     1
pixels                  res     1
lineadd                 res     1

hvis                    res     1                       'loaded from word table
hrest                   res     1
hhalf                   res     1
vvis                    res     1
vinv                    res     1
vrep                    res     1
burst                   res     1

fcolor                  res     1                       'loaded from long table
sync_scale1             res     1
sync_scale2             res     1
sync_normal             res     1
'
'
' Parameter buffer
'
_enable                 res     1       '0/non-0        read-only
_pins                   res     1       '%pppmmmm       read-only
_mode                   res     1       '%tccip         read-only
_screen                 res     1       '@word          read-only
_colors                 res     1       '@long          read-only
_ht                     res     1       '1+             read-only
_vt                     res     1       '1+             read-only
_hx                     res     1       '4+             read-only
_vx                     res     1       '1+             read-only
_ho                     res     1       '0+-            read-only
_vo                     res     1       '0+-            read-only
_broadcast              res     1       '0+             read-only
_auralcog               res     1       '0-7            read-only

                        fit     colortable              'fit underneath colortable ($180-$1BF)
''
''___
''VAR                   'TV parameters - 14 contiguous longs
''
''  long  tv_status     '0/1/2 = off/invisible/visible              read-only
''  long  tv_enable     '0/non-0 = off/on                           write-only
''  long  tv_pins       '%pppmmmm = pin group, pin group mode       write-only
''  long  tv_mode       '%tccip = tile,chroma,interlace,ntsc/pal    write-only
''  long  tv_screen     'pointer to screen (words)                  write-only      
''  long  tv_colors     'pointer to colors (longs)                  write-only                            
''  long  tv_ht         'horizontal tiles                           write-only                            
''  long  tv_vt         'vertical tiles                             write-only                            
''  long  tv_hx         'horizontal tile expansion                  write-only                            
''  long  tv_vx         'vertical tile expansion                    write-only                            
''  long  tv_ho         'horizontal offset                          write-only                            
''  long  tv_vo         'vertical offset                            write-only                            
''  long  tv_broadcast  'broadcast frequency (Hz)                   write-only                            
''  long  tv_auralcog   'aural fm cog                               write-only                            
''                                                                                              
''The preceding VAR section may be copied into your code.        
''After setting variables, do start(@tv_status) to start driver. 
''                                                               
''All parameters are reloaded each superframe, allowing you to make live
''changes. To minimize flicker, correlate changes with tv_status.
''
''Experimentation may be required to optimize some parameters.
''
''Parameter descriptions:
''  _________
''  tv_status
''
''    driver sets this to indicate status:
''      0: driver disabled (tv_enable = 0 or CLKFREQ < requirement)
''      1: currently outputting invisible sync data
''      2: currently outputting visible screen data
''  _________
''  tv_enable
''
''        0: disable (pins will be driven low, reduces power)
''    non-0: enable
''  _______
''  tv_pins
''
''    bits 6..4 select pin group:
''      %000: pins 7..0
''      %001: pins 15..8
''      %010: pins 23..16
''      %011: pins 31..24
''      %100: pins 39..32
''      %101: pins 47..40
''      %110: pins 55..48
''      %111: pins 63..56
''
''    bits 3..0 select pin group mode:
''      %0000: %0000_0111    -                    baseband
''      %0001: %0000_0111    -                    broadcast
''      %0010: %0000_1111    -                    baseband + chroma
''      %0011: %0000_1111    -                    broadcast + aural
''      %0100: %0111_0000    broadcast            -
''      %0101: %0111_0000    baseband             -
''      %0110: %1111_0000    broadcast + aural    -
''      %0111: %1111_0000    baseband + chroma    -
''      %1000: %0111_0111    broadcast            baseband
''      %1001: %0111_0111    baseband             broadcast
''      %1010: %0111_1111    broadcast            baseband + chroma
''      %1011: %0111_1111    baseband             broadcast + aural
''      %1100: %1111_0111    broadcast + aural    baseband
''      %1101: %1111_0111    baseband + chroma    broadcast
''      %1110: %1111_1111    broadcast + aural    baseband + chroma
''      %1111: %1111_1111    baseband + chroma    broadcast + aural
''      -----------------------------------------------------------
''            active pins    top nibble           bottom nibble
''
''      the baseband signal nibble is arranged as:
''        bit 3: chroma signal for s-video (attach via 560-ohm resistor)
''        bits 2..0: baseband video (sum 270/560/1100-ohm resistors to form 75-ohm 1V signal)
''
''      the broadcast signal nibble is arranged as:
''        bit 3: aural subcarrier (sum 560-ohm resistor into network below)
''        bits 2..0: visual carrier (sum 270/560/1100-ohm resistors to form 75-ohm 1V signal)
''  _______
''  tv_mode
''
''    bit 4 selects between 16x16 and 16x32 pixel tiles:
''      0: 16x16 pixel tiles (tileheight = 16)
''      1: 16x32 pixel tiles (tileheight = 32)
''
''    bit 3 controls chroma mixing into broadcast:
''      0: mix chroma into broadcast (color)
''      1: strip chroma from broadcast (black/white)
''
''    bit 2 controls chroma mixing into baseband:
''      0: mix chroma into baseband (composite color)
''      1: strip chroma from baseband (black/white or s-video)
''
''    bit 1 controls interlace:
''      0: progressive scan (243 display lines for NTSC, 286 for PAL)
''           less flicker, good for motion
''      1: interlaced scan (486 display lines for NTSC, 572 for PAL)
''           doubles the vertical display lines, good for text
''
''    bit 0 selects NTSC or PAL format
''      0: NTSC
''           3016 horizontal display ticks
''           243 or 486 (interlaced) vertical display lines
''           CLKFREQ must be at least 14_318_180 (4 * 3_579_545 Hz)*
''      1: PAL
''           3692 horizontal display ticks
''           286 or 572 (interlaced) vertical display lines
''           CLKFREQ must be at least 17_734_472 (4 * 4_433_618 Hz)*
''
''      * driver will disable itself while CLKFREQ is below requirement
''  _________
''  tv_screen
''
''    pointer to words which define screen contents (left-to-right, top-to-bottom)
''      number of words must be tv_ht * tv_vt
''      each word has two bitfields: a 6-bit colorset ptr and a 10-bit pixelgroup ptr
''        bits 15..10: select the colorset* for the associated pixel tile
''        bits 9..0: select the pixelgroup** address %ppppppppppcccc00 (p=address, c=0..15)
''
''       * colorsets are longs which each define four 8-bit colors
''
''      ** pixelgroups are <tileheight> longs which define (left-to-right, top-to-bottom) the 2-bit
''         (four color) pixels that make up a 16x16 or a 32x32 pixel tile
''  _________
''  tv_colors
''
''    pointer to longs which define colorsets
''      number of longs must be 1..64
''      each long has four 8-bit fields which define colors for 2-bit (four color) pixels
''      first long's bottom color is also used as the screen background color
''      8-bit color fields are as follows:
''        bits 7..4: chroma data (0..15 = blue..green..red..)*
''        bit 3: controls chroma modulation (0=off, 1=on)
''        bits 2..0: 3-bit luminance level:
''          values 0..1: reserved for sync - don't use
''          values 2..7: valid luminance range, modulation adds/subtracts 1 (beware of 7)
''          value 0 may be modulated to produce a saturated color toggling between levels 1 and 7
''
''      * because of TV's limitations, it doesn't look good when chroma changes abruptly -
''        rather, use luminance - change chroma only against a black or white background for
''        best appearance
''  _____
''  tv_ht
''
''    horizontal number pixel tiles - must be at least 1
''    practical limit is 40 for NTSC, 50 for PAL
''  _____
''  tv_vt
''
''    vertical number of pixel tiles - must be at least 1
''    practical limit is 13 for NTSC, 15 for PAL (26/30 max for interlaced NTSC/PAL)
''  _____
''  tv_hx
''
''    horizontal tile expansion factor - must be at least 3 for NTSC, 4 for PAL
''
''    make sure 16 * tv_ht * tv_hx + ||tv_ho + 32 is less than the horizontal display ticks
''  _____
''  tv_vx
''
''    vertical tile expansion factor - must be at least 1
''
''    make sure <tileheight> * tv_vt * tv_vx + ||tv_vo + 1 is less than the display lines
''  _____
''  tv_ho
''
''    horizontal offset in ticks - pos/neg value (0 for centered image)
''    shifts the display right/left
''  _____
''  tv_vo
''
''    vertical offset in lines - pos/neg value (0 for centered image)
''    shifts the display up/down
''  ____________
''  tv_broadcast
''
''    broadcast frequency expressed in Hz (ie channel 2 is 55_250_000)
''    if 0, modulator is turned off - saves power
''
''    broadcasting requires CLKFREQ to be at least 16_000_000
''    while CLKFREQ is below 16_000_000, modulator will be turned off
''  ___________
''  tv_auralcog
''
''    selects cog to supply aural fm signal - 0..7
''    uses ctra pll output from selected cog
''
''    in NTSC, the offset frequency must be 4.5MHz and the max bandwidth +-25KHz
''    in PAL, the offset frequency and max bandwidth vary by PAL type

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