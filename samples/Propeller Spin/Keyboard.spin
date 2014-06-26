''***************************************
''*  PS/2 Keyboard Driver v1.0.1        *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2004 Parallax, Inc.  *
''*  See end of file for terms of use.  *
''***************************************

{-----------------REVISION HISTORY-----------------
 v1.0.1 - Updated 6/15/2006 to work with Propeller Tool 0.96}
 
VAR

  long  cog

  long  par_tail        'key buffer tail        read/write      (19 contiguous longs)
  long  par_head        'key buffer head        read-only
  long  par_present     'keyboard present       read-only
  long  par_states[8]   'key states (256 bits)  read-only
  long  par_keys[8]     'key buffer (16 words)  read-only       (also used to pass initial parameters)


PUB start(dpin, cpin) : okay

'' Start keyboard driver - starts a cog
'' returns false if no cog available
''
''   dpin  = data signal on PS/2 jack
''   cpin  = clock signal on PS/2 jack
''
''     use 100-ohm resistors between pins and jack
''     use 10K-ohm resistors to pull jack-side signals to VDD
''     connect jack-power to 5V, jack-gnd to VSS
''
'' all lock-keys will be enabled, NumLock will be initially 'on',
'' and auto-repeat will be set to 15cps with a delay of .5s

  okay := startx(dpin, cpin, %0_000_100, %01_01000)


PUB startx(dpin, cpin, locks, auto) : okay

'' Like start, but allows you to specify lock settings and auto-repeat
''
''   locks = lock setup
''           bit 6 disallows shift-alphas (case set soley by CapsLock)
''           bits 5..3 disallow toggle of NumLock/CapsLock/ScrollLock state
''           bits 2..0 specify initial state of NumLock/CapsLock/ScrollLock
''           (eg. %0_001_100 = disallow ScrollLock, NumLock initially 'on')
''
''   auto  = auto-repeat setup
''           bits 6..5 specify delay (0=.25s, 1=.5s, 2=.75s, 3=1s)
''           bits 4..0 specify repeat rate (0=30cps..31=2cps)
''           (eg %01_00000 = .5s delay, 30cps repeat)

  stop
  longmove(@par_keys, @dpin, 4)
  okay := cog := cognew(@entry, @par_tail) + 1


PUB stop

'' Stop keyboard driver - frees a cog

  if cog
    cogstop(cog~ -  1)
  longfill(@par_tail, 0, 19)


PUB present : truefalse

'' Check if keyboard present - valid ~2s after start
'' returns t|f

  truefalse := -par_present


PUB key : keycode

'' Get key (never waits)
'' returns key (0 if buffer empty)

  if par_tail <> par_head
    keycode := par_keys.word[par_tail]
    par_tail := ++par_tail & $F


PUB getkey : keycode

'' Get next key (may wait for keypress)
'' returns key

  repeat until (keycode := key)


PUB newkey : keycode

'' Clear buffer and get new key (always waits for keypress)
'' returns key

  par_tail := par_head
  keycode := getkey


PUB gotkey : truefalse

'' Check if any key in buffer
'' returns t|f

  truefalse := par_tail <> par_head


PUB clearkeys

'' Clear key buffer

  par_tail := par_head


PUB keystate(k) : state

'' Get the state of a particular key
'' returns t|f

  state := -(par_states[k >> 5] >> k & 1)


DAT

'******************************************
'* Assembly language PS/2 keyboard driver *
'******************************************

                        org
'
'
' Entry
'
entry                   movd    :par,#_dpin             'load input parameters _dpin/_cpin/_locks/_auto
                        mov     x,par
                        add     x,#11*4
                        mov     y,#4
:par                    rdlong  0,x
                        add     :par,dlsb
                        add     x,#4
                        djnz    y,#:par

                        mov     dmask,#1                'set pin masks
                        shl     dmask,_dpin
                        mov     cmask,#1
                        shl     cmask,_cpin

                        test    _dpin,#$20      wc      'modify port registers within code
                        muxc    _d1,dlsb
                        muxc    _d2,dlsb
                        muxc    _d3,#1
                        muxc    _d4,#1
                        test    _cpin,#$20      wc
                        muxc    _c1,dlsb
                        muxc    _c2,dlsb
                        muxc    _c3,#1

                        mov     _head,#0                'reset output parameter _head
'
'
' Reset keyboard
'
reset                   mov     dira,#0                 'reset directions
                        mov     dirb,#0

                        movd    :par,#_present          'reset output parameters _present/_states[8]
                        mov     x,#1+8
:par                    mov     0,#0
                        add     :par,dlsb
                        djnz    x,#:par

                        mov     stat,#8                 'set reset flag
'
'
' Update parameters
'
update                  movd    :par,#_head             'update output parameters _head/_present/_states[8]
                        mov     x,par
                        add     x,#1*4
                        mov     y,#1+1+8
:par                    wrlong  0,x
                        add     :par,dlsb
                        add     x,#4
                        djnz    y,#:par

                        test    stat,#8         wc      'if reset flag, transmit reset command
        if_c            mov     data,#$FF
        if_c            call    #transmit
'
'
' Get scancode
'
newcode                 mov     stat,#0                 'reset state

:same                   call    #receive                'receive byte from keyboard

                        cmp     data,#$83+1     wc      'scancode?

        if_nc           cmp     data,#$AA       wz      'powerup/reset?
        if_nc_and_z     jmp     #configure

        if_nc           cmp     data,#$E0       wz      'extended?
        if_nc_and_z     or      stat,#1
        if_nc_and_z     jmp     #:same

        if_nc           cmp     data,#$F0       wz      'released?
        if_nc_and_z     or      stat,#2
        if_nc_and_z     jmp     #:same

        if_nc           jmp     #newcode                'unknown, ignore
'
'
' Translate scancode and enter into buffer
'
                        test    stat,#1         wc      'lookup code with extended flag
                        rcl     data,#1
                        call    #look

                        cmp     data,#0         wz      'if unknown, ignore
        if_z            jmp     #newcode

                        mov     t,_states+6             'remember lock keys in _states

                        mov     x,data                  'set/clear key bit in _states
                        shr     x,#5
                        add     x,#_states
                        movd    :reg,x
                        mov     y,#1
                        shl     y,data
                        test    stat,#2         wc
:reg                    muxnc   0,y

        if_nc           cmpsub  data,#$F0       wc      'if released or shift/ctrl/alt/win, done
        if_c            jmp     #update

                        mov     y,_states+7             'get shift/ctrl/alt/win bit pairs
                        shr     y,#16

                        cmpsub  data,#$E0       wc      'translate keypad, considering numlock
        if_c            test    _locks,#%100    wz
        if_c_and_z      add     data,#@keypad1-@table
        if_c_and_nz     add     data,#@keypad2-@table
        if_c            call    #look
        if_c            jmp     #:flags

                        cmpsub  data,#$DD       wc      'handle scrlock/capslock/numlock
        if_c            mov     x,#%001_000
        if_c            shl     x,data
        if_c            andn    x,_locks
        if_c            shr     x,#3
        if_c            shr     t,#29                   'ignore auto-repeat
        if_c            andn    x,t             wz
        if_c            xor     _locks,x
        if_c            add     data,#$DD
        if_c_and_nz     or      stat,#4                 'if change, set configure flag to update leds

                        test    y,#%11          wz      'get shift into nz

        if_nz           cmp     data,#$60+1     wc      'check shift1
        if_nz_and_c     cmpsub  data,#$5B       wc
        if_nz_and_c     add     data,#@shift1-@table
        if_nz_and_c     call    #look
        if_nz_and_c     andn    y,#%11

        if_nz           cmp     data,#$3D+1     wc      'check shift2
        if_nz_and_c     cmpsub  data,#$27       wc
        if_nz_and_c     add     data,#@shift2-@table
        if_nz_and_c     call    #look
        if_nz_and_c     andn    y,#%11

                        test    _locks,#%010    wc      'check shift-alpha, considering capslock
                        muxnc   :shift,#$20
                        test    _locks,#$40     wc
        if_nz_and_nc    xor     :shift,#$20
                        cmp     data,#"z"+1     wc
        if_c            cmpsub  data,#"a"       wc
:shift  if_c            add     data,#"A"
        if_c            andn    y,#%11

:flags                  ror     data,#8                 'add shift/ctrl/alt/win flags
                        mov     x,#4                    '+$100 if shift
:loop                   test    y,#%11          wz      '+$200 if ctrl
                        shr     y,#2                    '+$400 if alt
        if_nz           or      data,#1                 '+$800 if win
                        ror     data,#1
                        djnz    x,#:loop
                        rol     data,#12

                        rdlong  x,par                   'if room in buffer and key valid, enter
                        sub     x,#1
                        and     x,#$F
                        cmp     x,_head         wz
        if_nz           test    data,#$FF       wz
        if_nz           mov     x,par
        if_nz           add     x,#11*4
        if_nz           add     x,_head
        if_nz           add     x,_head
        if_nz           wrword  data,x
        if_nz           add     _head,#1
        if_nz           and     _head,#$F

                        test    stat,#4         wc      'if not configure flag, done
        if_nc           jmp     #update                 'else configure to update leds
'
'
' Configure keyboard
'
configure               mov     data,#$F3               'set keyboard auto-repeat
                        call    #transmit
                        mov     data,_auto
                        and     data,#%11_11111
                        call    #transmit

                        mov     data,#$ED               'set keyboard lock-leds
                        call    #transmit
                        mov     data,_locks
                        rev     data,#-3 & $1F
                        test    data,#%100      wc
                        rcl     data,#1
                        and     data,#%111
                        call    #transmit

                        mov     x,_locks                'insert locks into _states
                        and     x,#%111
                        shl     _states+7,#3
                        or      _states+7,x
                        ror     _states+7,#3

                        mov     _present,#1             'set _present

                        jmp     #update                 'done
'
'
' Lookup byte in table
'
look                    ror     data,#2                 'perform lookup
                        movs    :reg,data
                        add     :reg,#table
                        shr     data,#27
                        mov     x,data
:reg                    mov     data,0
                        shr     data,x

                        jmp     #rand                   'isolate byte
'
'
' Transmit byte to keyboard
'
transmit
_c1                     or      dira,cmask              'pull clock low
                        movs    napshr,#13              'hold clock for ~128us (must be >100us)
                        call    #nap
_d1                     or      dira,dmask              'pull data low
                        movs    napshr,#18              'hold data for ~4us
                        call    #nap
_c2                     xor     dira,cmask              'release clock

                        test    data,#$0FF      wc      'append parity and stop bits to byte
                        muxnc   data,#$100
                        or      data,dlsb

                        mov     x,#10                   'ready 10 bits
transmit_bit            call    #wait_c0                'wait until clock low
                        shr     data,#1         wc      'output data bit
_d2                     muxnc   dira,dmask
                        mov     wcond,c1                'wait until clock high
                        call    #wait
                        djnz    x,#transmit_bit         'another bit?

                        mov     wcond,c0d0              'wait until clock and data low
                        call    #wait
                        mov     wcond,c1d1              'wait until clock and data high
                        call    #wait

                        call    #receive_ack            'receive ack byte with timed wait
                        cmp     data,#$FA       wz      'if ack error, reset keyboard
        if_nz           jmp     #reset

transmit_ret            ret
'
'
' Receive byte from keyboard
'
receive                 test    _cpin,#$20      wc      'wait indefinitely for initial clock low
                        waitpne cmask,cmask
receive_ack
                        mov     x,#11                   'ready 11 bits
receive_bit             call    #wait_c0                'wait until clock low
                        movs    napshr,#16              'pause ~16us
                        call    #nap
_d3                     test    dmask,ina       wc      'input data bit
                        rcr     data,#1
                        mov     wcond,c1                'wait until clock high
                        call    #wait
                        djnz    x,#receive_bit          'another bit?

                        shr     data,#22                'align byte
                        test    data,#$1FF      wc      'if parity error, reset keyboard
        if_nc           jmp     #reset
rand                    and     data,#$FF               'isolate byte

look_ret
receive_ack_ret
receive_ret             ret
'
'
' Wait for clock/data to be in required state(s)
'
wait_c0                 mov     wcond,c0                '(wait until clock low)

wait                    mov     y,tenms                 'set timeout to 10ms

wloop                   movs    napshr,#18              'nap ~4us
                        call    #nap
_c3                     test    cmask,ina       wc      'check required state(s)
_d4                     test    dmask,ina       wz      'loop until got state(s) or timeout
wcond   if_never        djnz    y,#wloop                '(replaced with c0/c1/c0d0/c1d1)

                        tjz     y,#reset                'if timeout, reset keyboard
wait_ret
wait_c0_ret             ret


c0      if_c            djnz    y,#wloop                '(if_never replacements)
c1      if_nc           djnz    y,#wloop
c0d0    if_c_or_nz      djnz    y,#wloop
c1d1    if_nc_or_z      djnz    y,#wloop
'
'
' Nap
'
nap                     rdlong  t,#0                    'get clkfreq
napshr                  shr     t,#18/16/13             'shr scales time
                        min     t,#3                    'ensure waitcnt won't snag
                        add     t,cnt                   'add cnt to time
                        waitcnt t,#0                    'wait until time elapses (nap)

nap_ret                 ret
'
'
' Initialized data
'
'
dlsb                    long    1 << 9
tenms                   long    10_000 / 4
'
'
' Lookup table
'                               ascii   scan    extkey  regkey  ()=keypad
'
table                   word    $0000   '00
                        word    $00D8   '01             F9
                        word    $0000   '02
                        word    $00D4   '03             F5
                        word    $00D2   '04             F3
                        word    $00D0   '05             F1
                        word    $00D1   '06             F2
                        word    $00DB   '07             F12
                        word    $0000   '08
                        word    $00D9   '09             F10
                        word    $00D7   '0A             F8
                        word    $00D5   '0B             F6
                        word    $00D3   '0C             F4
                        word    $0009   '0D             Tab
                        word    $0060   '0E             `
                        word    $0000   '0F
                        word    $0000   '10
                        word    $F5F4   '11     Alt-R   Alt-L
                        word    $00F0   '12             Shift-L
                        word    $0000   '13
                        word    $F3F2   '14     Ctrl-R  Ctrl-L
                        word    $0071   '15             q
                        word    $0031   '16             1
                        word    $0000   '17
                        word    $0000   '18
                        word    $0000   '19
                        word    $007A   '1A             z
                        word    $0073   '1B             s
                        word    $0061   '1C             a
                        word    $0077   '1D             w
                        word    $0032   '1E             2
                        word    $F600   '1F     Win-L
                        word    $0000   '20
                        word    $0063   '21             c
                        word    $0078   '22             x
                        word    $0064   '23             d
                        word    $0065   '24             e
                        word    $0034   '25             4
                        word    $0033   '26             3
                        word    $F700   '27     Win-R
                        word    $0000   '28
                        word    $0020   '29             Space
                        word    $0076   '2A             v
                        word    $0066   '2B             f
                        word    $0074   '2C             t
                        word    $0072   '2D             r
                        word    $0035   '2E             5
                        word    $CC00   '2F     Apps
                        word    $0000   '30
                        word    $006E   '31             n
                        word    $0062   '32             b
                        word    $0068   '33             h
                        word    $0067   '34             g
                        word    $0079   '35             y
                        word    $0036   '36             6
                        word    $CD00   '37     Power
                        word    $0000   '38
                        word    $0000   '39
                        word    $006D   '3A             m
                        word    $006A   '3B             j
                        word    $0075   '3C             u
                        word    $0037   '3D             7
                        word    $0038   '3E             8
                        word    $CE00   '3F     Sleep
                        word    $0000   '40
                        word    $002C   '41             ,
                        word    $006B   '42             k
                        word    $0069   '43             i
                        word    $006F   '44             o
                        word    $0030   '45             0
                        word    $0039   '46             9
                        word    $0000   '47
                        word    $0000   '48
                        word    $002E   '49             .
                        word    $EF2F   '4A     (/)     /
                        word    $006C   '4B             l
                        word    $003B   '4C             ;
                        word    $0070   '4D             p
                        word    $002D   '4E             -
                        word    $0000   '4F
                        word    $0000   '50
                        word    $0000   '51
                        word    $0027   '52             '
                        word    $0000   '53
                        word    $005B   '54             [
                        word    $003D   '55             =
                        word    $0000   '56
                        word    $0000   '57
                        word    $00DE   '58             CapsLock
                        word    $00F1   '59             Shift-R
                        word    $EB0D   '5A     (Enter) Enter
                        word    $005D   '5B             ]
                        word    $0000   '5C
                        word    $005C   '5D             \
                        word    $CF00   '5E     WakeUp
                        word    $0000   '5F
                        word    $0000   '60
                        word    $0000   '61
                        word    $0000   '62
                        word    $0000   '63
                        word    $0000   '64
                        word    $0000   '65
                        word    $00C8   '66             BackSpace
                        word    $0000   '67
                        word    $0000   '68
                        word    $C5E1   '69     End     (1)
                        word    $0000   '6A
                        word    $C0E4   '6B     Left    (4)
                        word    $C4E7   '6C     Home    (7)
                        word    $0000   '6D
                        word    $0000   '6E
                        word    $0000   '6F
                        word    $CAE0   '70     Insert  (0)
                        word    $C9EA   '71     Delete  (.)
                        word    $C3E2   '72     Down    (2)
                        word    $00E5   '73             (5)
                        word    $C1E6   '74     Right   (6)
                        word    $C2E8   '75     Up      (8)
                        word    $00CB   '76             Esc
                        word    $00DF   '77             NumLock
                        word    $00DA   '78             F11
                        word    $00EC   '79             (+)
                        word    $C7E3   '7A     PageDn  (3)
                        word    $00ED   '7B             (-)
                        word    $DCEE   '7C     PrScr   (*)
                        word    $C6E9   '7D     PageUp  (9)
                        word    $00DD   '7E             ScrLock
                        word    $0000   '7F
                        word    $0000   '80
                        word    $0000   '81
                        word    $0000   '82
                        word    $00D6   '83             F7

keypad1                 byte    $CA, $C5, $C3, $C7, $C0, 0, $C1, $C4, $C2, $C6, $C9, $0D, "+-*/"

keypad2                 byte    "0123456789.", $0D, "+-*/"

shift1                  byte    "{|}", 0, 0, "~"

shift2                  byte    $22, 0, 0, 0, 0, "<_>?)!@#$%^&*(", 0, ":", 0, "+"
'
'
' Uninitialized data
'
dmask                   res     1
cmask                   res     1
stat                    res     1
data                    res     1
x                       res     1
y                       res     1
t                       res     1

_head                   res     1       'write-only
_present                res     1       'write-only
_states                 res     8       'write-only
_dpin                   res     1       'read-only at start
_cpin                   res     1       'read-only at start
_locks                  res     1       'read-only at start
_auto                   res     1       'read-only at start

''
''
''      _________
''      Key Codes
''
''      00..DF  = keypress and keystate
''      E0..FF  = keystate only
''
''
''      09      Tab
''      0D      Enter
''      20      Space
''      21      !
''      22      "
''      23      #
''      24      $
''      25      %
''      26      &
''      27      '
''      28      (
''      29      )
''      2A      *
''      2B      +
''      2C      ,
''      2D      -
''      2E      .
''      2F      /
''      30      0..9
''      3A      :
''      3B      ;
''      3C      <
''      3D      =
''      3E      >
''      3F      ?
''      40      @       
''      41..5A  A..Z
''      5B      [
''      5C      \
''      5D      ]
''      5E      ^
''      5F      _
''      60      `
''      61..7A  a..z
''      7B      {
''      7C      |
''      7D      }
''      7E      ~
''
''      80-BF   (future international character support)
''
''      C0      Left Arrow
''      C1      Right Arrow
''      C2      Up Arrow
''      C3      Down Arrow
''      C4      Home
''      C5      End
''      C6      Page Up
''      C7      Page Down
''      C8      Backspace
''      C9      Delete
''      CA      Insert
''      CB      Esc
''      CC      Apps
''      CD      Power
''      CE      Sleep
''      CF      Wakeup
''
''      D0..DB  F1..F12
''      DC      Print Screen
''      DD      Scroll Lock
''      DE      Caps Lock
''      DF      Num Lock
''
''      E0..E9  Keypad 0..9
''      EA      Keypad .
''      EB      Keypad Enter
''      EC      Keypad +
''      ED      Keypad -
''      EE      Keypad *
''      EF      Keypad /
''
''      F0      Left Shift
''      F1      Right Shift
''      F2      Left Ctrl
''      F3      Right Ctrl
''      F4      Left Alt
''      F5      Right Alt
''      F6      Left Win
''      F7      Right Win
''
''      FD      Scroll Lock State
''      FE      Caps Lock State
''      FF      Num Lock State
''
''      +100    if Shift
''      +200    if Ctrl
''      +400    if Alt
''      +800    if Win
''
''      eg. Ctrl-Alt-Delete = $6C9
''
''
'' Note: Driver will buffer up to 15 keystrokes, then ignore overflow.

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