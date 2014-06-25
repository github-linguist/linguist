{{
*****************************************
* 4x4 Keypad Reader           v1.0      *
* Author: Beau Schwabe                  *
* Copyright (c) 2007 Parallax           *               
* See end of file for terms of use.     *               
*****************************************
}}
{

Operation:

This object uses a capacitive PIN approach to reading the keypad.
To do so, ALL pins are made LOW and an OUTPUT to "discharge" the
I/O pins.  Then, ALL pins are set to an INPUT state.  At this point,
only one pin is made HIGH and an OUTPUT at a time.  If the "switch"
is closed, then a HIGH will be read on the input, otherwise a LOW
will be returned.

The keypad decoding routine only requires two subroutines and returns
the entire 4x4 keypad matrix into a single WORD variable indicating
which buttons are pressed.  Multiple button presses are allowed with
the understanding that“BOX entries can be confused. An example of a
BOX entry... 1,2,4,5 or 1,4,3,6 or 4,6,*,#  etc. where any 3 of the 4
buttons pressed will evaluate the non pressed button as being pressed,
even when they are not.  There is no danger of any physical or
electrical damage, that s just the way this sensing method happens to
work.

Schematic:
No resistors, No capacitors.  The connections are directly from the
keypad to the I/O's.  I literally plugged mine right into the demo
board RevC.

Looking at the Back of the 4x4 keypad...

       P7         P0
         ││││││││
┌─────── ││││││││ ───────┐
│     oo ││││││││ o      │
│                        │
│  O    O    O    O    O │ 
│                        │
│  O    O    O    O    O │
│         {LABEL}        │
│  O    O    O    O    O │ 
│                        │
│  O    O    O    O    O │
│                        │
│  O    O    O    O    O │
│             o    o     │
└────────────────────────┘

}
VAR
  word  keypad
  
PUB ReadKeyPad
    keypad := 0                 'Clear 4x4 'keypad' value
    ReadRow(3)                  'Call routine to read entire ROW 0
    keypad <<= 4                'Shift 'keypad' value left by 4
    ReadRow(2)                  'Call routine to read entire ROW 1
    keypad <<= 4                'Shift 'keypad' value left by 4
    ReadRow(1)                  'Call routine to read entire ROW 2
    keypad <<= 4                'Shift 'keypad' value left by 4
    ReadRow(0)                  'Call routine to read entire ROW 3
    Result := keypad

PRI ReadRow(n)
    outa[0..7]~                 'preset P0 to P7 as LOWs
    dira[0..7]~~                'make P0 to P7 OUTPUTs ... discharge pins or "capacitors" to VSS
    dira[0..7]~                 'make P0 to P7 INPUTSs ... now the pins act like tiny capacitors
    outa[n]~~                   'preset Pin 'n' HIGH         
    dira[n]~~                   'make Pin 'n' an OUTPUT... Make only one pin HIGH ; will charge
                                '                          "capacitor" if switch is closed.
                                ' 
    keypad += ina[4..7]         'read ROW value        ... If a switch is open, the pin or "capacitor"
    dira[n]~                    'make Pn an INPUT          will remain discharged
    
DAT
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