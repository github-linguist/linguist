{{
*****************************************
* Inductive Sensor Demo v1.0            *
* Author: Beau Schwabe                  *
* Copyright (c) 2007 Parallax           *
* See end of file for terms of use.     *
*****************************************


Test Circuit:

      10pF   100K  1M
FPin ───┳──┳── SDF(sigma-delta feedback)
          │      ┣──── SDI(sigma-delta input)
        L   100K
                     
         GND    GND  
                                 

Test Coils:

Wire used was the "Radio Shack Special" GREEN (about 27 gauge)

25T (Coke Can form)    = 2.1MHz
15T (Coke Can form)    = 3.9MHz
 5T (Coke Can form)    = 5.3MHz
50T (BIC pen form)     = 3.2MHz



How does it work?

Note: The reported resonate frequency is NOT the actual resonate LC frequency.  Instead it is where the voltage produced from
      the LC circuit was clipped.  

      In the example circuit below:
      
             C     L
        A ────┳──── GND
                │ 
                B         

      When you apply a small voltage at a specific frequency to an LC circuit (at point "A") that is at or near the resonate
      frequency of LC, it is not uncommon to measure 10's or 100's of times the amount of voltage (at point "B") that you are
      applying to the LC circuit. (at point "A")


      In the "Test Circuit" above, point "B" passes through a diode which then basically feeds a divide by 2 voltage divider:

              100K 100K
        B ───┳── GND
                  │
                  C

      ...So in order see the sigma-delta ADC "clip" the frequency sweep result, the output from the LC circuit only needs
      to generate about 6.6 Volts above ground. (0.6V drop across the diode, and since the ADC is only sensitive to about
      3V, it works out to be about 6.6V after the voltage divider.)


      A typical magnitude plot of a frequency sweep applied to an LC circuit might look something like this:
      
                    *
                    *
                    *
                    *
                   * *
                  *   *
                 *     *
               *         *
             *             *
       *****                 *****


       ...With 'clipping' the pattern looks more like this:

                  X****
                 *     *
               *         *
             *             *
       *****                 *****

       ...The 'X' denotes the location of the reported resonate frequency. The reason this is slightly off is for
       two reasons really. 1) lazy - I didn't want to fiddle with the voltage divider combo... adjusting so that the
       "peak" was also where the ADC happened to "clip".  2) some benefit - When you apply a frequency to a tuned LC
       circuit that's resonate frequency is the same as the applied frequency, the LC acts like a dead short.  A
       situation not exactly great for Propeller I/O's

       Now that we have that out of the way, what happens next?  How can we use this so called "coil" as a sensor?

       If a frequency sweep is initially preformed to determine the resonate frequency clip point, then it just so
       happens that adding additional "metal" (<- Does not need to be ferrous) causes the resonate frequency to shift
       to a HIGHER frequency.

       Once you determine the "clip" frequency and you use one of the available counters to constantly feed that
       particular frequency back to the LC circuit, the resulting ADC output is proportional and somewhat linear when
       metal objects are introduced to the coil.

       Assume frequency increases from Left to Right.  With a slight resonate shift to the right, the ADC reports a
       lower "de-tuned" value because the voltage magnitude no longer "clips" at the reported resonate frequency.
       Typical ranges are full scale between 65535 (no metal) and 0 (metal saturation)

 
                          X    *****
                              *     *
 ADC reports value here --> *         *
                          *             *
                    *****                 *****

                     Slight shift to the right

       I also made mention that the response is somewhat linear. As the LC resonance shifts and the ADC value begins
       to lower, the slope is steepest near the "clip" point.  Therefore, the slightest shift results in larger value
       changes.  Since the coil is actually the least sensitive to metal the further away it is (Law of squares) and
       most sensitive to metal the closer it is, the resulting combination acts to linearize the output. I need to
       point out that some LC combinations will exhibit plateaus and other anomalies caused by varying parasitic circuit
       conditions that will affect the overall output, so a little bit of trial and error is necessary to get things
       the way you want them.

}}
OBJ
   Freq   : "Synth"
   ADC    : "ADC"
   gr     : "graphics"
   Num    : "Numbers"
CON
  FPin =                        0

  UpperFrequency =              6_000_000
  LowerFrequency =              2_000_000

  bitmap_base = $2000
  display_base = $5000

VAR
    long              FMax, FTemp, FValue, Frequency
    
PUB demo
    'start and setup graphics
    gr.start
    gr.setup(16, 12, 128, 96, bitmap_base)

    FindResonateFrequency

    DisplayInductorValue

PUB DisplayInductorValue | X
    Freq.Synth("A", FPin, FValue)
    repeat
      ADC.SigmaDelta(@FTemp)

'**************************************** Graphics Option Start *********************************************
      'clear bitmap
      gr.clear
      'draw text
      gr.textmode(1,1,7,5)
      gr.colorwidth(1,0)
      gr.text(0,90,string("Inductive Propeller Sensor"))

      gr.colorwidth(1,5)
      X := (65535 - FTemp )*200/65535
      gr.plot(-100+X,15)

      gr.textmode(1,1,7,%0000)
      gr.colorwidth(1,0)
      gr.text(-100,-20,string("Resonate Frequency ="))
      gr.text(35,-20,Num.ToStr(FValue,10))

      gr.text(-100,-36,string("ADC Frequency Response ="))
      gr.text(65,-36,Num.ToStr(FTemp,10))

      'copy bitmap to display
      gr.copy(display_base)
'**************************************** Graphics Option Finish *********************************************      

PUB FindResonateFrequency | P
    dira[FPin]                  := 1
    
    FMax := 0
    repeat Frequency from LowerFrequency to UpperFrequency step 1000
      Freq.Synth("A", FPin, Frequency)
      ADC.SigmaDelta(@FTemp)

      if FTemp > FMax
         FMax := FTemp
         FValue := Frequency
'**************************************** Graphics Option Start *********************************************
      P := (Frequency - LowerFrequency)*100/(UpperFrequency - LowerFrequency)

      gr.colorwidth(1,5)
      gr.plot(0,0)
      gr.line(P,0)
      gr.colorwidth(3,5)
      gr.line(100,0)

      gr.colorwidth(2,0)
      gr.plot(P,(FTemp/1024)+10)
      gr.colorwidth(0,1)
      gr.plot(P+1,5)
      gr.line(P+1,50)

      gr.copy(display_base)
'**************************************** Graphics Option Finish *********************************************

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