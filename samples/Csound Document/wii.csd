<CsoundSynthesizer>
<CsOptions>
-+rtaudio=alsa -o dac:hw:0
</CsOptions>
<CsInstruments>
nchnls = 2
ksmps = 400

#define WII_B           #3#
#define WII_A           #4#
#define WII_R_A         #304#
#define WII_PITCH       #20#
#define WII_ROLL        #21#
#define WII_BATTERY     #27#

#define WII_RUMBLE      #3#
#define WII_SET_LEDS    #4#

gkcnt init 1

instr 1  
  i1  wiiconnect 3,1

      wiirange   $WII_PITCH., -20, 0
  kb  wiidata    $WII_BATTERY.
  kt  wiidata    $WII_B.
  ka  wiidata    $WII_A.
  kra wiidata    $WII_R_A.
  gka wiidata    $WII_PITCH.
  gkp wiidata    $WII_ROLL.
; If the B (trigger) button is pressed then activate a note
  if  (kt==0)    goto ee
  event "i", 2, 0, 5
  gkcnt = gkcnt + 1
  wiisend       $WII_SET_LEDS., gkcnt
ee:
  if (ka==0)    goto ff
  wiisend       $WII_RUMBLE., 1
ff:
  if (kra==0)   goto gg
  wiisend       $WII_RUMBLE., 0
gg:
  printk2  kb
endin

instr 2
  a1 oscil  ampdbfs(gka), 440+gkp, 1
     outs   a1, a1
endin

</CsInstruments>

<CsScore>
f1 0 4096 10 1
i1 0 300

</CsScore>

</CsoundSynthesizer>
