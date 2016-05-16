sr = 44100
kr = 4410
ksmps = 10  
nchnls = 2
0dbfs = 32767

; LOAD SOUNDFONTS
gienginenum1 fluidEngine
gienginenum2 fluidEngine
isfnum1      fluidLoad "Piano Steinway Grand Model C (21,738KB).sf2", gienginenum1, 1
; Bright Steinway, program 1, channel 1
             fluidProgramSelect gienginenum1, 1, isfnum1, 0, 1
; Concert Steinway with reverb, program 2, channel 3
             fluidProgramSelect gienginenum1, 3, isfnum1, 0, 2
isfnum2      fluidLoad "63.3mg The Sound Site Album Bank V1.0.SF2", gienginenum2, 1
; General MIDI, program 50, channel 2
             fluidProgramSelect gienginenum2, 2, isfnum2, 0, 50

; SEND NOTES TO STEINWAY SOUNDFONT

instr 1 ; FluidSynth Steinway Rev
  ; INITIALIZATION
             mididefault   60, p3 ; Default duration of 60 -- overridden by score.
             midinoteonkey p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 1
  ikey       = p4
  ivelocity  = p5
             fluidNote gienginenum1, ichannel, ikey, ivelocity
endin

instr 2 ; GM soundfont
  ; INITIALIZATION
             mididefault   60, p3 ; Default duration of 60 -- overridden by score.
             midinoteonkey p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 2
  ikey       = p4
  ivelocity  = p5
             fluidNote gienginenum2, ichannel, ikey, ivelocity
endin

instr 3 ; FluidSynth Steinway Rev
  ; INITIALIZATION
             mididefault   60, p3 ; Default duration of 60 -- overridden by score.
             midinoteonkey p4, p5 ; Channels MIDI input to pfields.
  ; Use channel assigned in fluidload.
  ichannel   = 3
  ikey       = p4
  ivelocity  = p5
             fluidNote gienginenum1, ichannel, ikey, ivelocity
endin

; COLLECT AUDIO FROM ALL SOUNDFONTS

instr 100 ; Fluidsynth output
  ; INITIALIZATION
  ; Normalize so iamplitude for p5 of 80 == ampdb(80).
  iamplitude1 = ampdb(p5) * (10000.0 / 0.1)
  iamplitude2 = ampdb(p6) * (10000.0 / 0.1)

; AUDIO
aleft1, aright1 fluidOut   gienginenum1
aleft2, aright2 fluidOut   gienginenum2
                outs       (aleft1 * iamplitude1) + (aleft2 * iamplitude2),  \
                           (aright1 * iamplitude1) + (aright2 * iamplitude2)
endin
