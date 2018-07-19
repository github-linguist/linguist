InitSound()
; We need this to use Sound functions
UseOGGSoundDecoder()
; Now we can not only load wav sound files, but also ogg encoded ones.
;   With movie library more formats can be played (depends on system) but you cannot
;   handle them with Sound functions
If Not LoadSound(1,"Path/to/Sound/1.ogg") Or Not LoadSound(2,"Path/to/Sound/2.wav")
MessageRequester("Error","One of our sounds could not be loaded"+Chr(10)+"Use Debugger to check which one")
EndIf

;- simultaneous playing
PlaySound(1)
PlaySound(2)

;- manipulating sounds
Delay(1000)
; pause for one second, to let user hear something
SoundVolume(1,90)
SoundVolume(2,60)
; reduce volume of the sounds a bit
SoundPan(1,-80)
SoundPan(2,100)
; Sound 1 mostly left speaker, sound 2 only right speaker
SoundFrequency(1,30000)
; play sound one faster
Delay(1000)
; pause for one second, to let user hear effects of previous actions

;- stopping while playing
StopSound(-1)
; value -1 stops all playing sounds
PlaySound(1,#PB_Sound_Loop)
; continous looping without glitch

;suitable for 2D games and music playing.
; TODO: There is a Sound3D library for 3D Games, needs to be decribed here too
