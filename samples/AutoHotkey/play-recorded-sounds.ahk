SoundPlay, %A_WinDir%\Media\tada.wav, wait
SoundPlay, %A_WinDir%\Media\Windows XP Startup.wav, wait

; simulaneous play may require a second script

SoundPlay, %A_WinDir%\Media\tada.wav
SoundPlay, Nonexistent  ; stop before finishing

SoundSet +10  ; increase volume by 10%
Loop, 2
    SoundPlay, %A_WinDir%\Media\tada.wav, wait
