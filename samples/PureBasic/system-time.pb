time=Date()  ; Unix timestamp
time$=FormatDate("%mm.%dd.%yyyy %hh:%ii:%ss",time)

; following value is only reasonable accurate, on Windows it can differ by +/- 20 ms
ms_counter=ElapsedMilliseconds()
; could use API like QueryPerformanceCounter_() on Windows for more accurate values
