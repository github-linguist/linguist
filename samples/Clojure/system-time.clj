(import '[java.util Date])
; the current system date time string
(print (new Date))
; the system time as milliseconds since 1970
(print (. (new Date) getTime))
; or
(print (System/currentTimeMillis))
