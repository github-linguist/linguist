s1 := "aleph",   p1 := 1/5.0                       ; Input
s2 := "beth",    p2 := 1/6.0
s3 := "gimel",   p3 := 1/7.0
s4 := "daleth",  p4 := 1/8.0
s5 := "he",      p5 := 1/9.0
s6 := "waw",     p6 := 1/10.0
s7 := "zayin",   p7 := 1/11.0
s8 := "heth",    p8 := 1-p1-p2-p3-p4-p5-p6-p7
n := 8, r0 := 0, r%n% := 1                         ; auxiliary data

Loop % n-1
   i := A_Index-1, r%A_Index% := r%i% + p%A_Index% ; cummulative distribution

Loop 1000000 {
   Random R, 0, 1.0
   Loop %n%                                        ; linear search
      If (R < r%A_Index%) {
          c%A_Index%++
          Break
      }
}
                                                   ; Output
Loop %n%
   t .= s%A_Index% "`t" p%A_Index% "`t" c%A_Index%*1.0e-6 "`n"
Msgbox %t%

/*
output:
---------------------------
aleph  0.200000   0.199960
beth   0.166667   0.166146
gimel  0.142857   0.142624
daleth 0.125000   0.124924
he     0.111111   0.111226
waw    0.100000   0.100434
zayin  0.090909   0.091344
heth   0.063456   0.063342
---------------------------
*/
