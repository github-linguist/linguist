     ∇ HappyNumbers arg;⎕IO;∆roof;∆first;bin;iroof
[1]   ⍝0: Happy number
[2]   ⍝1: http://rosettacode.org/wiki/Happy_numbers
[3]    ⎕IO←1                              ⍝ Index origin
[4]    ∆roof ∆first←2↑arg,10              ⍝
[5]
[6]    bin←{
[7]        ⍺←⍬                            ⍝ Default left arg
[8]        ⍵=1:1                          ⍝ Always happy!
[9]
[10]       numbers←⍎¨1⊂⍕⍵                 ⍝ Split numbers into parts
[11]       next←+/{⍵*2}¨numbers           ⍝ Sum and square of numbers
[12]
[13]       next∊⍺:0                       ⍝ Return 0, if already exists
[14]       (⍺,next)∇ next                 ⍝ Check next number (recursive)
[15]
[16]   }¨iroof←⍳∆roof                     ⍝ Does all numbers upto ∆root smiles?
[17]
[18]   ⎕←~∘0¨∆first↑bin/iroof             ⍝ Show ∆first numbers, but not 0
     ∇
