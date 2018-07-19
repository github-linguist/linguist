import strutils, math

const
  pricemap: array[0 .. 19, int] = [10,18,26,32,38,44,50,54,58,62,66,70,74,78,82,86,90,94,98,100]

# outputs an int (=>float*100)
proc floatToPrice100(f: float): int =
    # indx: 0.1-0.05->0, 0.06-0.10->1, 0.11-0.15->2, .....
    var valu: int = toInt(f*100)
    if valu == 0:
        result = 10
    else:
        dec(valu)
        # inc indx every 5 of valu, so value of 1..100 translates to indx of 0..19
        var indx: int = 2*int(valu/10)+int((valu%%10)/5)
        result = pricemap[indx]

# str representation of an int (that is a representation of a float price)
proc price100ToStr(p: int): string =
    if p < 10:
       result = "0.0" & $p
    if p < 100:
       result = "0." & $p
    else:
       result = "1.00"

randomize()
var i: int = 0

for x in 0 .. 10:
   i = random(101)
   echo("Price for ", i.price100ToStr(), ", is: ", float(i/100).floatToPrice100().price100ToStr())
