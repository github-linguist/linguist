; open two LUNs
openw,unit1,'output.txt,/get
openr,unit2,'input.txt',/get
; how many bytes to read
fs = fstat(unit2)
; make buffer
buff = bytarr(fs.size)
; transfer content
readu,unit2,buff
writeu,unit1,buff
; that's all
close,/all
