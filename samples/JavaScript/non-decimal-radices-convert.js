k = 26
s = k.toString(16) //gives 1a
i = parseInt('1a',16) //gives 26
//optional special case for hex:
i = +('0x'+s) //hexadecimal base 16, if s='1a' then i=26.
