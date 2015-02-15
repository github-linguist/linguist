 1e 0e f/ f.     \ inf
-1e 0e f/ f.     \ inf (output bug: should say "-inf")
-1e 0e f/ f0< .  \ -1 (true, it is -inf)
 0e 0e f/ f.     \ nan
-1e 0e f/ 1/f f0< .   \ 0 (false, can't represent IEEE negative zero)
