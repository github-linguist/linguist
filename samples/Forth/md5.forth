include ffl/md5.fs

\ Create a MD5 variable md1 in the dictionary

md5-create md1

\ Update the variable with data

s" The quick brown fox jumps over the lazy dog" md1 md5-update

\ Finish the MD5 calculation resulting in four unsigned 32 bit words
\ on the stack representing the hash value

md1 md5-finish

\ Convert the hash value to a hex string and print it

md5+to-string type cr
