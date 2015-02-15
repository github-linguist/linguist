10 PRINT "The border colour is "; PEEK (23624): REM bordcr
20 PRINT "The ramtop address is "; PEEK (23730) + 256 * PEEK (23731): REM ramtop
30 POKE 23609,50: REM set keyboard pip to 50
