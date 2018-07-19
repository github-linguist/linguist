ss = "In olden times when wishing still helped one, there lived a king"
   ++"whose daughters were all beautiful, but the youngest was so beautiful"
   ++"that the sun itself, which has seen so much, was astonished whenever"
   ++"it shone in her face.  Close by the king's castle lay a great dark"
   ++"forest, and under an old lime-tree in the forest was a well, and when"
   ++"the day was very warm, the king's child went out into the forest and"
   ++"sat down by the side of the cool fountain, and when she was bored she"
   ++"took a golden ball, and threw it up on high and caught it, and this"
   ++"ball was her favorite plaything."

wordwrap maxlen = (wrap_ 0) . words where
	wrap_ _ [] = "\n"
	wrap_ pos (w:ws)
		-- at line start: put down the word no matter what
		| pos == 0 = w ++ wrap_ (pos + lw) ws
		| pos + lw + 1 > maxlen = '\n':wrap_ 0 (w:ws)
		| otherwise = " " ++ w ++ wrap_ (pos + lw + 1) ws
		where lw = length w

main = do
	putStr $ wordwrap 72 ss
	putStr "\n"
	putStr $ wordwrap 32 ss
