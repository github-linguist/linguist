(define foldl
	F R [] -> R
	F R [X | Y] -> (foldl F (F R X) Y))
