gnomeSort [] = []
gnomeSort (x:xs) = gs [x] xs
    where
	gs vv@(v:vs) (w:ws)
		| v<=w = gs (w:vv) ws
		| otherwise = gs vs (w:v:ws)
	gs [] (y:ys) = gs [y] ys
	gs xs [] = reverse xs
-- keeping the first argument of gs in reverse order avoids the deterioration into cubic behaviour
