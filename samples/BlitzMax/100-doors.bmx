Graphics 640,480
i=1
While ((i*i)<=100)
	a$=i*i
	DrawText a$,10,20*i
	Print i*i
	i=i+1
Wend
Flip
WaitKey
