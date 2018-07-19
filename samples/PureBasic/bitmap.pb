w=800 : h=600
CreateImage(1,w,h)
;1 is internal id of image
StartDrawing(ImageOutput(1))
; fill with color red
Box(0,0,w,h,$ff)
; or using another (but slower) way in green
FillArea(0,0,-1,$ff00)
; a green Dot
Plot(10,10,$ff0000)
; check if we set it right (should be 255)
Debug Blue(Point(10,10))
