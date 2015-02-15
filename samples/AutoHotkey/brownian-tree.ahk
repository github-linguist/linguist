SetBatchLines -1
Process, Priority,, high
size := 400
D    := .08
num  := size * size * d
field:= Object()
field[size//2, size//2] := true ; set the seed
lost := 0

Loop % num
{
	x := Rnd(1, size), y := Rnd(1, size)
	Loop
	{
		oldX := X, oldY := Y
		x += Rnd(-1, 1), y += Rnd(1, -1)
		If ( field[x, y] )
		{
			field[oldX, oldY] := true
			break
		}
		If ( X > Size ) or ( Y > Size) or ( X < 1 ) or ( Y < 1 )
		{
			lost++
			break
		}
	}
}

pToken  := Gdip_startup()
pBitmap := Gdip_CreateBitmap(size, size)
loop %size%
{
	x := A_index
	Loop %size%
	{
		If ( field[x, A_Index] )
		{
			Gdip_SetPixel(pBitmap, x, A_Index, 0xFF0000FF)
		}
	}
}
Gdip_SaveBitmapToFile(pBitmap, "brownian.png")
Gdip_DisposeImage(pBitmap)
Gdip_Shutdown(pToken)
Run brownian.png

MsgBox lost %lost%

Rnd(min, max){
	Random, r, min, max
	return r
}
