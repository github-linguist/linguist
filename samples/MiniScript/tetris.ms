// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/tetris.ms
// TETRIS
// This demo uses `key.available()` and `key.get()`

import "vt"

delay = 0.5
delaySub = 0.01
fieldHeight = 20
fieldWidth = 10
fieldLeft = 20
fieldColor = vt.color.black
kUp = char(19)
kDown = char(20)
kRight = char(18)
kLeft = char(17)
anchorRow = fieldHeight + 2
gameOverFG = vt.color.white
gameOverBG = vt.color.teal

cell = {}
cell.empty = null
cell.I = vt.color.aqua
cell.O = vt.color.yellow
cell.T = vt.color.fuchsia
cell.L = vt.color.orange
cell.J = vt.color.blue
cell.S = vt.color.lime
cell.Z = vt.color.red
_cell = cell

field = []
shape = null

initField = function
	globals.field = []
	for r in range(0, fieldHeight - 1, 1)
		field.push []
		for c in range(0, fieldWidth - 1, 1)
			field[-1].push _cell.empty
		end for
	end for
end function

drawField = function
	for r in range(0, fieldHeight - 1, 1)
		print vt.cursor.goto(r + 1, fieldLeft - 1) + vt.backColor(fieldColor) + "|", ""
		for c in range(0, fieldWidth - 1, 1)
			print vt.backColor(field[r][c]) + "  ", ""
		end for
		print vt.backColor(fieldColor) + "|"
	end for
	print vt.cursor.goto(fieldHeight + 1, fieldLeft - 1) + vt.backColor(fieldColor) + "+" + "-" * fieldWidth * 2 + "+"
	print vt.cursor.goto(anchorRow, 1)
end function

drawGameOver = function
	print vt.cursor.goto(10, fieldLeft) + vt.textColor(gameOverFG) + vt.backColor(gameOverBG) + "       G A M E      "
	print vt.cursor.goto(11, fieldLeft) + vt.textColor(gameOverFG) + vt.backColor(gameOverBG) + "       O V E R      "
	print vt.cursor.goto(anchorRow, 1)
end function

initShape = function
	f = [@initI,
	     @initO,
	     @initT,
	     @initL,
	     @initJ,
	     @initS,
	     @initZ][rnd * 7]
	s = f
	for cell in s.cells
		if field[cell.row][cell.col] != _cell.empty then return null
	end for
	return s
end function

initI = function
	s = {}
	s.cells = []
	s.cells.push {"row": 0, "col": 3, "color": _cell.I}
	s.cells.push {"row": 0, "col": 4, "color": _cell.I}
	s.cells.push {"row": 0, "col": 5, "color": _cell.I}
	s.cells.push {"row": 0, "col": 6, "color": _cell.I}
	s.centerRow = 0
	s.centerCol = 4
	return s
end function

initO = function
	s = {}
	s.cells = []
	s.cells.push {"row": 0, "col": 4, "color": _cell.O}
	s.cells.push {"row": 0, "col": 5, "color": _cell.O}
	s.cells.push {"row": 1, "col": 5, "color": _cell.O}
	s.cells.push {"row": 1, "col": 4, "color": _cell.O}
	s.centerRow = 0.5
	s.centerCol = 4.5
	return s
end function

initT = function
	s = {}
	s.cells = []
	s.cells.push {"row": 0, "col": 3, "color": _cell.T}
	s.cells.push {"row": 0, "col": 4, "color": _cell.T}
	s.cells.push {"row": 0, "col": 5, "color": _cell.T}
	s.cells.push {"row": 1, "col": 4, "color": _cell.T}
	s.centerRow = 0
	s.centerCol = 4
	return s
end function

initL = function
	s = {}
	s.cells = []
	s.cells.push {"row": 1, "col": 4, "color": _cell.L}
	s.cells.push {"row": 0, "col": 4, "color": _cell.L}
	s.cells.push {"row": 0, "col": 5, "color": _cell.L}
	s.cells.push {"row": 0, "col": 6, "color": _cell.L}
	s.centerRow = 0.5
	s.centerCol = 4.5
	return s
end function

initJ = function
	s = {}
	s.cells = []
	s.cells.push {"row": 0, "col": 3, "color": _cell.J}
	s.cells.push {"row": 0, "col": 4, "color": _cell.J}
	s.cells.push {"row": 0, "col": 5, "color": _cell.J}
	s.cells.push {"row": 1, "col": 5, "color": _cell.J}
	s.centerRow = 0.5
	s.centerCol = 4.5
	return s
end function

initS = function
	s = {}
	s.cells = []
	s.cells.push {"row": 1, "col": 3, "color": _cell.S}
	s.cells.push {"row": 1, "col": 4, "color": _cell.S}
	s.cells.push {"row": 0, "col": 4, "color": _cell.S}
	s.cells.push {"row": 0, "col": 5, "color": _cell.S}
	s.centerRow = 0.5
	s.centerCol = 4.5
	return s
end function

initZ = function
	s = {}
	s.cells = []
	s.cells.push {"row": 0, "col": 4, "color": _cell.Z}
	s.cells.push {"row": 0, "col": 5, "color": _cell.Z}
	s.cells.push {"row": 1, "col": 5, "color": _cell.Z}
	s.cells.push {"row": 1, "col": 6, "color": _cell.Z}
	s.centerRow = 0.5
	s.centerCol = 4.5
	return s
end function

drawShape = function(s)
	for cell in s.cells
		print vt.cursor.goto(cell.row + 1, fieldLeft + cell.col * 2) + vt.backColor(cell.color) + "  "
	end for
	print vt.cursor.goto(anchorRow, 1)
end function

eraseShape = function(s)
	for cell in s.cells
		print vt.cursor.goto(cell.row + 1, fieldLeft + cell.col * 2) + vt.backColor(fieldColor) + "  "
	end for
	print vt.cursor.goto(anchorRow, 1)
end function

absorbShape = function(s)
	for cell in s.cells
		field[cell.row][cell.col] = cell.color
	end for
end function

cloneShape = function(s)
	s2 = {}
	s2.cells = []
	for cell in s.cells
		s2.cells.push cell + {}
	end for
	s2.centerRow = s.centerRow
	s2.centerCol = s.centerCol
	return s2
end function

moveDown = function(s)
	s2 = cloneShape(s)
	for cell in s2.cells
		cell.row += 1
		if cell.row >= fieldHeight then return null
		if field[cell.row][cell.col] != _cell.empty then return null
	end for
	s2.centerRow += 1
	return s2
end function

moveLeft = function(s)
	s2 = cloneShape(s)
	for cell in s2.cells
		cell.col -= 1
		if cell.col < 0 then return null
		if field[cell.row][cell.col] != _cell.empty then return null
	end for
	s2.centerCol -= 1
	return s2
end function

moveRight = function(s)
	s2 = cloneShape(s)
	for cell in s2.cells
		cell.col += 1
		if cell.col >= fieldWidth then return null
		if field[cell.row][cell.col] != _cell.empty then return null
	end for
	s2.centerCol += 1
	return s2
end function

fall = function(s)
	s2 = cloneShape(s)
	while true
		moved = moveDown(s2)
		if moved == null then return s2
		s2 = moved
	end while
end function

rotate = function(s)
	s2 = cloneShape(s)
	crossedLeftP = false
	crossedRightP = false
	for cell in s2.cells
		r = cell.row
		c = cell.col
		cell.row = s2.centerRow + (c - s2.centerCol)
		cell.col = s2.centerCol - (r - s2.centerRow)
		if cell.col < 0 then crossedLeftP = true
		if cell.col >= fieldWidth then crossedRightP = true
		if cell.row >= fieldHeight then return null
		if field[cell.row][cell.col] != _cell.empty then return null
	end for
	if crossedLeftP then s2 = moveRight(s2)
	if crossedRightP then s2 = moveLeft(s2)
	return s2
end function

deleteRows = function
	for i in range(0, fieldHeight - 1, 1)
		nNulls = 0
		for cell in field[i]
			if cell == _cell.empty then nNulls += 1
		end for
		if nNulls == 0 then
			emptyRow = []
			for j in range(0, fieldWidth - 1, 1)
				emptyRow.push _cell.empty
			end for
			outer.field = [emptyRow] + field[:i] + field[i + 1:]
			outer.delay -= delaySub
		end if
	end for
end function


initField
drawField
key._echo = false
t = time
while true
	yield
	
	if shape == null then
		shape = initShape
		if shape == null then
			drawGameOver
			key._echo = true
			exit
		end if
		drawShape shape
	end if
	
	s = shape
	moved = null
	while key.available
		k = key.get
		if k == kUp then
			moved = rotate(s)
		else if k == kDown then
			moved = fall(s)
		else if k == kLeft then
			moved = moveLeft(s)
		else if k == kRight then
			moved = moveRight(s)
		end if
		if moved != null then s = moved
	end while
	
	if time > t then
		t = time + delay
		moved = moveDown(s)
		if moved == null then
			absorbShape s
			deleteRows
			drawField
			shape = null
			s = null
		else
			s = moved
		end if
	end if
	
	if s != shape then
		eraseShape shape
		drawShape s
		shape = s
	end if
end while
