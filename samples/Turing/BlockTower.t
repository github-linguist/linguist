View.Set ("graphics:300;600,offscreenonly,nobuttonbar")

class block
    export var x, var y, var vSpd, var hSpd, var size, var clr, var falling, draw, move

    var x, y, vSpd, hSpd : real
    var size, clr : int
    var falling : boolean

    procedure move
	x += hSpd
	y += vSpd
    end move

    procedure draw

	Draw.FillBox (round (x) - size div 2, round (y) - size div 2, round (x) + size div 2, round (y) + size div 2, clr)
    end draw
end block

var blocks : flexible array 1 .. 0 of pointer to block
const gravity := 0.5
const rangeLow := maxx div 5
const rangeHigh := maxx div 5 * 4
var lvl, score : int
var clawX, clawY, spd : real
clawX := maxx div 2
clawY := maxy div 2
spd := 2
lvl := 1
score := 0

type pointsplash :
    record
	lvl : int
	aim : int
	duration : int
	block : int
    end record

var splash : flexible array 1 .. 0 of pointsplash

procedure createBlock
    new blocks, upper (blocks) + 1
    new block, blocks (upper (blocks))
    blocks (upper (blocks)) -> x := clawX
    blocks (upper (blocks)) -> y := clawY
    blocks (upper (blocks)) -> vSpd := 0
    blocks (upper (blocks)) -> hSpd := spd
    blocks (upper (blocks)) -> size := 31 - lvl
    blocks (upper (blocks)) -> clr := black
    blocks (upper (blocks)) -> falling := false
end createBlock

%-------------------------------movement----------------------------

var highest : int := 0
var ground : int := 5
procedure scrollUp
    const scrollSpd := 2
    for i : 1 .. upper (blocks)
	blocks (i) -> y -= scrollSpd
    end for
    ground -= scrollSpd
    highest -= scrollSpd
end scrollUp

procedure moveClaw
    if clawX <= rangeLow then
	clawX := rangeLow
	spd *= -1
    elsif clawX >= rangeHigh then
	clawX := rangeHigh
	spd *= -1
    end if
    if clawY > maxy div 3 * 2 then
	scrollUp
    end if
    clawX += spd
    clawY := highest + 100 + 2 * lvl
    blocks (upper (blocks)) -> x := clawX
    blocks (upper (blocks)) -> y := clawY
    blocks (upper (blocks)) -> hSpd := spd
end moveClaw

procedure moveBlocks
    for i : 1 .. upper (blocks)
	if blocks (i) -> falling then
	    blocks (i) -> vSpd -= gravity
	end if
	blocks (i) -> move
    end for
end moveBlocks

procedure checkHeight (i : int)
    if round (blocks (i) -> y) > highest then
	highest := round (blocks (i) -> y)
    end if
end checkHeight

var aim : real := 0
var total, dif : int := 0
function aimBonus (i, j : int) : int
    total := blocks (i) -> size div 2 + blocks (j) -> size div 2
    dif := abs (round (blocks (i) -> x - blocks (j) -> x))
    if total >= dif then
	aim := ((total - dif) / total) * 10
    else
	aim := 0
    end if
    result round (aim)
end aimBonus

const splashTime := 1500
procedure stopBlock (i, j : int)
    blocks (i) -> falling := false
    blocks (i) -> y := blocks (j) -> y + blocks (i) -> size div 2 + blocks (j) -> size div 2
    blocks (i) -> vSpd := 0
    blocks (i) -> hSpd := 0
    checkHeight (i)
    score += lvl + aimBonus (i, j)
    new splash, upper (splash) + 1
    splash (upper (splash)).lvl := lvl
    splash (upper (splash)).aim := aimBonus (i, j)
    splash (upper (splash)).duration := splashTime
    splash (upper (splash)).block := i
end stopBlock

%----------------high scores--------------------

type scores :
    record
	name : string (5)
	score : int
    end record
var highScores : array 1 .. 10 of scores
var currentHS : scores
currentHS.name := ""

const fileName := "BlockTowerHS.bin"
var fileNo : int
procedure createFile
    open : fileNo, fileName, write
    for i : 1 .. 10
	highScores (i).name := "Name"
	highScores (i).score := 0
	write : fileNo, highScores (i)
    end for
    close : fileNo
end createFile

procedure loadHS
    if File.Exists (fileName) then
	open : fileNo, fileName, read
	for i : 1 .. 10
	    read : fileNo, highScores (i)
	end for
	close : fileNo
    else
	createFile
	loadHS
    end if
end loadHS

procedure saveHS
    open : fileNo, fileName, write
    for i : 1 .. 10
	write : fileNo, highScores (i)
    end for
    close : fileNo
end saveHS

procedure sortScores
    for i : 1 .. 10
	if i = 1 then
	    if currentHS.score >= highScores (i).score then
		for decreasing j : 10 .. 2
		    highScores (j).score := highScores (j - 1).score
		    highScores (j).name := highScores (j - 1).name
		end for
		highScores (i).score := currentHS.score
		highScores (i).name := currentHS.name
	    end if
	else
	    if currentHS.score >= highScores (i).score and currentHS.score < highScores (i - 1).score then
		for decreasing j : 10 .. i
		    highScores (j).score := highScores (j - 1).score
		    highScores (j).name := highScores (j - 1).name
		end for
		highScores (i).score := currentHS.score
		highScores (i).name := currentHS.name
	    end if
	end if
    end for
end sortScores

procedure getName
    var test : string
    loop
	locate (20, maxcol div 2 - 6)
	put ""
	locate (20, maxcol div 2 - 6)
	put "Name: " ..
	View.Update
	get test
	if length (test) <= 5 then
	    currentHS.name := test
	    currentHS.score := score
	    exit
	end if
    end loop
end getName

procedure displayHS
    loadHS
    locate (7, maxcol div 2 - 6)
    put "High Scores"
    for i : 1 .. 10
	if highScores (i).name = currentHS.name and highScores (i).score = currentHS.score then
	    locate (7 + i, maxcol div 2 - 9)
	    put ">"
	end if
	locate (7 + i, maxcol div 2 - 7)
	put highScores (i).name
	locate (7 + i, maxcol div 2 + 6 - length (intstr (highScores (i).score)))
	put highScores (i).score
	if highScores (i).name = currentHS.name and highScores (i).score = currentHS.score then
	    locate (7 + i, maxcol div 2 + 7)
	    put "<"
	end if
    end for
    locate (19, maxcol div 2 - (4 + length (intstr (score)) div 2))
    put "Score: ", score
end displayHS

%------------------------losing--------------------------
procedure playAgain
    View.Set ("offscreenonly")
    lvl := 1
    score := 0
    spd := 2
    clawX := maxx div 2
    clawY := maxy div 2
    new blocks, 0
    new splash, 0
    ground := 5
    highest := 0
    currentHS.name := ""
    currentHS.score := 0
end playAgain

var chars : array char of boolean
var gameOver := false
procedure loseGame
    View.Set ("nooffscreenonly")
    cls
    put "You Lose!"
    put "You got to level ", lvl, "!"
    put "Score: ", score
    displayHS
    getName
    sortScores
    saveHS
    displayHS
    locate (21, maxcol div 2 - 14)
    put "Press <Enter> To Play Again"
    locate (22, maxcol div 2 - 10)
    put "Press <Esc> To Quit"
    delay (500)
    Input.Flush
    loop
	Input.KeyDown (chars)
	if chars (KEY_ENTER) then
	    playAgain
	    exit
	elsif chars (KEY_ESC) then
	    gameOver := true
	    exit
	end if
    end loop
end loseGame

%---------------------interactions------------------------

procedure collisions
    var i : int := 0
    loop
	exit when i >= upper (blocks)
	i += 1
	if blocks (i) -> falling then
	    for j : 1 .. upper (blocks)
		if i not= j then
		    if blocks (i) -> y - blocks (j) -> y <= blocks (i) -> size div 2 + blocks (j) -> size div 2
			    and blocks (j) -> falling = false and blocks (j) -> y not= clawY then
			if blocks (i) -> x >= blocks (j) -> x then
			    if blocks (i) -> x - blocks (j) -> x <= blocks (i) -> size div 2 + blocks (j) -> size div 2 then
				stopBlock (i, j)
			    end if
			elsif blocks (i) -> x < blocks (j) -> x then
			    if blocks (j) -> x - blocks (i) -> x <= blocks (i) -> size div 2 + blocks (j) -> size div 2 then
				stopBlock (i, j)
			    end if
			end if
		    end if
		end if
	    end for
	    if (blocks (i) -> y <= ground or blocks (i) -> y < 0) and i not= 1 then
		loseGame
	    elsif blocks (i) -> y <= ground then
		blocks (i) -> falling := false
		blocks (i) -> y := blocks (i) -> size div 2 + ground
		blocks (i) -> vSpd := 0
		blocks (i) -> hSpd := 0
		checkHeight (i)
		score += lvl
	    end if
	end if
    end loop
end collisions

var blockPress := false
procedure keyPresses
    Input.KeyDown (chars)
    if chars (KEY_ENTER) and blockPress = false then
	blocks (upper (blocks)) -> falling := true
	createBlock
	blockPress := true
	lvl := floor ((upper (blocks) - 1) / 5) + 1
	if spd > 0 then
	    spd := 2 + 0.25 * lvl
	elsif spd < 0 then
	    spd := -2 - 0.25 * lvl
	end if
    end if
    if chars (KEY_ENTER) = false then
	blockPress := false
    end if
end keyPresses

%-------------------------------art------------------------------

procedure manageSplashes
    var counter := 0
    loop
	counter += 1
	exit when counter > upper (splash)
	splash (counter).duration -= 30
	if splash (counter).duration <= 0 then
	    for i : counter .. upper (splash) - 1
		splash (i).lvl := splash (i + 1).lvl
		splash (i).aim := splash (i + 1).aim
		splash (i).duration := splash (i + 1).duration
		splash (i).block := splash (i + 1).block
	    end for
	    new splash, upper (splash) - 1
	    counter -= 1
	end if
    end loop
end manageSplashes

procedure draw
    var font := Font.New ("arial:12")
    var splashFont := Font.New ("arial:10")
    cls
    for i : 1 .. upper (blocks)
	blocks (i) -> draw
    end for
    for i : 1 .. upper (splash)
	Font.Draw ("+" + intstr (splash (i).lvl) + " +" + intstr (splash (i).aim), round (blocks (splash (i).block) -> x + blocks (splash (i).block) -> size div 2) + 2,
	    round (blocks (splash (i).block) -> y), splashFont, black)
    end for
    Draw.Line (0, ground, maxx, ground, black)
    Draw.Line (rangeLow, round (clawY), rangeHigh, round (clawY), black)
    Font.Draw ("Level " + intstr (lvl), 1, maxy - 13, font, black)
    Font.Draw ("Score " + intstr (score), 1, maxy - 28, font, black)
    %Font.Draw ("Aim = " + realstr (aim, 0), 1, maxy - 43, font, black)
    %Font.Draw ("Total = " + intstr (total), 1, maxy - 58, font, black)
    %Font.Draw ("Dif = " + intstr (dif), 1, maxy - 73, font, black)
    %Draw.FillOval (round (clawX), round (clawY), 5, 5, brightred)
    %Draw.Line (0, maxy div 3 * 2, maxx, maxy div 3 * 2, brightred)
    View.Update
end draw

%-----------------main--------------------
createBlock
loop
    keyPresses
    moveClaw
    moveBlocks
    collisions
    manageSplashes
    exit when gameOver
    draw
    Time.DelaySinceLast (30)
end loop
cls
