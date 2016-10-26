%This is a comment
var x : array 0 .. 100 of int 
var rangeX, rangeY, loopFor : int := 0
setscreen ("graphics:696;400,nobuttonbar")
setscreen ("nocursor")
setscreen ("noecho")
var colourP : string := "Yellow"
var title : int := Font.New ("Algerian:60")
var subtitle : int := Font.New ("Algerian:20")
var xValue, yValue, font1, font2, instructionsTitle : int
var scoreR, scoreY, button : int := 0
var scoreRS, scoreYS, PreventFor : string
var win : string := "N"
var full, samePlayer : boolean := false
font1 := Font.New ("Algerian:20")
font2 := Font.New ("TimesNewRoman:15")
instructionsTitle := Font.New ("Algerian:35")
var shouldPlay : boolean := true
forward proc game
for i : 0 .. 100  
    x (i) := 0
end for
proc pauseProgram
    var reply : string (1)
    getch (reply)
end pauseProgram
process backgroundMusic
    loop
	exit when shouldPlay = false
	Music.PlayFile ("music.MP3")
    end loop
end backgroundMusic
proc winner
    cls
    win := "N"
    for i : 1 .. 100
	x (i) := 0
    end for
    drawfillbox (0, 0, 696, 400, black)
    scoreRS := "Red Wins: " + intstr (scoreR)
    scoreYS := "Yellow Wins: " + intstr (scoreY)
    Font.Draw ("", 190, 320, title, 48)
    Font.Draw (scoreRS, 0, 250, font1, 52)
    Font.Draw (scoreYS, 0, 220, font1, 52)
    Font.Draw ("Press any Key to continue.", 180, 100, subtitle, 12)
    pauseProgram
    game
end winner
body proc game
    for i : 1 .. 100
	x (i) := 0
    end for
    cls
    drawfillbox (0, 0, 700, 400, black)%Grid
    Draw.ThickLine (0, 400, 0, 0, 5, 48)
    Draw.ThickLine (696, 400, 696, 0, 5, 48)
    Draw.ThickLine (3, 0, 696, 0, 5, 48)
    Draw.ThickLine (696, 0, 696, 70, 5, 48)
    Draw.ThickLine (610, 70, 87, 70, 5, 48)
    Draw.ThickLine (87, 70, 87, 400, 5, 48)
    Draw.ThickLine (87, 400, 0, 400, 5, 48)
    Draw.ThickLine (173, 70, 173, 400, 5, 48)
    Draw.ThickLine (260, 70, 260, 400, 5, 48)
    Draw.ThickLine (347, 70, 347, 400, 5, 48)
    Draw.ThickLine (435, 70, 435, 400, 5, 48)
    Draw.ThickLine (521, 70, 521, 400, 5, 48)
    Draw.ThickLine (609, 70, 609, 400, 5, 48)
    Draw.ThickLine (0, 70, 87, 70, 2, 11) %NEW!
    Draw.ThickLine (0, 120, 87, 120, 2, 11)
    Draw.ThickLine (0, 170, 87, 170, 2, 11)
    Draw.ThickLine (0, 220, 87, 220, 2, 11)
    Draw.ThickLine (0, 270, 87, 270, 2, 11)
    Draw.ThickLine (0, 320, 87, 320, 2, 11)
    Draw.ThickLine (0, 370, 87, 370, 2, 11)
    Draw.ThickLine (609, 70, 696, 70, 2, 11) %Part 2
    Draw.ThickLine (609, 120, 696, 120, 2, 11)
    Draw.ThickLine (609, 170, 696, 170, 2, 11)
    Draw.ThickLine (609, 220, 696, 220, 2, 11)
    Draw.ThickLine (609, 270, 696, 270, 2, 11)
    Draw.ThickLine (609, 320, 696, 320, 2, 11)
    Draw.ThickLine (609, 370, 696, 370, 2, 11)
    loop
	if colourP = "Yellow" then
	    if button = 1 then
		if (rangeX >= 87 and rangeX <= 173) then
		    %Drop on right corner (now verifies height)
		    if x (1) = 0 then
			x (1) := 1
			loopFor := 300
		    elsif x (2) = 0 then
			x (2) := 1
			loopFor := 250
		    elsif x (3) = 0 then
			x (3) := 1
			loopFor := 200
		    elsif x (4) = 0 then
			x (4) := 1
			loopFor := 150
		    elsif x (5) = 0 then
			x (5) := 1
			loopFor := 100
		    elsif x (6) = 0 then
			x (6) := 1
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (135, 401 - i, 25, 25, black)
			    drawfilloval (135, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 174 and rangeX <= 260) then
		    if x (7) = 0 then %Drop on right corner (now verifies height)
			x (7) := 1
			loopFor := 300
		    elsif x (8) = 0 then
			x (8) := 1
			loopFor := 250
		    elsif x (9) = 0 then
			x (9) := 1
			loopFor := 200
		    elsif x (10) = 0 then
			x (10) := 1
			loopFor := 150
		    elsif x (11) = 0 then
			x (11) := 1
			loopFor := 100
		    elsif x (12) = 0 then
			x (12) := 1
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (222, 401 - i, 25, 25, black)
			    drawfilloval (222, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 261 and rangeX <= 347) then
		    if x (13) = 0 then%Drop on right corner (now verifies height)
			x (13) := 1
			loopFor := 300
		    elsif x (14) = 0 then
			x (14) := 1
			loopFor := 250
		    elsif x (15) = 0 then
			x (15) := 1
			loopFor := 200
		    elsif x (16) = 0 then
			x (16) := 1
			loopFor := 150
		    elsif x (17) = 0 then
			x (17) := 1
			loopFor := 100
		    elsif x (18) = 0 then
			x (18) := 1
			loopFor := 50
		    else
			full := true
		    end if
		    if full = true then
			full := false
			samePlayer := true
		    else
			for i : 1 .. loopFor
			    drawfilloval (309, 401 - i, 25, 25, black)
			    drawfilloval (309, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 348 and rangeX <= 434) then
		    if x (19) = 0 then %Drop on right corner (now verifies height)
			x (19) := 1
			loopFor := 300
		    elsif x (20) = 0 then
			x (20) := 1
			loopFor := 250
		    elsif x (21) = 0 then
			x (21) := 1
			loopFor := 200
		    elsif x (22) = 0 then
			x (22) := 1
			loopFor := 150
		    elsif x (23) = 0 then
			x (23) := 1
			loopFor := 100
		    elsif x (24) = 0 then
			x (24) := 1
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (396, 401 - i, 25, 25, black)
			    drawfilloval (396, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 435 and rangeX <= 521) then
		    if x (25) = 0 then %Drop on right corner (now verifies height)
			x (25) := 1
			loopFor := 300
		    elsif x (26) = 0 then
			x (26) := 1
			loopFor := 250
		    elsif x (27) = 0 then
			x (27) := 1
			loopFor := 200
		    elsif x (28) = 0 then
			x (28) := 1
			loopFor := 150
		    elsif x (29) = 0 then
			x (29) := 1
			loopFor := 100
		    elsif x (30) = 0 then
			x (30) := 1
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (483, 401 - i, 25, 25, black)
			    drawfilloval (483, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 522 and rangeX <= 609) then
		    if x (31) = 0 then %Drop on right corner (now verifies height)
			x (31) := 1
			loopFor := 300
		    elsif x (32) = 0 then
			x (32) := 1
			loopFor := 250
		    elsif x (33) = 0 then
			x (33) := 1
			loopFor := 200
		    elsif x (34) = 0 then
			x (34) := 1
			loopFor := 150
		    elsif x (35) = 0 then
			x (35) := 1
			loopFor := 100
		    elsif x (36) = 0 then
			x (36) := 1
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (570, 401 - i, 25, 25, black)
			    drawfilloval (570, 400 - i, 25, 25, yellow)
			    delay (1)
			end for
		    end if
		end if
		if samePlayer = true then
		    samePlayer := false
		else
		    colourP := "Red"
		end if
	    end if
	else
	    if button = 1 then
		if (rangeX >= 87 and rangeX <= 173) then
		    if x (1) = 0 then %Drop on right corner (now verifies height)
			x (1) := 2
			loopFor := 300
		    elsif x (2) = 0 then
			x (2) := 2
			loopFor := 250
		    elsif x (3) = 0 then
			x (3) := 2
			loopFor := 200
		    elsif x (4) = 0 then
			x (4) := 2
			loopFor := 150
		    elsif x (5) = 0 then
			x (5) := 2
			loopFor := 100
		    elsif x (6) = 0 then
			x (6) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (135, 401 - i, 25, 25, black)
			    drawfilloval (135, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 174 and rangeX <= 260) then
		    if x (7) = 0 then %Drop on right corner (now verifies height)
			x (7) := 2
			loopFor := 300
		    elsif x (8) = 0 then
			x (8) := 2
			loopFor := 250
		    elsif x (9) = 0 then
			x (9) := 2
			loopFor := 200
		    elsif x (10) = 0 then
			x (10) := 2
			loopFor := 150
		    elsif x (11) = 0 then
			x (11) := 2
			loopFor := 100
		    elsif x (12) = 0 then
			x (12) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (222, 401 - i, 25, 25, black)
			    drawfilloval (222, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 261 and rangeX <= 347) then
		    if x (13) = 0 then %Drop on right corner (now verifies height)
			x (13) := 2
			loopFor := 300
		    elsif x (14) = 0 then
			x (14) := 2
			loopFor := 250
		    elsif x (15) = 0 then
			x (15) := 2
			loopFor := 200
		    elsif x (16) = 0 then
			x (16) := 2
			loopFor := 150
		    elsif x (17) = 0 then
			x (17) := 2
			loopFor := 100
		    elsif x (18) = 0 then
			x (18) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			samePlayer := true
		    else
			for i : 1 .. loopFor
			    drawfilloval (309, 401 - i, 25, 25, black)
			    drawfilloval (309, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 348 and rangeX <= 434) then
		    %Drop on right corner (now verifies height)
		    if x (19) = 0 then
			x (19) := 2
			loopFor := 300
		    elsif x (20) = 0 then
			x (20) := 2
			loopFor := 250
		    elsif x (21) = 0 then
			x (21) := 2
			loopFor := 200
		    elsif x (22) = 0 then
			x (22) := 2
			loopFor := 150
		    elsif x (23) = 0 then
			x (23) := 2
			loopFor := 100
		    elsif x (24) = 0 then
			x (24) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (396, 401 - i, 25, 25, black)
			    drawfilloval (396, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 435 and rangeX <= 521) then
		    %Drop on right corner (now verifies height)
		    if x (25) = 0 then
			x (25) := 2
			loopFor := 300
		    elsif x (26) = 0 then
			x (26) := 2
			loopFor := 250
		    elsif x (27) = 0 then
			x (27) := 2
			loopFor := 200
		    elsif x (28) = 0 then
			x (28) := 2
			loopFor := 150
		    elsif x (29) = 0 then
			x (29) := 2
			loopFor := 100
		    elsif x (30) = 0 then
			x (30) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (483, 401 - i, 25, 25, black)
			    drawfilloval (483, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		elsif (rangeX >= 522 and rangeX <= 609) then
		    %Drop on right corner (now verifies height)
		    if x (31) = 0 then
			x (31) := 2
			loopFor := 300
		    elsif x (32) = 0 then
			x (32) := 2
			loopFor := 250
		    elsif x (33) = 0 then
			x (33) := 2
			loopFor := 200
		    elsif x (34) = 0 then
			x (34) := 2
			loopFor := 150
		    elsif x (35) = 0 then
			x (35) := 2
			loopFor := 100
		    elsif x (36) = 0 then
			x (36) := 2
			loopFor := 50
		    else
			full := true
			samePlayer := true
		    end if
		    if full = true then
			full := false
		    else
			for i : 1 .. loopFor
			    drawfilloval (570, 401 - i, 25, 25, black)
			    drawfilloval (570, 400 - i, 25, 25, 12)
			    delay (1)
			end for
		    end if
		end if
		if samePlayer then
		    samePlayer := false
		else
		    colourP := "Yellow"
		end if

	    end if
	end if
	mousewhere (rangeX, rangeY, button)
	%Detect Winner - Horizontal Verification
	for i : 1 .. 18
	    if (x (i) = x (i + 6) and x (i) = x (i + 12) and x (i) = x (i + 18) and x (i) = 1) then
		scoreY := scoreY + 1
		win := "Y" %Red
	    elsif x (i) = x (i + 6) and x (i) = x (i + 12) and x (i) = x (i + 18) and x (i) = 2 then
		scoreR := scoreR + 1
		win := "Y"
		if i > 6 then
		    if x (i - 6) = x (i) and x (i) = x (i + 6) and x (i) = x (i + 12) and x (i) = 2 then
			scoreR := scoreR + 1 %Yellow
			win := "Y"
		    elsif x (i - 6) = x (i) and x (i) = x (i + 6) and x (i) = x (i + 12) and x (i) = 1 then
			scoreY := scoreY + 1 %Yellow
			win := "Y"
		    end if
		end if
	    end if
	end for
	%Verticle Detection
	for i : 1 .. 36 by 6
	    if (x (i) = x (i + 1) and x (i) = x (i + 2) and x (i) = x (i + 3) and x (i) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i) = x (i + 1) and x (i) = x (i + 2) and x (i) = x (i + 3) and x (i) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    elsif (x (i + 1) = x (i + 2) and x (i + 1) = x (i + 3) and x (i + 1) = x (i + 4) and x (i + 1) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i + 1) = x (i + 2) and x (i + 1) = x (i + 3) and x (i + 1) = x (i + 4) and x (i + 1) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    elsif (x (i + 2) = x (i + 3) and x (i + 2) = x (i + 4) and x (i + 2) = x (i + 5) and x (i + 2) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i + 2) = x (i + 3) and x (i + 2) = x (i + 4) and x (i + 2) = x (i + 5) and x (i + 2) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    end if
	end for
	for i : 1 .. 36 %Positive Slope
	    if (x (i) = x (i + 7) and x (i) = x (i + 14) and x (i) = x (i + 21) and x (i) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i) = x (i + 7) and x (i) = x (i + 14) and x (i) = x (i + 21) and x (i) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    elsif (x (i + 1) = x (i + 8) and x (i + 1) = x (i + 15) and x (i + 1) = x (i + 22) and x (i + 1) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    elsif (x (i + 1) = x (i + 8) and x (i + 1) = x (i + 15) and x (i + 1) = x (i + 22) and x (i + 1) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i + 2) = x (i + 9) and x (i + 2) = x (i + 16) and x (i + 2) = x (i + 23) and x (i + 2) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (i + 2) = x (i + 9) and x (i + 2) = x (i + 16) and x (i + 2) = x (i + 23) and x (i + 2) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    elsif (x (1) = x (7) and x (1) = x (13) and x (1) = x (19) and x (1) = 1) then
		scoreY := scoreY + 1
		win := "Y"
	    elsif (x (1) = x (7) and x (1) = x (13) and x (1) = x (19) and x (1) = 2) then
		scoreR := scoreR + 1
		win := "Y"
	    end if
	end for
	for i : 1 .. 70 %Negative Slope
	    if i > 24 then
		if (x (i) = x (i - 5) and x (i) = x (i - 10) and x (i) = x (i - 15) and x (i) = 1) then
		    scoreY := scoreY + 1
		    win := "Y"
		elsif (x (i) = x (i - 5) and x (i) = x (i - 10) and x (i) = x (i - 15) and x (i) = 2) then
		    scoreR := scoreR + 1
		    win := "Y"
		elsif (x (i + 1) = x (i - 6) and x (i - 1) = x (i - 11) and x (i - 1) = x (i - 16) and x (i + 1) = 2) then
		    scoreR := scoreR + 1
		    win := "Y"
		elsif (x (i + 1) = x (i - 6) and x (i - 1) = x (i - 11) and x (i - 1) = x (i - 16) and x (i + 1) = 1) then
		    scoreY := scoreY + 1
		    win := "Y"
		elsif (x (i + 2) = x (i - 7) and x (i - 2) = x (i - 12) and x (i - 2) = x (i - 17) and x (i + 2) = 1) then
		    scoreY := scoreY + 1
		    win := "Y"
		elsif (x (i + 2) = x (i - 7) and x (i + 2) = x (i - 12) and x (i - 2) = x (i - 17) and x (i + 2) = 2) then
		    scoreR := scoreR + 1
		    win := "Y"
		end if
	    end if
	end for
	exit when win = "Y"
    end loop
    winner
end game
proc mainMenu
    drawfillbox (0, 0, 696, 400, black)
    Font.Draw ("", 295, 220, font1, 48)
    Font.Draw ("", 251, 150, font1, 48)
    Font.Draw ("", 305, 79, font1, 48)
    loop
	Font.Draw ("Connect4", 155, 290, title, 52)
	Mouse.Where (xValue, yValue, button)
	if xValue >= 215 and xValue <= 455 then
	    if yValue >= 200 and yValue <= 250 then
		if button = 1 then
		    delay (1000)
		    game
		    exit
		end if
		drawbox (215, 200, 455, 250, 48)
	    else
		drawbox (215, 200, 455, 250, black)
	    end if
	    if yValue >= 130 and yValue <= 180 then
		if button = 1 then
		    drawfillbox (0, 0, 696, 400, black)
		    Font.Draw ("", 70, 350, instructionsTitle, 48)
		    Font.Draw ("", 0, 320, font2, 52)
		    Font.Draw ("", 0, 280, font2, 52)
		    Font.Draw ("", 0, 240, font2, 52)
		    Font.Draw ("", 0, 200, font2, 52)
		    Font.Draw ("", 245, 75, font2, 14)
		    pauseProgram
		    mainMenu
		end if
		drawbox (215, 130, 455, 180, 48)
	    else
		drawbox (215, 130, 455, 180, black)
	    end if
	    if yValue >= 60 and yValue <= 110 then
		if button = 1 then
		    shouldPlay := false
		    exit
		end if
		drawbox (215, 60, 455, 110, 48)
	    else
		drawbox (215, 60, 455, 110, black)
	    end if
	end if
	exit when button = 1
    end loop
end mainMenu
proc intro
    drawfillbox (0, 0, 696, 400, black)
    for i : 1 .. 400
	drawfillbox (0, 0, 696, 400, black)
	Font.Draw ("", 560 - i, 250, title, 48)
	Font.Draw ("", -255 + i, 220, subtitle, 52)
	delay (4)
    end for
    for i : 1 .. 390
	drawfilloval (40, 401 - i, 20, 20, black)
	drawfilloval (656, 401 - i, 20, 20, black)
	drawfilloval (656, 400 - i, 20, 20, 12)
	drawfilloval (40, 400 - i, 20, 20, yellow)
	delay (3)
    end for
    loop
	drawfillbox (0, 0, 696, 100, black)
	Font.Draw ("Press any Key to Continue.", 169, 20, subtitle, 12)
	delay (300)
	drawfillbox (0, 0, 696, 100, black)
	Font.Draw ("Press any Key to Continue.", 169, 20, subtitle, 14)
	delay (300)
	exit when hasch
    end loop
    mainMenu
end intro
%fork backgroundMusic
intro
