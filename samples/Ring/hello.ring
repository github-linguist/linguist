# The Ring Standard Library
# Game Engine for 2D Games
# Flappy Bird 3000 Game
# 2016, Mahmoud Fayed <msfclipper@yahoo.com>

oGameState = NULL

Load "gameengine.ring"

func main

	oGame = New Game  

	while true

	oGameState = New GameState

	oGame {
		title = "Flappy Bird 3000"
		sprite
		{
			file = "images/fbback.png"
			x = 0 y=0 width=800 height = 600 scaled = true animate = false
			keypress = func ogame,oself,nKey {
				if nkey = key_esc or nKey = GE_AC_BACK
					ogame.shutdown()
				but nKey = key_space
					oGameState.startplay=true
					ogame.shutdown=true
				ok
			}
			mouse = func ogame,oself,nType,aMouseList {
				if nType = GE_MOUSE_UP
					call oself.keypress(oGame,oSelf,Key_Space)
				ok
			}
		}
		text {
			animate = false
			size = 35
			file = "fonts/pirulen.ttf"
			text = "Flappy Bird 3000"
			x = 150	y=50
		}
		text {
			animate = false
			size = 25
			file = "fonts/pirulen.ttf"
			text = "Version 1.0"
			x = 280	y=100
		}
		text {
			animate = false
			size = 16
			file = "fonts/pirulen.ttf"
			text = "(C) 2016, Mahmoud Fayed"
			x = 245	y=140
		}

		text {
			animate = false
			size = 25
			file = "fonts/pirulen.ttf"
			text = "To Win Get Score = 3000"
			x = 150	y=270
		}

		text {
			animate = false
			size = 25
			file = "fonts/pirulen.ttf"
			text = "Press Space to start"
			x = 190	y=470
		}
		text {
			animate = false
			size = 20
			file = "fonts/pirulen.ttf"
			text = "Press Esc to Exit"
			x = 260	y=510
		}

		animate {
			file = "images/fbbird.png"
			x = 200
			y = 200
			framewidth = 20
			scaled = true
			height = 50
			width = 50
			nStep = 3
			transparent = true
			animate = true
			direction = ge_direction_random
			state = func oGame,oSelf {
				oSelf {
					nStep--
					if nStep = 0
						nStep = 3
						if frame < 3
							frame++
						else
							frame=1
						ok
					ok
					if x <= 0 x=0 ok
					if y <= 0 y=0 ok
					if x >= 750 x= 750 ok
					if y > 550 y=550 ok
				}
			}
		}

		Sound {
			file = "sound/music2.wav"
		}
	}
	if oGameState.startplay
		oGame.refresh()
		playstart(oGame)
		oGame.refresh()
	ok

	end


func playstart oGame

	oGame {
		FPS = 60
		FixedFPS = 120
		Title = "Flappy Bird 3000"
		Sprite {
			file = "images/fbback.png"
			x = 0 y=0 width=800 height = 600 scaled = true animate = false
			keypress = func ogame,oself,nKey {
				if nkey = key_esc or nKey = GE_AC_BACK
					ogame.shutdown()
				ok
			}
		}

		Map {
			blockwidth = 80
			blockheight = 80
			aMap = [
				 	[0,0,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0],
					[0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0],
					[0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,1,0,0,0],
					[0,0,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0,1,0,0,0,1,0,0,0],
					[0,0,0,0,0,0,0,0,0,3,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,3,0,0,0],
					[0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
					[0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
					[0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0]
				]
			newmap(aMap)
			aImages = ["images/fbwall.png","images/fbwallup.png",
					"images/fbwalldown.png"]
			state = func oGame,oSelf {
				if oGameState.gameresult = false
					px = oGame.aObjects[3].x
					py = oGame.aObjects[3].y
					oSelf {
						x -=  3
						if x < - 2100
							x = 0
							newmap(aMap)
						ok
						nCol =  getcol(px,0)
						if nCol=11 or nCol=15 or nCol=19 or nCol=23 or nCol=27
							if nCol != oGameState.lastcol
								oGameState.lastcol = nCol
								oGameState.Score += 100
								oGame { Sound {
									once = true
									file = "sound/sfx_point.wav"
								} }
								checkwin(oGame)
							ok
						ok
					}
					if  oSelf.getvalue(px+40,py) != 0 or
					    oSelf.getvalue(px+40,py+40) != 0 or
					    oSelf.getvalue(px,py) != 0 or
					    oSelf.getvalue(px,py+40) != 0
						oGameState.gameresult = true
						oGame {
							text {
								point = 550
								size = 30
								nStep = 3
								file = "fonts/pirulen.ttf"
								text = "Game Over !!!"
								x = 500	y=10
								state = func ogame,oself {
									if oself.y >= 550
											ogame.shutdown = true
									ok
										if oself.y = 90
										ogame {
											Sound {
												once = true
												file = "sound/sfx_die.wav"
											}
										}
									ok
								}
							}
							Sound {
								once = true
								file = "sound/sfx_hit.wav"
							}
						}
					ok
				ok
			}
		}

		animate {
			file = "images/fbbird.png"
			x = 10
			y = 10
			framewidth = 20
			scaled = true
			height = 50
			width = 50
			nStep = 3
			transparent = true
			state = func oGame,oSelf {
				oSelf {
					nStep--
					if nStep = 0
						nStep = 3
						if frame < 3
							frame++
						else
							frame=1
						ok
					ok
				}

				if not oGameState.playerwin
					oGameState.down --
					if oGameState.down = 0
						oGameState.down = 3
						oself {
							y += 25
							if y > 550 y=550 ok
						}
					ok
				ok

			}
			keypress = func ogame,oself,nKey {
				if oGameState.gameresult = false
					oself {
						if nkey = key_space
							y -= 55
							oGameState.down = 60
							if y<=0 y=0 ok
						ok
					}
				ok
			}
			mouse = func ogame,oself,nType,aMouseList {
				if nType = GE_MOUSE_UP
					call oself.keypress(oGame,oSelf,Key_Space)
				ok
			}
		}

		text {
			animate = false
			point = 400
			size = 30
			file = "fonts/pirulen.ttf"
			text = "Score : " + oGameState.score
			x = 500	y=10
			state = func oGame,oSelf {
				oSelf { text = "Score : " + oGameState.score }
			}
		}

	}

func newmap aMap
	aV = [
	[1,1,3,0,0,2,1,1],
	[1,3,0,0,0,2,1,1],
	[1,1,1,3,0,2,1,1],
	[1,1,1,3,0,0,0,0],
	[0,0,0,0,2,1,1,1],
	[0,0,2,1,1,1,1,1],
	[0,0,0,2,1,1,1,1],
	[1,1,1,3,0,2,1,1],
	[1,1,1,1,1,3,0,0],
	[3,0,0,2,1,1,1,1],
	[3,0,0,2,3,0,0,2]
	]
	for x = 10 to 24 step 4
		aVar = aV[ (random(10)+1) ]
		for y = 1 to 8
			aMap[y][x] = aVar[y]
		next
	next

func checkwin ogame
	if oGameState.score = 3000
		oGameState.gameresult = true
		oGameState.playerwin = true
		oGame {
			text {
				point = 400
				size = 30
				nStep = 3
				file = "fonts/pirulen.ttf"
				text = "You Win !!!"
				x = 500	y=10
				state = func ogame,oself {
					if oself.y >= 400
						ogame.shutdown = true
						oGameState.Score = 0
					ok
				}
			}
		}
	ok

Class GameState
	down = 3
	gameresult = false
	Score = 0
	startplay=false
	lastcol = 0
	playerwin = false
