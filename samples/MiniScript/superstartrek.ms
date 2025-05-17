// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/superstartrek.ms
// SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
//
// ****        **** STAR TREK ****        ****
// **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
// **** AS SEEN ON THE STAR TREK TV SHOW.
// **** ORIGIONAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
// **** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
// **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
// *** LEEDOM - APRIL & DECEMBER 1974,
// *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
// *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
// *** SEND TO;  R. C. LEEDOM
// ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
// ***           BOX 746, M.S. 338
// ***           BALTIMORE, MD  21203
// ***
// *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
// *** Convertod te MiniScript 10/10/23 by Joe Strout
// *** (based heavily on the Python port by @jkboyce)

import "listUtil"
import "stringUtil"
import "mathUtil"
import "vt"
color = vt.color

printMark = function(s)
	if not s then; print; return; end if
	boldOn = false
	pos = -1
	while pos+1 < s.len
		nextPos = s.indexOf("\", pos)
		if nextPos < 0 then nextPos = pos.len
		if boldOn then code = vt.bold + vt.textColor(color.yellow) else code = vt.normal
		boldOn = not boldOn
		print code + s[pos+1:nextPos], ""
		pos = nextPos
	end while
	if not boldOn then print vt.normal, ""
	print
end function
		
//=================================================================
// Entity: map of strings used both as unique identifiers for
// things that can be at any position in a quadrant, AND also used
// as display strings in the short-range scan (and instructions).
//=================================================================
color.klingon = color.red
color.ship = vt._getColorIndex("#8888FF")
color.starbase = vt._getColorIndex("#44CCFF")
color.star = vt._getColorIndex("#FFFF88")
Entity = {}
Entity.klingon = vt.textColor(color.klingon) + "+K+" + vt.normal
Entity.ship = vt.textColor(color.ship) + "<*>" + vt.normal
Entity.empty = "   "
Entity.starbase = vt.textColor(color.starbase) + ">!<" + vt.normal
Entity.star = vt.textColor(color.star) + " * " + vt.normal

//=================================================================
// Instructions
// (This was originally in a separate program, superstartreckins.bas,
// for memory reasons.  I've chosen to incorporate it into the main
// program, as that's more convenient for everybody.)
//=================================================================
Instructions = {}
Instructions.print = function
	print
	print "At each pause, press Return to continue."
	input

	printMark "      Instructions for 'Super Star Trek'"
	printMark
	printMark "1. When you see \Command?\ printed, enter one of the legal"
	printMark "     commands (NAV,SRS,LRS,PHA,TOR,SHE,DAM,COM,HLP, OR XXX)."
	printMark "2. If you should type in an illegal command, you'll get a short"
	printMark "     list of the legal commands printed out."
	printMark "3. Some commands require you to enter data (for example, the"
	printMark "     'NAV' command comes back with 'Course (1-9)?'.)  If you"
	printMark "     type in illegal data (like negative numbers), that command"
	printMark "     will be aborted"
	printMark
	printMark "     The galaxy is divided into an 8 x 8 quadrant grid,"
	printMark "and each quadrant is further divided into an 8 x 8 sector grid."
	printMark
	printMark "     You will be assigned a starting point somewhere in the"
	printMark "galaxy to begin a tour of duty as comander of the starship"
	printMark "\Enterprise\; your mission: to seek and destroy the fleet of"
	printMark "klingon warwhips which are menacing the United Federation of"
	printMark "Planets."
	input
	
	printMark
	printMark "     You have the following commands available to you as captain"
	printMark "of the starship Enterprise:"
	printMark
	printMark "\NAV\ command = warp engine control --"
	printMark "     course is in a circular numerical      4  3  2"
	printMark "     vector arrangement as shown             . | ."
	printMark "     integer and real values may be           .|."
	printMark "     used.  (Thus course 1.5 is half-     5 ---*--- 1"
	printMark "     way between 1 and 2.)                    .|."
	printMark "                                             . | ."
	printMark "     Values may approach 9.0, which         6  7  8"
	printMark "     itself is equivalent to 1.0"
	printMark "                                            course"
	printMark "     One warp factor is the size of "
	printMark "     one quadrant.  Therefore, to get"
	printMark "     from quadrant 6,5 to 5,5, you would"
	printMark "     use course 3, warp factor 1."
	input
	
	printMark
	printMark "\SRS\ command = short range sensor scan"
	printMark "     Shows you a scan of your present quadrant."
	printMark
	printMark "     Symbology on your sensor screen is as follows:"
	printMark "        " + Entity.ship + " = your starship's position"
	printMark "        " + Entity.klingon + " = klingon battle cruiser"
	printMark "        " + Entity.starbase + " = federation starbase (refuel/repair/re-arm here!)"
	printMark "        " + Entity.star + " = star"
	printMark
	printMark "     A condensed 'status report' will also be presented."
	input
	
	printMark
	printMark "\LRS\ command = long range sensor scan"
	printMark "     Shows conditions in space for one quadrant on each side"
	printMark "     of the enterprise (which is in the middle of the scan)."
	printMark "     The scan is coded in the form \###\, where the units digit"
	printMark "     is the number of stars, the tens digit is the number of"
	printMark "     starbases, and the hundresds digit is the number of"
	printMark "     klingons."
	printMark
	printMark "     Example - 207 = 2 klingons, no starbases, & 7 stars."
	input
	
	printMark
	printMark "\PHA\ command = phaser control."
	printMark "     Allows you to destroy the klingon battle cruisers by "
	printMark "     zapping them with suitably large units of energy to"
	printMark "     deplete their shield power.  (Remember, klingons have"
	printMark "     phasers too!)"
	printMark
	printMark "\TOR\ command = photon torpedo control"
	printMark "     Torpedo course is the same as used in warp engine control."
	printMark "     If you hit the klingon vessel, he is destroyed and"
	printMark "     cannot fire back at you.  If you miss, you are subject to"
	printMark "     his phaser fire.  In either case, you are also subject to "
	printMark "     the phaser fire of all other klingons in the quadrant."
	printMark
	printMark "     The library-computer (\COM\ command) has an option to "
	printMark "     compute torpedo trajectory for you (option 2)."
	input
	
	printMark
	printMark "\SHE\ command = shield control"
	printMark "     Defines the number of energy units to be assigned to the"
	printMark "     shields.  Energy is taken from total ship's energy.  Note"
	printMark "     that the status display total energy includes shield energy."
	printMark
	printMark "\DAM\ command = damage control report"
	printMark "     Gives the state of repair of all devices, where a negative"
	printMark "     'state of repair' shows that the device is temporarily"
	printMark "     damaged."
	printMark
	printMark "\HLP\ command = help"
	printMark "     Displays these instructions."
	input
	
	printMark
	printMark "\COM\ command = library-computer"
	printMark "     The library-computer contains six options:"
	printMark "     option \0\ = \cumulative galactic record\"
	printMark "         This option showes computer memory of the results of all"
	printMark "        previous short and long range sensor scans."
	printMark "     option \1\ = \status report\"
	printMark "        This option shows the number of klingons, stardates,"
	printMark "        and starbases remaining in the game."
	printMark "     option \2\ = \photon torpedo data\"
	printMark "        Which gives directions and distance from the enterprise"
	printMark "        to all klingons in your quadrant."
	printMark "     option \3\ = \starbase nav data\"
	printMark "        This option gives direction and distance to any "
	printMark "        starbase within your quadrant."
	printMark "     option \4\ = \direction/distance calculator\"
	printMark "        This option allows you to enter coordinates for"
	printMark "        direction/distance calculations."
	printMark "     option \5\ = \galactic region name map\"
	printMark "        This option prints the names of the sixteen major "
	printMark "        galactic regions referred to in the game."
	input
	print
end function

//=================================================================
// Constants (tweak these to make the game easier or harder!)
//=================================================================
Constant = {}
Constant.klingonShieldStrength = 200
Constant.maxEnergy = 3000
Constant.maxTorpedos = 10
Constant.minDays = 25
Constant.maxDays = 34

// Directions.  NOTE: this game has an unusual coordinate
// system, where x is the row (higher X values are further
// down on the screen), and y is the column (higher Y values
// are further to the right).
dirs = [	// (down, right)
    [0, 1],		// 1: go right (same as #9)
    [-1, 1],	// 2: go up-right
    [-1, 0],	// 3: go up  (lower x-coordines; north)
    [-1, -1],	// 4: go up-left (north-west)
    [0, -1],	// 5: go left (west)
    [1, -1],	// 6: go down-left (south-west)
    [1, 0],		// 7: go down (higher x-coordines; south)
    [1, 1],		// 8: go down-right
    [0, 1]]		// 9: go right (east)



//=================================================================
// Global Utility Functions
//=================================================================

// Generate a random integer from 0 to 7 inclusive.
fnr = function
	return floor(rnd * 8)
end function

// Generate a random integer between any two limits (inclusive).
randInt = function(minVal, maxVal)
	return floor(rnd * (maxVal - minVal + 1)) + minVal
end function

// Print distance and direction between two points on a grid.
printDirection = function(source, dest)
	delta1 = -(dest.x - source.x)
	delta2 = dest.y - source.y
	
	if delta2 > 0 then
		if delta1 < 0 then
			base = 7
		else
			base = 1
			temp = delta1; delta1 = delta2; delta2 = temp
		end if
	else
		if delta1 > 0 then
			base = 3
		else
			base = 5
			temp = delta1; delta1 = delta2; delta2 = temp
		end if
	end if

	delta1 = abs(delta1)
	delta2 = abs(delta2)
	if delta1 or delta2 then	// bug in original: failed when source == dest
		if delta1 >= delta2 then
			print "Direction = " + round(base + delta2 / delta1, 6)
		else
			print "Direction = " + round(base + 2 - delta1 / delta2, 6)
		end if
	end if
	print "Distance = " + round(sqrt(delta1 ^ 2 + delta2 ^ 2), 6)
end function

printCourseChart = function
	print vt.cursor.save + vt.cursor.up(8) + vt.textColor(color.gray), ""
	x = 55
	print vt.cursor.right(x) + "   4  3  2"
	print vt.cursor.right(x) + "    . | ."
	print vt.cursor.right(x) + "     .|."
	print vt.cursor.right(x) + " 5 ---*--- 1"
	print vt.cursor.right(x) + "     .|."
	print vt.cursor.right(x) + "    . | ."
	print vt.cursor.right(x) + "   6  7  8"
	print vt.cursor.restore + vt.normal, ""
end function

getYesNo = function(prompt)
	while true
		ans = input(prompt + "? ").lower + " "
		if ans[0] == "y" then return "yes"
		if ans[0] == "n" then return "no"
		print "Please answer Yes or No."
	end while
end function

getXY = function(prompt)
	while true
		ans = input(prompt + "? ").split(",")
		if ans.len == 2 then return {"x":ans[0].val, "y":ans[1].val}
		print "Please enter two numbers separated by a comma.  Example: 5,2"
	end while
end function

string.padBoth = function(length, padChar=" ")
	// Pad a string on BOTH sides, so that it ends up centered.
	if self.len > length then return self.len[:length]
	extra = length - self.len
	leftExtra = floor(extra/2)
	rightExtra = extra - leftExtra
	return padChar * leftExtra + self + padChar * rightExtra
end function


//=================================================================
// Point class: represents an X,Y position.  Note that because
// the original game used 1-based coordinates, we have a conversion
// to string method that adds 1 to X and Y.  But internally, these
// are assumed to be 0-based.
//=================================================================
Point = {}
Point.make = function(x,y)
	p = new Point
	p.x = x
	p.y = y
	return p
end function
Point.random = function
	return Point.make(fnr, fnr)
end function
Point.oneBasedStr = function
	return (self.x + 1) + " , " + (self.y + 1)
end function

//=================================================================
// Position: combination of quadrant and sector
//=================================================================
Position = {}
Position.make = function(quadX, quadY, secX, secY)
	pos = new Position
	pos.quadrant = Point.make(quadX, quadY)
	pos.sector = Point.make(secX, secY)
	return pos
end function
Position.random = function
	return Position.make(fnr, fnr, fnr, fnr)
end function

//=================================================================
// Quadrant: represents one area of the map
//=================================================================
Quadrant = {}
Quadrant.make = function(quadrantX, quadrantY, klingons, bases, stars)
	q = new Quadrant
	q.point = Point.make(quadrantX, quadrantY)
	q.klingons = klingons
	q.bases = bases
	q.stars = stars
	q.extraRepairTime = rnd * 0.5
	q.klingonShips = []	// (list of KlingonShip)
	q.starbase = null	// (Point of starbase, if any)
	q.charted = false
	q.data = null
	return q
end function

// Get the name of the region of this quadrant (e.g. "Rigel").
Quadrant.name = function
	region1 = [
		"Antares",
		"Rigel",
		"Procyon",
		"Vega",
		"Canopus",
		"Altair",
		"Sagittarius",
		"Pollux"]
	region2 = [
		"Sirius",
		"Deneb",
		"Capella",
		"Betelgeuse",
		"Aldebaran",
		"Regulus",
		"Arcturus",
		"Spica"]
	if self.point.y < 4 then return region1[self.point.x]
	return region2[self.point.x]
end function

// Get the full name of this quadrant (e.g. "Rigel IV")
Quadrant.fullName = function
	return self.name + " " + ["I", "II", "III", "IV"][self.point.y % 4]
end function

// Store the given entity for the given cell in this quadrant
// (rounding to the nearest integer coordinates).
Quadrant.setValue = function(x, y, entity)
	self.data[round(x)][round(y)] = entity
end function

// Get the entity in the given cell of this quadrant
// (again rounding to the nearest integers).
Quadrant.value = function(x, y)
	return self.data[round(x)][round(y)]
end function

// Find an empty position within this quadrant.
Quadrant.findEmptyPoint = function
	while true
		p = Point.random
		if self.value(p.x, p.y) == Entity.empty then return p
	end while
end function

Quadrant.addStarbase = function
	pos = self.findEmptyPoint
	self.setValue pos.x, pos.y, Entity.starbase
	self.starbase = pos	// (note: assumes 0 or 1 starbases)
end function

// Fill out the contents of this quadrant, given the position of
// the ship and our number of klingons, bases, and stars.
Quadrant.populate = function(shipPos)
	self.data = list.init2d(8, 8, Entity.empty)
	if shipPos != null then self.setValue shipPos.x, shipPos.y, Entity.ship
	for i in range(0, self.klingons-1, 1)
		pos = self.findEmptyPoint
		self.setValue pos.x, pos.y, Entity.klingon
		self.klingonShips.push KlingonShip.make(pos.x, pos.y)
	end for
	if self.bases > 0 then self.addStarbase
	for i in range(0, self.stars-1, 1)
		pos = self.findEmptyPoint
		self.setValue pos.x, pos.y, Entity.star
	end for
end function

// Return the three-digit number that represents the contents of 
// this quadrant in a long-range scan: <klingons><bases><stars>
Quadrant.lrsValue = function
	return vt.textColor(color.klingon) + str(self.klingons) + 
	  vt.textColor(color.starbase) + str(self.bases) +
	  vt.textColor(color.star) + str(self.stars) + vt.normal
end function

// Return a list of lines representing the short-range scan
// of this quadrant (i.e., a representation of what entities
// it contains).
Quadrant.scanLines = function
	result = []
	for row in self.data
		result.push row.join
	end for
	return result
end function

//=================================================================
// KlingonShip: represents an enemy ship.  These are very simple;
// they need only their position (sector) within the quadrant,
// and their current shield level.
//=================================================================
KlingonShip = {}
KlingonShip.make = function(x, y)
	k = new KlingonShip
	k.sector = Point.make(x,y)
	k.shield = Constant.klingonShieldStrength * (rnd + 0.5)
	return k
end function
KlingonShip.distance = function(ship)
	// Find distance, in sector units, between this ship and the given
	// (player) ship.  Assumes both are in the same quadrant.
	return mathUtil.distance(self.sector, ship.position.sector)
end function
	
//=================================================================
// Ship: represents the starship Enterprise and all its systems.
//=================================================================
Ship = {}
Ship.maxEnergy = Constant.maxEnergy
Ship.maxTorpedos = Constant.maxTorpedos
Ship.init = function
	self.position = Position.random
	self.devices = [
		"Warp Engines",
		"Short Range Sensors",
		"Long Range Sensors",
		"Phaser Control",
		"Photon Tubes",
		"Damage Control",
		"Shield Control",
		"Library-Computer",
	]
	self.deviceStatus = [0] * self.devices.len  // > 0 means working; < 0 means broken
	self.docked = false
	self.shields = 0
	self.refill
end function

Ship.useWarpEnergy = function(warpRounds)
	// Note: movement costs 10 energy per sub-step (sector change).
	self.energy -= warpRounds * 10
	if self.energy < 0 then
		print "Shield control supplies energy to complete the maneuver."
		self.shields += self.energy
		self.energy = 0
		if self.shields < 0 then self.shields = 0
	end if
end function

Ship.refill = function
	self.energy = self.maxEnergy
	self.torpedos = self.maxTorpedos
end function

Ship.stranded = function
	return self.shields + self.energy <= 10 or
	  (self.energy <= 10 and self.deviceStatus[6] < 0)
end function

//=================================================================
// Galaxy: all 64 quadrants, plus global information about how many
// klingons there are, the current game time, etc.  Basically 
// represents the state of the game (but not the flow; that happens
// in the Game class).
//=================================================================
Galaxy = {}
Galaxy.init = function(minDuration=20)
	self.qtyKlingons = 0
	self.qtyBases = 0
	self.startDate = 100 * randInt(20, 39)
	self.endDate = self.startDate + randInt(Constant.minDays, Constant.maxDays)
	self.stardate = self.startDate
	self.ship = new Ship
	self.ship.init
	self.quadrants = list.init2d(8, 8)
	for x in range(0, 7)
		for y in range(0, 7)
			klingons = 0
			r1 = rnd
			if r1 > 0.80 then klingons = 1
			if r1 > 0.95 then klingons = 2
			if r1 > 0.98 then klingons = 3
			bases = (rnd > 0.96)
			self.quadrants[x][y] = Quadrant.make(x, y, klingons, bases, fnr+1)
			self.qtyKlingons += klingons
			self.qtyBases += bases
		end for
	end for
	self.missionDuration = minDuration
	if self.qtyKlingons > self.missionDuration then
		self.missionDuration = self.qtyKlingons + 1
	end if
	here = self.localQuadrant
	if self.qtyBases == 0 then self.qtyBases = 1
	self.ship.position.sector = Point.random
end function

Galaxy.daysElapsed = function; return self.stardate - self.startDate; end function
Galaxy.daysLeft = function; return self.endDate - self.stardate; end function
Galaxy.missionOver = function; return self.stardate > self.endDate; end function

Galaxy.localQuadrant = function
	qp = self.ship.position.quadrant
	return self.quadrants[qp.x][qp.y]
end function

Galaxy.printLongRangeScan = function(point)
	sep = "-" * 19
	for x in range(point.x - 1, point.x + 1)
		print sep
		entries = []
		for y in range(point.y - 1, point.y + 1)
			if not (0 <= x <= 7 and 0 <= y <= 7) then
				entries.push vt.textColor(color.gray) + "***"
			else
				q = self.quadrants[x][y]
				entries.push q.lrsValue
				q.charted = true
			end if
		end for
		print ": " + entries.join(" : ") + " :"
	end for
	print sep
end function

//=================================================================
// Game: represents the flow of the game, and handles user actions.
//=================================================================
Game = {}
Game.init = function
	self.galaxy = new Galaxy
	self.galaxy.init
	self.gameOver = false
end function

Game.printIntro = function
	print;print;print;print;print;print;print;print;print;print
	print vt.textColor("#8888FF")
	print "                                    ,------*------,"
	print "                    ,-------------   '---  ------'"
	print "                     '-------- --'      / /"
	print "                         ,---' '-------/ /--,"
	print "                          '----------------'"
	print vt.textColor("#CCCCCC")
	print "                    THE USS ENTERPRISE --- NCC-1701"
	print vt.normal
	print;print;print;print
	k = self.galaxy.qtyKlingons
	sb = self.galaxy.qtyBases
	if sb == 1 then isAre = "is" else isAre = "are"
	days = self.galaxy.endDate - self.galaxy.startDate
	orders = ["Your orders are as follows:"]
	orders.push "     Destroy the \" + k + "\ Klingon warships which have invaded"
	orders.push "   the galaxy before they can attack federation headquarters"
	orders.push "   on stardate \" + self.galaxy.endDate + "\.  This gives you \" + 
	   days + "\ days.  There " + isAre
 	orders.push "   \" + sb + "\ starbase" + "s"*(sb!=1) + " in the galaxy for resupplying your ship."
 	for line in orders
 		printMark line		// for extra drama, add a `wait 0.5` line here!
 		wait 0.5
 	end for
 	print
 	input "Press Return when ready to accept command"
 	print
 	self.enterQuadrant
end function

Game.enterQuadrant = function
	// Populate the local quadrant and print a short-range scan.
	ship = self.galaxy.ship
	here = self.galaxy.localQuadrant
	here.charted = true
	here.populate ship.position.sector
	
	if self.galaxy.daysElapsed == 0 then
		print "Your mission begins with your ship located"
		print "in the galactic quadrant, '" + here.fullName + "'"
	else
		print "Now entering " + here.fullName + " quadrant . . ."
	end if
	print

	if here.klingons > 0 then
		print vt.textColor(color.red) + "COMBAT AREA      CONDITION RED" + vt.normal
		if ship.shields <= 200 then 
			print vt.textColor(color.red) + "   SHIELDS DANGEROUSLY LOW" + vt.normal
		end if
	end if
	self.shortRangeScan
end function

Game.shortRangeScan = function
	ship = self.galaxy.ship
	ship.docked = false
	quad = self.galaxy.localQuadrant
	cs = null
	for x in range(ship.position.sector.x - 1, ship.position.sector.x + 1)
		for y in range(ship.position.sector.y - 1, ship.position.sector.y + 1)
			if 0 <= x <= 7 and 0 <= y <= 7 and quad.value(x, y) == Entity.starbase then
				ship.docked = true
				cs = "DOCKED"
				ship.refill
				print "Shields dropped for docking purposes"
				ship.shields = 0
			end if
		end for
	end for
	if not cs then
		if quad.klingons then
			cs = vt.textColor(color.red) + "*RED*" + vt.normal
		else if ship.energy < ship.maxEnergy * 0.1 then
			cs = vt.textColor(color.yellow) + "*YELLOW*" + vt.normal
		else
			cs = vt.textColor(color.green) + "GREEN" + vt.normal
		end if
	end if
	if ship.deviceStatus[1] < 0 then
		print
		print vt.textColor(color.gray) + "*** Short range sensors are out ***" + vt.normal
		return
	end if
	print "-"*33
	for x in range(0, 7)
		line = quad.data[x].join
		if x == 0 then
			line += "        STARDATE           " + round(self.galaxy.stardate, 1)
		else if x == 1 then
			line += "        CONDITION          " + cs
		else if x == 2 then
			line += "        QUADRANT           " + ship.position.quadrant.oneBasedStr
		else if x == 3 then
			line += "        SECTOR             " + ship.position.sector.oneBasedStr
		else if x == 4 then
			line += "        PHOTON TORPEDOES   " + ship.torpedos
		else if x == 5 then
			line += "        TOTAL ENERGY       " + floor(ship.energy + ship.shields)
		else if x == 6 then
			line += "        SHIELDS            "
			if ship.shields <= 0 then
				line += vt.textColor(color.red) + floor(ship.shields) + vt.normal
			else
				line += floor(ship.shields)
			end if
		else if x == 7 then
			line += "        KLINGONS REMAINING " + self.galaxy.qtyKlingons
		end if
		print line
	end for
	print "-"*33
end function

Game.longRangeScan = function
	ship = self.galaxy.ship
	if ship.deviceStatus[2] < 0 then
		print "Long range sensors are inoperable"
		return
	end if
	print "Long range scan for quadrant " + ship.position.quadrant.oneBasedStr
	self.galaxy.printLongRangeScan ship.position.quadrant
end function

Game.updateKlingons = function
	// Move klingons around randomly within the sector
	here = self.galaxy.localQuadrant
	if not here.klingonShips then return
	
	for klingonShip in here.klingonShips
		if klingonShip.shield <= 0 then continue	// (already disabled/destroyed)
		here.setValue klingonShip.sector.x, klingonShip.sector.y, Entity.empty
		klingonShip.sector = here.findEmptyPoint
		here.setValue klingonShip.sector.x, klingonShip.sector.y, Entity.klingon
	end for
	
	self.klingonsFire
end function

Game.klingonsFire = function
	// Nearby klingons fire on the player ship.
	here = self.galaxy.localQuadrant
	if here.klingons <= 0 then return
	ship = self.galaxy.ship
	if ship.docked then
		print "Starbase shields protect the Enterprise"
		return
	end if
	for klingonShip in here.klingonShips
		if klingonShip.shield <= 0 then continue	// (already disabled/destroyed)
		hit = floor((klingonShip.shield / klingonShip.distance(ship)) * (rnd + 2))
		ship.shields -= hit
		print " " + hit + " unit hit on Enterprise from sector " + klingonShip.sector.oneBasedStr
		if ship.shields <= 0 then
			self.endGame false, false, true
			return
		end if
		print "      <Shields down to " + ship.shields + " units>"
		if hit >= 20 and rnd < 0.60 and hit / ship.shields > 0.02 then
			deviceNum = floor(rnd * ship.devices.len)
			ship.deviceStatus[deviceNum] -= hit / ship.shields + 0.5 * rnd
			print "Damage control reports  '" + ship.devices[deviceNum] + " damaged by the hit'"
		end if
	end for	
end function

Game.damControlWhileUnderway = function(improvement=1)
	// Work on repairing damaged devices, and 20% of the time, do additional work on 
	// some random device (which may make it better or worse).
	ship = self.galaxy.ship
	first = true
	for i in range(0, ship.devices.len-1)
		if ship.deviceStatus[i] >= 0 then continue
		ship.deviceStatus[i] += improvement
		if -0.1 < ship.deviceStatus[i] < 0 then ship.deviceStatus[i] = -0.1
		if ship.deviceStatus[i] >= 0 then
			if first then s = "Damage control report:" else s = ""
			s += "   " + ship.devices[i] + " repair completed"
			print s
			first = false
		end if
	end for
	
	if rnd > 0.2 then return
	deviceNum = floor(rnd * ship.devices.len)
	if rnd < 0.6 then
		ship.deviceStatus[deviceNum] -= rnd * 5 + 1
		print "Damage control report:   " + ship.devices[deviceNum] + " damaged"
	else
		ship.deviceStatus[deviceNum] += rnd * 3 + 1
		print "Damage control report:   " + ship.devices[deviceNum] + " state of repair improved"
	end if
end function	

Game.navigate = function
	galaxy = self.galaxy
	ship = galaxy.ship
	printCourseChart
	cd = input("Course (1-9)? ").val - 1	// (convert input to 0-8)
	if cd == dirs.len - 1 then cd == 0
	if cd < 0 or cd >= dirs.len then
		print "   Lt. Sulu reports, 'Incorrect course data, sir!'"
		return
	end if
	
	if ship.deviceStatus[0] < 0 then maxWarp = 0.2 else maxWarp = 8
	warp = input("Warp factor (0-" + maxWarp + ")? ").val
	if warp > maxWarp and maxWarp < 1 then
		print "Warp engines are damaged. Maximum speed = warp " + maxWarp
		return
	end if
	if warp == 0 then return
	if warp < 0 or warp > 8 then
		print "   Chief engineer Scott reports 'The engines won't take warp " + warp + "!'"
		return
	end if
	
	warpRounds = round(warp * 8)
	// Note that we check for sufficient energy based on 1/10th of what it
	// will actually cost.  This is apparently intentional.
	if ship.energy < warpRounds then
		print "Engineering reports   'Insufficient energy available"
		print "                       for maneuvering at warp " + warp + "!'"
		if ship.shields >= warpRounds - ship.energy and ship.deviceStatus[6] >= 0 then
			print "Deflector control room acknowledges " + ship.shields + " units of energy"
			print "                         presently deployed to shields."
		end if
		return
	end if
	
	self.updateKlingons
	self.damControlWhileUnderway
	
	here = galaxy.localQuadrant
	here.setValue ship.position.sector.x, ship.position.sector.y, Entity.empty
	startQuad = Point.make(here.point.x, here.point.y)
//	print "DEBUG set value at " + ship.position.sector.x + "," + ship.position.sector.y + " to Entity.empty"
	// interpolate the direction
	cdi = floor(cd)
	dx = mathUtil.lerp(dirs[cdi][0], dirs[cdi+1][0], cd - cdi)
	dy = mathUtil.lerp(dirs[cdi][1], dirs[cdi+1][1], cd - cdi)
	finalGlobalX = ship.position.quadrant.x * 8 + ship.position.sector.x + dx * warpRounds
	finalGlobalY = ship.position.quadrant.y * 8 + ship.position.sector.y + dy * warpRounds
	for i in range(1, warpRounds)
		ship.position.sector.x += dx
		ship.position.sector.y += dy
		if not (0 <= ship.position.sector.x <= 7 and 0 <= ship.position.sector.y <= 7) then
			// Exceeded quadrant limits; calculate final position.
			// Note that we randomly re-generate all the stuff in a quadrant every
			// time we enter it.  So we can jump right to the final position now;
			// We only check for collisions when moving *within* your starting sector.
			// your starting sector.
			ship.position.quadrant.x = floor(finalGlobalX / 8)
			ship.position.quadrant.y = floor(finalGlobalY / 8)
			ship.position.sector.x = finalGlobalX  - ship.position.quadrant.x * 8
			ship.position.sector.y = finalGlobalY  - ship.position.quadrant.y * 8
			hitEdge = false
			if ship.position.quadrant.x < 0 then
				hitEdge = true
				ship.position.quadrant.x = 0
				ship.position.sector.x = 0
			else if ship.position.quadrant.x > 7 then
				hitEdge = true
				ship.position.quadrant.x = 7
				ship.position.sector.x = 7
			end if
			if ship.position.quadrant.y < 0 then
				hitEdge = true
				ship.position.quadrant.y = 0
				ship.position.sector.y = 0
			else if ship.position.quadrant.y > 7 then
				hitEdge = true
				ship.position.quadrant.y = 7
				ship.position.sector.y = 7
			end if
			if hitEdge then
				print "Lt. Uhura reports message from Starfleet Command:"
				print "  'Permission to attempt crossing of galactic perimeter"
				print "  is hereby *denied*. Shut down your engines.'"
				print "Chief Engineer Scott reports  'Warp engines shut down"
				print "  at sector " + ship.position.sector.oneBasedStr + 
				  " of quadrant " + ship.position.quadrant.oneBasedStr + ".'"
			end if
			break
		end if
		x = floor(ship.position.sector.x)
		y = floor(ship.position.sector.y)
		if here.value(x, y) != Entity.empty then
			ship.position.sector.x = floor(x - dx)
			ship.position.sector.y = floor(y - dy)
			print "Warp engines shut down at sector " + ship.position.sector.oneBasedStr +
			  " due to bad navigation"
			break
		end if
	end for
	ship.position.sector.x = floor(ship.position.sector.x)
	ship.position.sector.y = floor(ship.position.sector.y)
	doScan = true
	if ship.position.quadrant != startQuad then
		self.enterQuadrant
		doScan = false
	else
		here.setValue ship.position.sector.x, ship.position.sector.y, Entity.ship
//		print "DEBUG set value at " + ship.position.sector.x + "," + ship.position.sector.y + " to Entity.ship"
	end if
	ship.useWarpEnergy warpRounds
	if warp < 1 then galaxy.stardate += round(warp, 1) else galaxy.stardate += 1
	if galaxy.missionOver then
		self.endGame false, false, false
	else if doScan then
		self.shortRangeScan
	end if
end function

Game.damageControl = function
	ship = self.galaxy.ship
	if ship.deviceStatus[5] < 0 then
		print "Damage control report not available."
	else
		print
		print "DEVICE             STATE OF REPAIR"
		for i in range(0, ship.devices.len-1)
			n = round(ship.deviceStatus[i], 2)
			if n < 0 then n = vt.textColor(color.red) + n + vt.normal
			print ship.devices[i].pad(26) + n
		end for
		print
	end if

	if not ship.docked then return
	
	repairTime = 0
	for status in ship.deviceStatus
		if status < 0 then repairTime += 0.1
	end for
	if repairTime == 0 then return
	
	repairTime += self.galaxy.localQuadrant.extraRepairTime
	if repairTime >= 1 then repairTime = 0.9
	print
	print "Technicians standing by to effect repairs to your ship;"
	print "estimated time to repair: " + round(repairTime, 2) + " stardates"
	if getYesNo("Will you authorize the repair order (Y/N)") == "no" then return
	
	for i in range(0, ship.devices.len-1)
		if ship.deviceStatus[i] < 0 then ship.deviceStatus[i] = 0
	end for
	self.galaxy.stardate += repairTime + 0.1
end function

Game.shieldControl = function
	ship = self.galaxy.ship
	if ship.deviceStatus[6] < 0 then
		print "Shield control inoperable"
		return
	end if
	totalEnergy = ship.energy + ship.shields
	shieldEnergy = input("Energy available = " + totalEnergy + "  Number of units to shields? ")
	if shieldEnergy == "" then shieldEnergy = ship.shields else shieldEnergy = shieldEnergy.val
	if shieldEnergy > totalEnergy then
		print "Shield control reports  'This is not the federation treasury.'"
		print
		shieldEnergy = -1
	end if
	if shieldEnergy < 0 or shieldEnergy == ship.shields then
		print "<Shields unchanged>"
		return
	end if
	ship.energy += ship.shields - shieldEnergy
	ship.shields = shieldEnergy
	print "Deflector control room report:"
	print "  'Shields now at " + ship.shields + " units per your command.'"
end function

Game.computer = function
	galaxy = self.galaxy
	ship = galaxy.ship
	if ship.deviceStatus[7] < 0 then
		print "Computer disabled"
		return
	end if
	
	while true
		command = input("Computer active and awaiting command? ")
		print
		if command == "0" then	// Cumulative Galactic Record
			print
			print "        COMPUTER RECORD OF GALAXY FOR QUADRANT " + 
			  ship.position.quadrant.oneBasedStr
			print "       1     2     3     4     5     6     7     8"
			sep = "     ----- ----- ----- ----- ----- ----- ----- -----"
			print sep
			for i in range(0,7)
				line = " " + (i+1) + " "
				for j in range(0, 7)
					line += "   "
					if galaxy.quadrants[i][j].charted then
						line += galaxy.quadrants[i][j].lrsValue
					else
						line += vt.textColor(color.gray) + "***" + vt.normal
					end if
				end for
				print line
				print sep
			end for
		else if command == "1" then	// Status Report
			print "   STATUS REPORT:"
			printMark "Klingon" + "s" * (galaxy.qtyKlingons != 1) + 
			" left: \" + galaxy.qtyKlingons + "\"
			printMark "Mission must be completed in \" + 
			  round(galaxy.daysLeft, 1) + "\ stardates"

			if galaxy.qtyBases == 0 then
				print "Your stupidity has left you on your own in"
				print "  the galaxy -- you have no starbases left!"
			else
				printMark "The Federation is maintaining \" + galaxy.qtyBases +
			     "\ starbase" + "s" * (galaxy.qtyBases != 1) + " in the galaxy"
			end if

			self.damageControl
		else if command == "2" then	// Photon Torpedo Data
			here = galaxy.localQuadrant
			if here.klingons <= 0 then
				print "Science officer Spock reports  'Sensors show no enemy ships"
				print "                                in this quadrant'"
			else
				print "FROM ENTERPRISE TO KLINGON BATTLE CRUISER" + "S" * (here.klingons != 1)
				for klingonShip in here.klingonShips
					if klingonShip.shield <= 0 then continue
					printDirection ship.position.sector, klingonShip.sector
				end for
			end if
		else if command == "3" then	// Starbase Nav Data
			here = galaxy.localQuadrant
			if not here.starbase then
				print "Science officer Spock reports,  'Sensors show no starbases"
				print "                                 in this quadrant.'"
			else
				print "FROM ENTERPRISE TO STARBASE:"
				printDirection ship.position.sector, here.starbase
			end if
		else if command == "4" then	// Direction/Distance Calculator
			print "DIRECTION/DISTANCE CALCULATOR:"
			print "You are at quadrant " + ship.position.quadrant.oneBasedStr +
			  " SECTOR " + ship.position.sector.oneBasedStr
			print "Please enter"
			fromXY = getXY("  Initial coordinates (X,Y)")
			toXY = getXY("  Final coordinates (X,Y)")
			printDirection fromXY, toXY
		else if command == "5" then	// Galaxy 'Region Name' Map
			print
			print "                       THE GALAXY"
			print "       1     2     3     4     5     6     7     8"
			sep = "     ----- ----- ----- ----- ----- ----- ----- -----"
			print sep
			for i in range(0,7)
				line = " " + (i+1) + " "
				line += galaxy.quadrants[i][0].name.padBoth(25)
				line += galaxy.quadrants[i][4].name.padBoth(25)
				print line
				print sep
			end for
		else
			print "Functions available from Library-Computer:"
			print "   0 = Cumulative Galactic Record"
			print "   1 = Status Report"
			print "   2 = Photon Torpedo Data"
			print "   3 = Starbase Nav Data"
			print "   4 = Direction/Distance Calculator"
			print "   5 = Galaxy 'Region Name' Map"
			continue
		end if
		print
		break	// break out after any valid command
	end while
end function

Game.destroyKlingon = function(x, y)
	// Destroy the Klingon in the local quadrant at sector x,y.
	// Return true if the game is now won (last klingon destroyed),
	// or false otherwise.
	galaxy = self.galaxy
	here = galaxy.localQuadrant
	print "*** KLINGON DESTROYED ***"
	galaxy.qtyKlingons -= 1
	here.klingons -= 1
	here.setValue x, y, Entity.empty
	for i in here.klingonShips.indexes
		ks = here.klingonShips[i]
		if ks.sector.x == x and ks.sector.y == y then
			ks.shield = 0
			here.klingonShips.remove i
			break
		end if
	end for
		
	if galaxy.qtyKlingons <= 0 then
		self.endGame true, false, false
		return true
	end if
	return false
end function


Game.phasers = function
	galaxy = self.galaxy
	ship = galaxy.ship
	here = galaxy.localQuadrant
	klingonShips = here.klingonShips
	
	if ship.deviceStatus[3] < 0 then
		print "Phasers inoperative"
		return
	end if
	
	if here.klingons <= 0 then
		print "Science officer Spock reports  'Sensors show no enemy ships"
		print "                                in this quadrant'"
		return
	end if
	
	if ship.deviceStatus[7] < 0 then
		print "Computer failure hampers accuracy"
	end if
	
	print "Phasers locked on target;  energy available = " + ship.energy + " units"
	while true
		phaserPower = input("Number of units to fire? ").val
		if phaserPower <= 0 then return
		if phaserPower <= ship.energy then break
		print "Energy available = " + ship.energy + " units"
	end while
	
	ship.energy -= phaserPower
	if ship.deviceStatus[7] < 0 then		// (bug in original; was d(6)
		phaserPower *= rnd
	end if
	
	phaserPerKlingon = floor(phaserPower / here.klingons)
	for i in range(klingonShips.len - 1)
		ks = klingonShips[i]
		if ks.shield <= 0 then continue
		distance = mathUtil.distance(ks.sector, ship.position.sector)
		hit = floor((phaserPerKlingon / distance) * (rnd + 2))
		if hit <= 0.15 * ks.shield then
			print "Sensors show no damage to enemy at " + ks.sector.oneBasedStr
		else
			ks.shield -= hit
			print " " + hit + " unit hit on Klingon at sector " + ks.sector.oneBasedStr
			if ks.shield <= 0 then
				if self.destroyKlingon(ks.sector.x, ks.sector.y) then return
			else
				print "   (Sensors show " + round(ks.shield, 6) + " units remaining)"
			end if
		end if			
	end for
	
	self.klingonsFire
end function

Game.torpedos = function
	galaxy = self.galaxy
	ship = galaxy.ship
	here = galaxy.localQuadrant
	klingonShips = here.klingonShips
	
	if ship.torpedos <= 0 then
		print "All photon torpedos expended"
		return
	else if ship.deviceStatus[4] < 0 then
		print "Photon tubes are not operational"
		return
	end if
	
	printCourseChart
	cd = input("Course (1-9)? ").val - 1	// (convert input to 0-8)
	if cd == dirs.len - 1 then cd == 0
	if cd < 0 or cd >= dirs.len then
		print "   Lt. Sulu reports, 'Incorrect course data, sir!'"
		return
	end if
	
	cdi = floor(cd)
	dx = mathUtil.lerp(dirs[cdi][0], dirs[cdi+1][0], cd - cdi)
	dy = mathUtil.lerp(dirs[cdi][1], dirs[cdi+1][1], cd - cdi)
	
	ship.energy -= 2
	ship.torpedos -= 1
	
	x = ship.position.sector.x
	y = ship.position.sector.y
	print "Torpedo track:"
	while true
		x += dx
		y += dy
		roundX = round(x); roundY = round(y)
		if not (0 <= roundX <= 7) or not (0 <= roundY <= 7) then
			print "Torpedo missed"
			self.klingonsFire
			return
		end if
		print "                " + (roundX+1) + " , " + (roundY+1)
		wait 0.5	// (added for dramatic effect)
		entityHit = here.value(roundX, roundY)
		if entityHit != Entity.empty then break
	end while
	
	if entityHit == Entity.klingon then
		if self.destroyKlingon(roundX, roundY) then return
	else if entityHit == Entity.star then
		print "Star at " + (roundX+1) + " , " + (roundY+1) + " absorbed torpedo energy."
	else if entityHit == Entity.starbase then
		print "*** STARBASE DESTROYED ***"
		here.bases -= 1
		here.setValue roundX, roundY, Entity.empty
		galaxy.qtyBases -= 1
		if galaxy.qtyBases == 0 and galaxy.qtyKlingons <= galaxy.daysLeft then
			// (Note: bug in original code, compared qtyKlingons to 
			// quantity daysLeft - missionDuration, which would always be a negative
			// number.  So this hard-labor message could never appear.  I've chosen
			// a different comparison which at least can be true sometimes.)
			print "That does it, captain!! You are hereby relieved of command"
			print "and sentenced to 99 stardates at hard labor on Cygnus 12!!"
			self.endGame false, false, false
			return
		end if
		print "Starfleet Command reviewing your record to consider"
		print "court martial!"
		ship.docked = false
	end if
	
	self.klingonsFire	
end function

Game.endGame = function(won=false, quit=true, shipDestroyed=false)
	if won then
		print "Congratulations, captain! The last klingon battle cruiser"
		print "menacing the federation has been destroyed."
		efficiency = round(1000 * self.galaxy.qtyKlingons / self.galaxy.daysElapsed^2, 4)
		print "Your efficiency rating is " + efficiency
		print
	else
		if not quit then
			if shipDestroyed then
				print
				print "The enterprise has been destroyed. The Federation "
				print "will be conquered."
			end if
			print "It is stardate " + round(self.galaxy.stardate, 1)
		end if

		print "There were " + self.galaxy.qtyKlingons + " klingon battle cruisers left at"
		print "the end of your mission."
		print
	end if
	if self.galaxy.qtyBases == 0 then exit
	print "The Federation is in need of a new starship commander"
	print "for a similar mission -- if there is a volunteer,"
	if input("let him step forward and enter 'aye'? ").lower().trim != "aye" then exit
	self.gameOver = true
end function

notImplemented = function
	print
	print "Not implemented yet."
	print
end function

Game.commands = []	// each entry is [command, summary, function]
Game.addCommand = function(command, summary, func)
	self.commands.push [command, summary, @func]
end function

Game.addCommand "NAV", "to set course", @Game.navigate
Game.addCommand "SRS", "for short range sensor scan", @Game.shortRangeScan
Game.addCommand "LRS", "for long range sensor scan", @Game.longRangeScan
Game.addCommand "PHA", "to fire phasers", @Game.phasers
Game.addCommand "TOR", "to fire photon torpedos", @Game.torpedos
Game.addCommand "SHE", "to raise or lower shields", @Game.shieldControl
Game.addCommand "DAM", "for damage control reports", @Game.damageControl
Game.addCommand "COM", "to call on library-computer", @Game.computer
Game.addCommand "HLP", "for help, i.e. instructions", @Instructions.print	// (not in original game)
Game.addCommand "XXX", "to resign your command", @Game.endGame


Game.doOneCommand = function
	cmd = input("Command? ").upper
	for entry in self.commands
		if cmd == entry[0] then
			self.curCmdFunc = entry[2]
			self.curCmdFunc
			return
		end if
	end for
	print "Enter one of the following:"
	for entry in self.commands
		print "  " + entry[0] + "  (" + entry[1] + ")"
	end for
	print
end function

Game.mainLoop = function
	while not self.gameOver		
		self.doOneCommand
		if self.galaxy.ship.stranded then
			print
			print "** FATAL ERROR **   You've just stranded your ship in space."
			print "You have insufficient maneuvering energy, and shield control"
			print "is presently incapable of cross-circuiting to engine room!!"
		end if
	end while
end function

for i in range(1,12); print; end for
print " "*10 + "*************************************"
print " "*10 + "*                                   *"
print " "*10 + "*                                   *"
print " "*10 + "*      * * SUPER STAR TREK * *      *"
print " "*10 + "*                                   *"
print " "*10 + "*                                   *"
print " "*10 + "*************************************"
for i in range(1,8); print; end for
if getYesNo("Do you need instructions (y/n)") == "yes" then
	Instructions.print
end if

while true
	game = new Game
	game.init
	game.printIntro
	game.mainLoop
end while