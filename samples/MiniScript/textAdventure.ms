// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/textAdventure.ms
// The Greedy Gargoyle
//
// a miniature text adventure

import "vt"
color = vt.color

// We don't yet have any way to get the terminal size, so for now assume:
termRows = 24
termCols = 80

printWrap = function(s, width=null, maxLines=null)
	left = 0
	if width == null then width = termCols - 1 - left
	lineCount = 0
	while s
		if s.len <= width then
			print s
			return null
		end if
		foundCut = false
		for i in range(width, 0)
			if s[i] == " " then
				print s[:i]
				s = s[i+1:]
				foundCut = true
				break
			end if
		end for
		if not foundCut then
			// Couldn't find a space to cut on...
			// so, out of desperation, just cut at width
			print s[:width]
			s = s[width:]
		end if
		lineCount = lineCount + 1
		if maxLines and lineCount == maxLines then return s
	end while
end function

it = ""

// Define some utility functions.
printIfNonEmpty = function(s)
	if s > null then printWrap s
end function

removeValue = function(lst, val)
	idx = lst.indexOf(val)
	if idx != null then
		lst.remove idx
	else
		print "Couldn't find " + val + " in " + lst
		print stackTrace
	end if 
end function

aOrAn = function(s)
	if "aeiou".indexOf(s[0]) != null then return "an " + s
	return "a " + s
end function

// Match the start of a command, returning the rest as a string.
match = function(start, cmd)
	if start isa list then
		for s in start
			if s[-1] != " " then s = s + " "
			if cmd[:s.len] == s then return cmd[s.len:]
		end for
	else
		if start[-1] != " " then start = start + " "
		if cmd[:start.len] == start then return cmd[start.len:]
	end if
	return null
end function

// Match a command involving a preposition.  Return both the part
// before the preposition (but after the verb), and the part
// after the preposition, as a 2-element list.
matchPrep = function(verb, preposition, cmd)
	prepPos = cmd.indexOf(" " + preposition + " ")
	if prepPos == null then return null
	prepObj = cmd[prepPos + preposition.len + 2:]
	cmd = cmd[:prepPos]
	obj = match(verb, cmd)
	if not obj then return null
	return [obj, prepObj]
end function

//----------------------------------------------------------------------
// Set up the base class hierarchy.
//----------------------------------------------------------------------
Object = {}
Object.location = null
Object.contents = []
Object.name = "Object"
Object.altNames = []
Object.desc = "You see a nondescript object."
Object.salient = true

Object.isCalled = function(s)
	if self.name == s then return true
	if self.altNames.indexOf(s) != null then return true
	return false
end function

Object.fullName = function
	return self.name
end function

Object.matches = function(verb, cmd)
	objStr = match(verb, cmd)
	return objStr != null and self.isCalled(objStr)
end function

Object.splitItem = function(s)
	// This is a special function used with countable things.
	// It gives them a chance to split into a separate item
	// with a different count.
	return null
end function

Object.contentNamed = function(s)
	for item in self.contents
		part = item.splitItem(s)
		if part != null then return part
		if item.isCalled(s) then return item
	end for
	return null
end function

Object.descInRoom = function
	if rnd > 0.5 then
		print "You see " + self.fullName + " lying here."
	else
		print "There is " + self.fullName + " here."
	end if
end function

Object.describe = function
	printWrap self.desc
	self.listContents("The " + self.name + " contains:")
end function

Object.moveTo = function(location)
	oldLoc = self.location
	self.location = location
	if oldLoc != null then removeValue oldLoc.contents, self
	self.location = location
	if location == null then return
	// Note how we don't use location.contents.push here, because we don't
	// want to accidentally share lists among multiple objects.
	location.contents = location.contents + [self]
end function

Object.listContents = function(headerMsg, emptyMsg)
	if not self.contents then
		printIfNonEmpty emptyMsg
	else
		for item in self.contents
			if not item.salient then continue
			if headerMsg then
				printWrap headerMsg
				headerMsg = ""
			end if			
			printWrap "   - " + item.fullName
		end for
	end if
end function

Object.handleCommand = function(cmd, pass)
	// We're given two opportunities to handle the command from the user.
	// When pass=1, handle it only if it is clearly meant for this object.
	// When pass=2, everyone else has already passed on it once, so be
	// more aggressive about handling it or reporting an error.
	// Return true if handled, false to pass.
	if cmd == "quit" then
		print "Quitting game."
		globals.done = true
		return true
	end if
	obj = match("say", cmd)
	if obj != null then
		printWrap "You say, """ + obj + """"
		return true
	end if
	if self.matches(["look at", "look", "examine", "l "], cmd) then
		self.describe
		globals.it = self.name
		return true
	end if
	if pass == 2 then
		if ["north","south","east","west","up","down"].indexOf(cmd) != null then
			print "You can see no way to go " + cmd + "."
			return true
		end if
		for item in self.contents
			if item.handleCommand(cmd, 1) then return true
		end for
	end if
end function

Object.canGet = function
	print "Don't be ridiculous."
	return false
end function

Object.contain = function(obj)
	print "The " + self.name + " can't contain things."
end function

//----------------------------------------------------------------------
Room = new Object
Room.name = "Room"
Room.desc = "You are in a nondescript room."
Room.darkDesc = function
	return self.desc
end function
Room.visited = false

Room.handleCommand = function(cmd, pass)
	if cmd == "look" or cmd == "l" then
		look true
		return true
	end if
end function

//----------------------------------------------------------------------
Exit = new Object
Exit.name = "Exit"		// typically, direction
Exit.type = "door"		// passage, exit, door, archway, etc.
Exit.desc = "It looks like an ordinary exit."
Exit.open = true
Exit.dest = null	// destination: Room this exit leads to

Exit.fullName = function
	return self.type  + " to the " + self.name
end function

Exit.descInRoom = function
	if self.open then
		return "There is an open " + self.fullName + "."
	else
		return "The " + self.fullName + " is closed."
	end if
end function

Exit.handleCommand = function(cmd, pass)
	if cmd == self.name or self.altNames.indexOf(cmd) != null then
		self.invoke
		return true
	end if
	if self.matches(["go", "exit", "head", "leave"], cmd) then
		self.invoke
		return true
	end if
	if self.matches("open", cmd) then
		self.openDoor
		return true
	end if
	if self.matches(["close", "shut"], cmd) then
		self.closeDoor
		return true
	end if
	return super.handleCommand(cmd, pass)
end function

Exit.openDoor = function
	if self.open then
		print "The " + self.fullName + " is already open."
	else
		self.open = true
		print "You open the " + self.fullName + "."
	end if
end function
Exit.closeDoor = function
	if not self.open then
		print "The " + self.fullName + " is already closed."
	else
		self.open = false
		print "You close the " + self.fullName + "."
	end if
end function

Exit.successMsg = ""

Exit.failureMsg = function
	return "The " + self.fullName + " is closed."
end function

Exit.invoke = function
	if self.open and self.dest != null then
		printIfNonEmpty self.successMsg
		Player.moveTo self.dest
		look
	else
		printIfNonEmpty self.failureMsg
	end if
end function

Exit.connect = function(room1, name1, room2, name2)
	exit1 = new self
	exit1.moveTo room1
	exit1.dest = room2
	exit1.name = name1
	
	exit2 = new self
	exit2.moveTo room2
	exit2.dest = room1
	if name2 == null then
		if name1 == "north" then name2 = "south"
		if name1 == "south" then name2 = "north"
		if name1 == "east" then name2 = "west"
		if name1 == "west" then name2 = "east"
	end if
	if name2 then exit2.name = name2 else exit2.name = name1
	
	return [exit1, exit2]
end function

//----------------------------------------------------------------------
Thing = new Object
Thing.name = "Thing"
Thing.desc = "You see a nondescript thing."
Thing.a = function
	if "aeiou".indexOf(self.name[0]) != null then return "an "
	return "a"
end function
Thing.fullName = function
	return self.a + " " + self.name
end function
Thing.size = 1
Thing.canGet = function
	if self.size > 10 then
		printWrap "The "+ self.name + " is much too large to pick up."
		return false
	end if
	return true
end function

//----------------------------------------------------------------------
CountableThing = new Thing
CountableThing.name = "Countable"

CountableThing.pluralName = function
	return self.name + "s"
end function

CountableThing.count = 1

CountableThing.fullName = function
	if self.count == 1 then return aOrAn(self.name)
	return self.count + " " + self.pluralName
end function

CountableThing.singleDesc = "You see #FULLNAME#."
CountableThing.pluralDesc = "You see #FULLNAME#."

CountableThing.desc = function
	if self.count == 1 then return self.singleDesc.replace("#FULLNAME#", self.fullName)
	return self.pluralDesc.replace("#FULLNAME#", self.fullName)
end function

CountableThing.descInRoom = function
	return self.desc - "." + " here."
end function

CountableThing.isCalled = function(s)
	if super.isCalled(s) then return true
	if self.count > 1 and super.isCalled(s - "s") then return true
	return false
end function

numberWordToVal = {"a":1, "one":1, "two":2, "three":3}
CountableThing.splitItem = function(s)
	count = null
	words = s.split
	if words.len == 1 then return null
	rest = words[1:].join
	if numberWordToVal.hasIndex(words[0]) then
		count = numberWordToVal[words[0]]
	else if val(words[0]) > 0 then
		count = val(words[0])
	else if words[0] == "all" and self.isCalled(rest) then
		return self
	end if
	if not count or count > self.count then return null
	if count == self.count then return self
	// OK, we need to split this item by the given count.
	noob = self + {}   // (makes a shallow copy of self)
	noob.count = count
	self.count -= count
	noob.location.contents.push noob
	return noob
end function

CountableThing.moveTo = function(location)
	oldLoc = self.location
	if oldLoc != null then
		self.location = null
		removeValue oldLoc.contents, self
	end if
	self.location = null
	if location == null then return
	// combine with an existing item of the same type, if possible
	for item in location.contents
		if item isa CountableThing and item.name == self.name then
			item.count += self.count
			return
		end if
	end for
	// ...failing that, just move in the standard way
	super.moveTo location
end function
//----------------------------------------------------------------------
Liquid = new Thing
Liquid.name = "Liquid"
Liquid.desc = "You see some nondescript liquid."
Liquid.a = "some"
Liquid.canGet = function
	printWrap "You try to pick up " + self.fullName + ", but it just falls through your fingers."
	return false
end function

//----------------------------------------------------------------------
Container = new Thing
Container.handleCommand = function(cmd, pass)
	if self.matches(["look in", "look inside"], cmd) then
		if self.contents then
			print "It looks like the " + self.name + " contains:"
			for item in self.contents
				printWrap "   - " + item.fullName
			end for
		else
			print "The " + self.name + " is empty."
		end if
		globals.it = self.name
		return true
	end if
	return super.handleCommand(cmd, pass)
end function
Container.contain = function(obj)
	obj.moveTo self
	printWrap "You put " + obj.fullName + " into " + self.fullName + "."
end function

//----------------------------------------------------------------------
Player = new Thing
Player.name = "yourself"
Player.altNames = ["me", "myself"]
Player.desc = "You see yourself."
Player.descInRoom = null

Player.findObj = function(objStr, inventoryFirst=false)
	if objStr == null then return null
	
	if inventoryFirst then
		// Check the player's inventory, including one level deeper
		item = Player.contentNamed(objStr)
		if item != null then return item
		for item in Player.contents
			subitem = item.contentNamed(objStr)
			if subitem != null then return subitem
		end for
	end if
			
	// Check the room
	item = Player.location.contentNamed(objStr)
	if item != null then return item
	
	// Check the player's inventory (if we haven't already)
	if not inventoryFirst then
		item = Player.contentNamed(objStr)
		if item != null then return item
	end if
	
	// Check things contained one level deeper
	for item in Player.location.contents + Player.contents * (not inventoryFirst)
		subitem = item.contentNamed(objStr)
		if subitem != null then return subitem
	end for
	return null
end function

Player.findObjs = function(objects, inventoryFirst=false)
	if objects == null then return null
	result = [self.findObj(objects[0], inventoryFirst), self.findObj(objects[1], inventoryFirst)]
	if not result[0] or not result[1] then return null
	return result
end function

Player.handleCommand = function(cmd, pass)
	if cmd == "inventory" or cmd == "inv" or cmd == "i" then
		self.listContents "You are carrying:", "You are empty-handed."
		return true
	end if
	
	objStrs = matchPrep(["get", "pick up", "take"], "from", cmd)
	if objStrs then
		objs = [null, self.findObj(objStrs[1])]
		if not objs[1] then
			print "You don't see any " + objStrs[1] + " here."
			return true
		end if
		objs[0] = objs[1].contentNamed(objStrs[0])
		if not objs[0] then
			print "The " + objs[1].name + " doesn't seem to contain any " + objStrs[0] + "."
			return true
		end if
		globals.it = objs[0].name
		return self.get(objs[0])
	end if
	
	obj = self.findObj(match(["get", "pick up", "take"], cmd))
	if obj then
		globals.it = obj.name
		return self.get(obj)
	end if
	
	objs = self.findObjs(matchPrep("put", "in", cmd), true)
	if not objs then objs = self.findObjs(matchPrep("put", "into", cmd), true)
	if not objs then objs = self.findObjs(matchPrep("give", "to", cmd), true)
	if objs then
		globals.it = objs[0].name
		return self.put(objs[0], objs[1])
	end if
	
	obj = self.findObj(match(["drop", "put down", "toss"], cmd), true)
	if obj then
		globals.it = obj.name
		return self.drop(obj)
	end if
	
	return super.handleCommand(cmd, pass)
end function

Player.get = function(obj)
	if not obj.canGet then return true
	if obj.location == Player then
		print "You're already holding the " + obj.name + "."
		return true
	end if
	obj.moveTo Player
	print "You get " + obj.fullName + "."
	return true
end function

Player.drop = function(obj)
	obj.moveTo Player.location
	print "You drop " + obj.fullName + "."
	return true
end function

Player.put = function(obj, container)
	if obj.location == container then
		print "The " + obj.name + " is already in " + container.fullName + "."
		return true
	end if
	if obj.location != self and not obj.canGet then return true
	container.contain obj
	return true
end function

Player.canGet = function
	print "You can't get yourself.  Except in a metaphorical sense."
	return false
end function

//----------------------------------------------------------------------
// Command helper functions.

look = function(verbose=false)
	room = Player.location
	print vt.textColor(color.yellow) + room.name + vt.textColor(normalColor)
	if verbose or not room.visited then
		printWrap room.desc * lamp.lit + room.darkDesc * (not lamp.lit)
	end if
	for item in room.contents
		if not item.salient then continue
		printIfNonEmpty item.descInRoom
	end for
	room.visited = true
end function

//----------------------------------------------------------------------
// Global command handler.
handleCommand = function(cmd)
	cmd = cmd + " "
	itPos = cmd.indexOf(" it ")
	if itPos != null then
		if not it then
			print "I don't know what you mean by ""it""."
			return
		end if
		cmd = cmd.replace(" it ", " " + it + " ")
	end if
	cmd = cmd[:-1]
	
	room = Player.location
	for pass in range(1,2)
		// try all the objects in the room (except the player)
		for item in room.contents
			if item == Player then continue
			if item.handleCommand(cmd, pass) then return
		end for
		// try objects in the inventory
		for item in Player.contents
			if item.handleCommand(cmd, pass) then return
		end for
		// try the room itself
		if room.handleCommand(cmd, pass) then return
		// try the player
		if Player.handleCommand(cmd, pass) then return
	end for
	// If we get here, nothing could handle the command.
	obj = match(["look", "l", "examine", "get", "take", "pick up"], cmd)
	if obj then
		print "You don't see any " + obj + " here."
		return
	end if
	objs = matchPrep("put", "in", cmd)
	if not objs then objs = matchPrep("put", "into", cmd)
	if not objs then objs = matchPrep("give", "to", cmd)
	if objs and not Player.findObj(objs[0]) then
		print "I don't know what " + objs[0] + " you mean."
	else if objs and not Player.findObj(objs[1]) then
		print "I don't know what " + objs[1] + " you mean."
	else
		print "Unknown command."
	end if
end function

//----------------------------------------------------------------------
// Main program.
if locals == globals then
	
	normalColor = color.green
	print vt.clear + vt.reset + vt.normal + vt.textColor(normalColor)
	
	// Create the world.
	fountainRoom = new Room
	fountainRoom.name = "Fountain Room"
	fountainRoom.desc = "You're in a small room with walls of stone." +
	"  In the middle of the room is a raised stone pool filled with" +
	" water.  To the north is a great stone door, into which a" +
	" gargoyle has been carved with amazingly lifelike detail."
	fountainRoom.darkDesc = "You're in a very dark room.  The only" +
	" light comes from the glowing gargoyle carved into the great" +
	" stone door to the north."
	Player.moveTo fountainRoom
	
	endRoom = new Room
	endRoom.name = "Congratulations!"
	endRoom.desc = "You've gotten past the greedy gargoyle -- and"+
	" it only cost you three fake coins!  Nicely done.  You may" +
	" examine the credits, or quit."
	
	doors = Exit.connect(fountainRoom, "north", endRoom)
	mainExit = doors[0]
	mainExit.altNames = ["exit", "leave", "out", "door", "n"]
	mainExit.desc = "It's a great stone door, with a carving of a" +
	" gargoyle that appears amazingly lifelike."
	doors[1].altNames = ["s"]
	mainExit.open = false
	mainExit.salient = false
	mainExit.openDoor = function
		print "The stone door won't budge."
	end function
	
	pool = new Container
	pool.name = "pool"
	pool.altNames = ["fountain", "fountian"]
	pool.desc =  "The pool is composed of circular stone walls about a"+
	" foot high and five feet across.  The floor of the pool is also" +
	" depressed several inches below the floor of the room.  It's" +
	" filled with a clear liquid that appears to be water."
	pool.size = 20
	pool.salient = false
	pool.moveTo fountainRoom
	
	water = new Liquid
	water.name = "water"
	water.altNames = ["liquid", "fluid"]
	water.desc = "It appears to be ordinary water, and fairly clean."
	water.moveTo pool
	
	garg = new Thing
	garg.name = "gargoyle"
	garg.altNames = ["statue"]
	garg.altNames = ["garg", "gar", "statue", "carving", "hand"]
	garg.desc = "The gargoyle is carved into the door with amazing" +
	" three-dimensional relief.  Its face wears an exaggerated" +
	" expression of boredom.  Its hand juts out from the door, palm up."
	garg.moveTo fountainRoom
	garg.salient = false
	garg.size = 20
	garg.note = "The gargoyle on the door glows faintly in the dark," +
	" as magical items frequently do."
	garg.door = doors[0]
	
	lamp = new Thing
	lamp.name = "lamp"
	lamp.altNames = ["lantern"]
	lamp.lit = true
	lamp.fullName = function
		result = self.a + " " + self.name + " "
		if self.lit then result = result + "(lit)" else result = result + "(not lit)"
		return result
	end function
	lamp.desc = function
		if self.lit then return "Your trusty oil lamp is burning brightly."
		return "Your trusty oil lamp is currently dark, much like the room itself."
	end function
	lamp.handleCommand = function(cmd, pass)
		if super.handleCommand(cmd, pass) then return true
		cmd = cmd.replace("lantern", "lamp")
		if cmd == "douse lamp" or cmd == "extinguish lamp" or 
				cmd == "turn lamp off" or cmd == "turn off lamp" or
				cmd == "put out lamp" then
			if not self.lit then
				print "The lamp is already off."
			else
				self.lit = false
				printWrap "You extinguish the lamp.  The room goes" +
				" dark, except for a faint glow from the gargoyle statue."
			end if
			return true
		else if cmd == "light lamp" or cmd == "turn lamp on" or
		cmd == "turn on lamp" then
			if self.lit then
				print "The lamp is already lit."
			else if self.location == pool then
				print "You can't get the lamp lit while it's in the pool."
			else
				self.lit = true
				printWrap "You light the lamp."
			end if
			return true
		end if
	end function	
	lamp.moveTo Player
	
	goldCoin = new CountableThing
	goldCoin.name = "gold coin"
	goldCoin.altNames = ["coin", "real coin"]
	goldCoin.moveTo Player
	
	tinCoin = new CountableThing
	tinCoin.name = "tin coin"
	tinCoin.altNames = ["coin", "fake"]
	tinCoin.singleDesc = "This is a fake coin made of worthless tin."
	tinCoin.pluralDesc = "These are fake coins made of worthless tin."
	tinCoin.descInRoom = function
		return "You see " + self.fullName + " lying here."
	end function
	tinCoin.count = 3
	tinCoin.moveTo pool
	
	credits = new Thing
	credits.name = "credits"
	credits.altNames = ["cred"]
	credits.desc = "The Greedy Gargoyle was originally written on May"+
	" 11, 1996 by Joe Strout, using the Python Universe Builder (a"+
	" little Python library for making text adventures).  It was" +
	" ported to MiniScript as a Mini Micro demo in summer 2019."
	credits.a = "the"
	credits.moveTo endRoom
	credits.salient = false
	
	//----------------------------------------------------------------------
	// Special code for the gargoyle.
	garg.contain = function(obj)
		if obj == goldCoin then
			printWrap "As you place the coin in the gargoyle's palm, "+
	" its face comes to life.  It cocks one bulging eye at the coin for"+
	" a moment, then nods approvingly.  With the help of a clawed" +
	" finger, it begins to count -- but, finding only one coin in its"+
	" palm, it stops abruptly and throws the coin down in disgust."
			obj.moveTo self.location
		else if obj == tinCoin then
			if lamp.lit then
				printWrap "As you place the false coinage in the gargoyle's" +
	" hand, its face springs to life.  It cocks a bulging eye at your"+
	" offering.  Noting the dull grey color and featureless surfaces,"+
	" its eyes grow wide.  With an angry flick of its arm, it hurls"+
	" the " + obj.fullName + " over your head."
				obj.moveTo self.location
			else
				if obj.count > 1 then
					shapes = "shapes"
					their = "their"
					coins = "coins"
					them = "them"
				else
					shapes = "shape"
					their = "its"
					coins = "coin"
					them = "it"
				end if
				printWrap "As you place "+obj.fullName+" into the"+
	" gargoyle's glowing hand, its face comes to life once again.  It"+
	" cocks an eye at the dark round "+shapes+" in its hand.  Frowning,"+
	" it turns "+them+" over and appears to be judging "+their+" weight." +
	"  With a shrug, it lifts a clawed finger and begins counting."
				if obj.count < 3 then
					printWrap "Finding only " + obj.count + " " +
	coins+", it stops abruptly and throws "+them+" down in disgust."
					obj.moveTo self.location
				else
					printWrap "It stabs its finger at the coins three"+
	" times, and compares this to the three clawed toes on its foot." +
	"  Apparently satisfied, the gargoyle opens its mouth and tosses" +
	" the coins down."
					obj.moveTo null
					print; wait 2
					printWrap "Grinning, the gargoyle splits in half,"+
	" right down the middle.  Its two halves separate and slide" +
	" apart, taking the two halves of the great stone door with them."+
	"  A dark passageway extends beyond the door to the north."
					mainExit.open = true
					mainExit.salient = true
				end if
			end if
		else
			printWrap "As you place " + obj.fullName + " in the" +
	" gargoyle's hand, its face comes to life.  It glances briefly" +
	" at your offering, but then without hesitation, drops it onto" +
	" the floor."
			obj.moveTo self.location
		end if
	end function
	
	print "                         THE GREEDY GARGOYLE"
	print "                        A 5-Minute Text Puzzle"
	print
	printWrap "Holding your lantern high, you cautiously explore the" +
	" stone panel before you.  According to the maps, the entrance to"+
	" the tomb should be right at the end of this passageway -- but" +
	" you find only smooth stone."
	print
	printWrap "Just as you're about to give up and go home, your" +
	" fingers catch on a small indentation on the adjoining wall. " +
	" When you press the knob inside, the panel spins around," +
	" carrying you with it."
	print
	printWrap "You find yourself in a small room.  The panel through " +
	"which you entered joins perfectly with the wall on this side," +
	" leaving no hope of returning the way you came.  You'd been" +
	" studying for years to find a way into the tomb -- now, if you" +
	" can just find a way out..."
	print char(13)*3
	input "[Press Return.]"
	print char(13)*3
	
	// Main loop.
	done = false
	look
	while not done
		print vt.textColor(color.gray), ""
		cmd = input(">").lower
		print vt.textColor(normalColor), ""
		handleCommand cmd
	end while
	
	print vt.normal
end if
