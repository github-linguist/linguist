// This is a single line comment.
/*
	This is a multi-line comment
*/

// Pre-processor keywords

#define PI 3.1415

#if PI == 4

#define G 5

#elif PI == 3

#define I 6

#else

#define K 7

#endif


var/GlobalCounter = 0
var/const/CONST_VARIABLE = 2
var/list/MyList = list("anything", 1, new /datum/entity)
var/list/EmptyList[99] // creates a list of 99 null entries
var/list/NullList = null

/*
	Entity Class
*/

/datum/entity
	var/name = "Entity"
	var/number = 0

/datum/entity/proc/myFunction()
	world.log << "Entity has called myFunction"

/datum/entity/New()
	number = GlobalCounter++

/*
	Unit Class, Extends from Entity
*/

/datum/entity/unit
	name = "Unit"

/datum/entity/unit/New()
	..() // calls the parent's proc; equal to super() and base() in other languages
	number = rand(1, 99)

/datum/entity/unit/myFunction()
	world.log << "Unit has overriden and called myFunction"

// Global Function
/proc/ReverseList(var/list/input)
	var/list/output = list()
	for(var/i = input.len; i >= 1; i--) // IMPORTANT: List Arrays count from 1.
		output += input[i] // "+= x" is ".Add(x)"
	return output

// Bitflags
/proc/DoStuff()
	var/bitflag = 0
	bitflag |= 8
	return bitflag

/proc/DoOtherStuff()
	var/bitflag = 65535 // 16 bits is the maximum amount
	bitflag &= ~8
	return bitflag

// Logic
/proc/DoNothing()
	var/pi = PI
	if(pi == 4)
		world.log << "PI is 4"
	else if(pi == CONST_VARIABLE)
		world.log << "PI is [CONST_VARIABLE]!"
	else
		world.log << "PI is approximety [pi]"

#undef PI // Undefine PI