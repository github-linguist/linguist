// Source: https://github.com/JoeStrout/miniscript/blob/master/MiniScript-cpp/demo/therapist.ms
// Therapist
// a cheezy little Eliza knock-off by Joe Strout
// with some updates by Jeff Epler
// hacked into a module and updated by Jez Higgins
// and finally translated from Python into MiniScript by Joe Strout :)

import "vt"

//----------------------------------------------------------------------
//  respond: take a string, a set of regexps, and a corresponding
//    set of response lists; find a match, and return a randomly
//    chosen response from the corresponding list.
//----------------------------------------------------------------------
respond = function(s)
	// find a match among our patterns
	for kv in gPats
		match = patMatch(kv[0], s)
		if match == null then continue
		// Found a match... stuff with corresponding value
		// chosen randomly from among the available options
		resp = kv[1][rnd * kv[1].len]
		// now we have a response; stuff in reflected text where indicated
		for i in match.indexes
			resp = resp.replace("%"+(i+1), translate(match[i], gReflections))
		end for
		resp = resp[0].upper + resp[1:]
		return resp
	end for
end function

//----------------------------------------------------------------------
// translate: take a string, replace any words found in dict.keys()
//  with the corresponding dict.values()
//----------------------------------------------------------------------
translate = function(s, dict)
	words = s.split
	for i in words.indexes
		w = words[i].lower
		if dict.hasIndex(w) then words[i] = dict[w]
	end for
	return words.join
end function


//----------------------------------------------------------------------
// patmatch: Match a given pattern like "Hello *!" against a string
//	 like "Hello world", and return null if no match, or a list of
//	 the text that fits into the wildcards, like ["world"].
//----------------------------------------------------------------------
patMatch = function(pat, s, wildcard="(.*)")
	result = []
	pos = 0
	literals = pat.split(wildcard)
	for i in literals.indexes
		lit = literals[i]
		if s[pos:pos+lit.len] != lit then return null
		if i == literals.len-1 then return result
		pos = pos + lit.len
		if i == literals.len-2 and literals[-1] == "" then
			// special case: match to end of line
			result.push s[pos:]
			return result
		end if
		nextpos = s.indexOf(literals[i+1], pos)
		if nextpos == null then return null
		result.push s[pos:nextpos]
		pos = nextpos
	end for
	return result
end function

//----------------------------------------------------------------------
// gReflections, a translation table used to convert things you say
//    into things the computer says back, e.g. "I am" --> "you are"
//----------------------------------------------------------------------
gReflections = {
"am"    : "are",
"was"   : "were",
"i"     : "you",
"i'd"   : "you would",
"i've"  : "you have",
"i'll"  : "you will",
"i'm"	: "you're",
"my"    : "your",
"are"   : "am",
"you've": "I have",
"you'll": "I will",
"your"  : "my",
"you're": "I'm",
"yours" : "mine",
"you"   : "me",
"me"    : "you" }

//----------------------------------------------------------------------
// gPats, the main response table.  Each element of the list is a
//  two-element list; the first is an input pattern, and the second is a
//  list of possible responses, with group-macros labelled as
//  %1, %2, etc.
//----------------------------------------------------------------------
gPats = [
  ["I need (.*)",
  [ "Why do you need %1?",
    "Would it really help you to get %1?",
    "Are you sure you need %1?"]],

  ["Why don't you (.*)?",
  [ "Do you really think I don't %1?",
    "Perhaps eventually I will %1.",
    "Do you really want me to %1?"]],

  ["Why can't I (.*)?",
  [ "Do you think you should be able to %1?",
    "If you could %1, what would you do?",
    "I don't know -- why can't you %1?",
    "Have you really tried?"]],

  ["I can't (.*)",
  [ "How do you know you can't %1?",
    "Perhaps you could %1 if you tried.",
    "What would it take for you to %1?",
    "Not with THAT attitude.",
    "Are you sure you can't %1?",
    "What would you do if you could %1?",
    "Do you think others could %1?",
    "Have you tried to %1?"]],

  ["I am (.*)",
  [ "Did you come to me because you are %1?",
    "How long have you been %1?",
    "How do you feel about being %1?"]],

  ["I'm (.*)",
  [ "How does being %1 make you feel?",
    "Do you enjoy being %1?",
    "Why do you tell me you're %1?",
    "Why do you think you're %1?"]],

  ["Are you (.*)?",
  [ "Why does it matter whether I am %1?",
    "Would you prefer it if I were not %1?",
    "Perhaps you believe I am %1.",
    "I may be %1 -- what do you think?"]],

  ["What (.*)",
  [ "Why do you ask?",
    "How would an answer to that help you?",
    "What do you think?"]],

  ["How (.*)",
  [ "How do you suppose?",
    "Perhaps you can answer your own question.",
    "What is it you're really asking?"]],

  ["Because (.*)",
  [ "Is that the real reason?",
    "What other reasons come to mind?",
    "Does that reason apply to anything else?",
    "If %1, what else must be true?"]],

  ["(.*) sorry (.*)",
  [ "There are many times when no apology is needed.",
    "What feelings do you have when you apologize?"]],

  ["Hello(.*)",
  [ "Hello... I'm glad you could drop by today.",
    "Hi there... how are you today?",
    "Hello, how are you feeling today?"]],

  ["I think (.*)",
  [ "Do you doubt %1?",
    "Do you really think so?",
    "But you're not sure %1?"]],

  ["(.*) friend (.*)",
  [ "Tell me more about your friends.",
    "When you think of a friend, what comes to mind?",
    "Why don't you tell me about a childhood friend?"]],

  ["Yes",
  [ "You seem quite sure.",
    "OK, but can you elaborate a bit?"]],

  ["(.*) computer(.*)",
  [ "Are you really talking about me?",
    "Does it seem strange to talk to a computer?",
    "How do computers make you feel?",
    "Do you feel threatened by computers?"]],

  ["Is it (.*)",
  [ "Do you think it is %1?",
    "Perhaps it's %1 -- what do you think?",
    "If it were %1, what would you do?",
    "It could well be that %1."]],

  ["It is (.*)",
  [ "You seem very certain.",
    "If I told you that it probably isn't %1, what would you feel?"]],

  ["Can you (.*)?",
  [ "What makes you think I can't %1?",
    "If I could %1, then what?",
    "Why do you ask if I can %1?"]],

  ["Can I (.*)?",
  [ "Perhaps you don't want to %1.",
    "Do you want to be able to %1?",
    "If you could %1, would you?"]],

  ["You are (.*)",
  [ "Why do you think I am %1?",
    "Does it please you to think that I'm %1?",
    "Perhaps you would like me to be %1.",
    "Perhaps you're really talking about yourself?"]],

  ["You're (.*)",
  [ "Why do you say I am %1?",
    "Why do you think I am %1?",
    "Are we talking about you, or me?"]],

  ["I don't (.*)",
  [ "Don't you really %1?",
    "Why don't you %1?",
    "Do you want to %1?"]],

  ["I feel (.*)",
  [ "Good, tell me more about these feelings.",
    "Do you often feel %1?",
    "When do you usually feel %1?",
    "When you feel %1, what do you do?"]],

  ["I have (.*)",
  [ "Why do you tell me that you've %1?",
    "Have you really %1?",
    "Now that you have %1, what will you do next?"]],

  ["I would (.*)",
  [ "Could you explain why you would %1?",
    "Why would you %1?",
    "Who else knows that you would %1?"]],

  ["Is there (.*)",
  [ "Do you think there is %1?",
    "It's likely that there is %1.",
    "Would you like there to be %1?"]],

  ["My (.*)",
  [ "I see, your %1.",
    "Why do you say that your %1?",
    "When your %1, how do you feel?"]],

  ["You (.*)",
  [ "We should be discussing you, not me.",
    "Why do you say that about me?",
    "Why do you care whether I %1?"]],

  ["Why (.*)",
  [ "Why don't you tell me the reason why %1?",
    "Why do you think %1?" ]],

  ["I want (.*)",
  [ "What would it mean to you if you got %1?",
    "Why do you want %1?",
    "What would you do if you got %1?",
    "If you got %1, then what would you do?"]],

  ["(.*) mother(.*)",
  [ "Tell me more about your mother.",
    "What was your relationship with your mother like?",
    "How do you feel about your mother?",
    "How does this relate to your feelings today?",
    "Good family relations are important."]],

  ["(.*) father(.*)",
  [ "Tell me more about your father.",
    "How did your father make you feel?",
    "How do you feel about your father?",
    "Does your relationship with your father relate to your feelings today?",
    "Do you have trouble showing affection with your family?"]],

  ["(.*) child(.*)",
  [ "Did you have close friends as a child?",
    "What is your favorite childhood memory?",
    "Do you remember any dreams or nightmares from childhood?",
    "Did the other children sometimes tease you?",
    "How do you think your childhood experiences relate to your feelings today?"]],

  ["(.*)?",
  [ "Why do you ask that?",
    "Please consider whether you can answer your own question.",
    "Perhaps the answer lies within yourself?",
    "Why don't you tell me?"]],

  ["quit",
  [ "Thank you for talking with me.",
    "Good-bye.",
    "Thank you, that will be $150.  Have a good day!"]],

  ["(.*)",
  [ "Please tell me more.",
    "Let's change focus a bit... Tell me about your family.",
    "Can you elaborate on that?",
    "Why do you say that %1?",
    "I see.",
    "Very interesting.",
    "%1.",
    "I see.  And what does that tell you?",
    "How does that make you feel?",
    "How do you feel when you say that?"]]  ]


runInteractive = function
	print vt.clear + vt.reset + vt.normal
	print "Therapist" + char(13) + "---------"
	print "Talk to the program by typing in plain English, using normal upper-"
	print "and lower-case letters and punctuation.  Enter ""quit"" when done."
	print "=" * 68
	print "Hello.  How are you feeling today?"
	
	while true
		print vt.textColor(vt.color.gray), ""
		s = input("> ")
		while s and (s[-1] == "!" or s[-1] == ".")
			s = s[:-1]
		end while
		print vt.normal
		print respond(s)
		if s.lower == "quit" then break
	end while
end function

if locals == globals then
	runInteractive
end if
