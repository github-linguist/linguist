"Test Case" by Andrew Plotkin.

Include Trivial Extension by Andrew Plotkin.

Volume 1 - overview

Chapter - setting the scene

The Kitchen is a room.

[Comment: this kitchen is modelled after the one in Zork, although it lacks the detail to establish this to the player.]

Section - the kitchen table

The spicerack is a container in the Kitchen.

Table of Spices
Name	Flavor
"cinnamon"	5
"nutmeg"	4
"szechuan pepper"	8

The description of the spicerack is "It's mostly empty."

Chapter - a character

A purple cow called Gelett is in the Kitchen.

[This comment spans multiple lines..

...and this line contains [nested square[] brackets]...

...which is legal in Inform 7.]

Instead of examining Gelett:
	say "You'd rather see than be one."

Instead of examining Gelett:
	say "You'd rather see than be one."

Check smelling Gelett:
	say "This text contains several lines.

A blank line is displayed as a paragraph break,
but a simple line break is not.";
	stop the action.

Section - cow catching

Gelett has a number called the mooness.

Instead of taking Gelett:
	increment the mooness of Gelett;
	if the mooness of Gelett is one:
		say "Gelett moos once.";
	else:
		say "Gelett moos [mooness of Gelett in words] times.";

Volume 2 - the turn cycle

Every turn:
	say "A turn passes[one of][or] placidly[or] idly[or] tediously[at random]."
