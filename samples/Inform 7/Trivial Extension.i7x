Version 2 of Trivial Extension by Andrew Plotkin begins here.

"This is the rubric of the extension."

"provided for the Linguist package by Andrew Plotkin"

[Note the two special quoted lines above.]

A cow is a kind of animal. A cow can be purple.

Understand "cow" as a cow.
Understand "purple" as a purple cow.

Check pushing a cow:
	instead say "Cow-tipping, at your age?[paragraph break]Inconceivable."

[Here are the possible levels of heading:]

Volume One

Text-line is always "A line of text."

Book 2

	Part the third - indented headings still count

Chapter IV - not for release

[Heading labels are case-insensitive.]

section foobar

[A line beginning "Volume" that does not have blank lines before and after it is *not* a header line. So the following should all be part of section foobar. Sadly, the "Volume is..." line gets colored as a header, because Atom's regexp model can't recognize "thing with blank lines before and after"!]

Measure is a kind of value.
Volume is a measure. Length is a measure.
Area is a measure.

[And now some Inform 6 inclusions.]

To say em -- running on:
	(- style underline; -).
To say /em -- running on:
	(- style roman; -).

Include (-

! Inform 6 comments start with a ! mark and run to the end of the line.
Global cowcount;

[ inform6func arg;
	print "Here is some text; ", (address) 'dictword', ".^";
	cowcount++;  ! increment this variable
];

Object i6cow
	with name 'cow' 'animal',
	with description "It looks like a cow.",
	has animate scenery;

-) after "Global Variables" in "Output.i6t".

Trivial Extension ends here.

---- DOCUMENTATION ----

Everything after the "---- DOCUMENTATION ----" line is documentation, so it should have the comment style.

However, tab-indented lines are sample Inform code within the documentation:

	Horns are a kind of thing. Every cow has horns.
	say "Moo[if the noun is purple] indigo[end if]."

So we need to allow for that.
