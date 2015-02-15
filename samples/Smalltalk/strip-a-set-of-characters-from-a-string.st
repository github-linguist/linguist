| stripChars |
stripChars := [ :string :chars |
	string reject: [ :c | chars includes: c ] ].
stripChars
	value: 'She was a soul stripper. She took my heart!'
	value: 'aei'.

"'Sh ws  soul strppr. Sh took my hrt!'"
