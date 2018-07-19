| varName |
varName := FillInTheBlankMorph
	request: 'Enter a variable name'.
Compiler
	evaluate: '| ', varName, ' | ', varName, ' := 42.
	Transcript
		show: ''value of ', varName, ''';
		show: '' is '';
		show: ', varName.
