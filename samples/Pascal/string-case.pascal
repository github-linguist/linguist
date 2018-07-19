// Uppercase and Lowercase functions for a minimal standard Pascal
// where no library routines for these operations exist
PROGRAM upperlower;

// convert a character to uppercase
FUNCTION uch(ch: CHAR): CHAR;
	BEGIN
		uch := ch;
		IF ch IN ['a'..'z'] THEN
			uch := chr(ord(ch) AND $5F);
	END;
	
// convert a character to lowercase
FUNCTION lch(ch: CHAR): CHAR;
	BEGIN
		lch := ch;
		IF ch IN ['A'..'Z'] THEN
			lch := chr(ord(ch) OR $20);
	END;
	
// toggle uper/lower case character
FUNCTION ulch(ch: CHAR): CHAR;
	BEGIN
		ulch := ch;
		IF ch IN ['a'..'z'] THEN ulch := uch(ch);
		IF ch IN ['A'..'Z'] THEN ulch := lch(ch);
	END;
	
// convert a string to uppercase
FUNCTION ucase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		ucase := '';
		FOR i := 1 TO Length(str) DO
			ucase := ucase + uch(str[i]);
	END;
	
// convert a string to lowercase
FUNCTION lcase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		lcase := '';
		FOR i := 1 TO Length(str) DO
			lcase := lcase + lch(str[i]);
	END;

// reverse cases in a given string
FUNCTION ulcase(str: STRING): STRING;
	var i: Integer;
	BEGIN
		ulcase := '';
		FOR i := 1 TO Length(str) DO
			ulcase := ulcase + ulch(str[i]);
	END;

VAR
	ab : STRING = 'alphaBETA';
	
BEGIN
	// demonstration
	Writeln('Original string : ',ab);
	Writeln('Reversed case   : ',ulcase(ab));
	Writeln('Upper case      : ',ucase(ab));
	Writeln('Lower case      : ',lcase(ab));
END.
