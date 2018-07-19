// The Vigenere cipher in reasonably standard Pascal
// <no library functions: all conversions hand-coded>
PROGRAM Vigenere;

// get a letter's alphabetic position (A=0)
FUNCTION letternum(letter: CHAR): BYTE;
	BEGIN
		letternum := (ord(letter)-ord('A'));
	END;

// convert a character to uppercase
FUNCTION uch(ch: CHAR): CHAR;
	BEGIN
		uch := ch;
		IF ch IN ['a'..'z'] THEN
			uch := chr(ord(ch) AND $5F);
	END;
	
// convert a string to uppercase
FUNCTION ucase(str: STRING): STRING;
	VAR i: BYTE;
	BEGIN
		ucase := '';
		FOR i := 1 TO Length(str) DO
			ucase := ucase + uch(str[i]);
	END;
	
// construct a Vigenere-compatible string:
// uppercase; no spaces or punctuation.
FUNCTION vstr(pt: STRING): STRING;
	VAR c: Cardinal;
		s: STRING;
	BEGIN
		vstr:= '';
		s 	:= ucase(pt);
		FOR c := 1 TO Length(s) DO BEGIN
			IF s[c] IN ['A'..'Z'] THEN
				vstr += s[c];
		END;
	END;
	
// construct a repeating Vigenere key
FUNCTION vkey(pt, key: STRING): STRING;
	VAR c,n: Cardinal;
		k  : STRING;
	BEGIN
		k    := vstr(key);
		vkey := '';
		FOR c := 1 TO Length(pt) DO BEGIN
			n := c mod Length(k);
			IF n>0 THEN vkey += k[n] ELSE vkey += k[Length(k)];
		END;
	END;
	
// Vigenere encipher	
FUNCTION enVig(pt,key:STRING): STRING;
	VAR ct: STRING;
		c,n	 : Cardinal;
	BEGIN
		ct := pt;
		FOR c := 1 TO Length(pt) DO BEGIN
			n := letternum(pt[c])+letternum(key[c]);
			n := n mod 26;
			ct[c]:=chr(ord('A')+n);
		END;
		enVig := ct;
	END;
	
// Vigenere decipher
FUNCTION deVig(ct,key:STRING): STRING;
	VAR pt	: STRING;
		c,n	: INTEGER;
	BEGIN
		pt := ct;
		FOR c := 1 TO Length(ct) DO BEGIN
			n := letternum(ct[c])-letternum(key[c]);
			IF n<0 THEN n:=26+n;
			pt[c]:=chr(ord('A')+n);
		END;
		deVig := pt;
	END;	

	
VAR 	key: STRING = 'Vigenere cipher';
		msg: STRING = 'Beware the Jabberwock! The jaws that bite, the claws that catch!';
		vtx: STRING = '';
		ctx: STRING = '';
		ptx: STRING = '';

BEGIN
	// make Vigenere-compatible
	vtx := vstr(msg);
	key := vkey(vtx,key);
	// Vigenere encipher / decipher
	ctx := enVig(vtx,key);
	ptx := deVig(ctx,key);
	// display results
	Writeln('Message      : ',msg);
	Writeln('Plaintext    : ',vtx);
	Writeln('Key          : ',key);
	Writeln('Ciphertext   : ',ctx);
	Writeln('Plaintext    : ',ptx);
END.
