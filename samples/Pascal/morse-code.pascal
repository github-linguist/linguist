{$mode delphi}
PROGRAM cw;
		// Output a string as Morse code and CW.
		// Cross-platform PCM audio uses OpenAL
USES 	OpenAL, HRTimer;

		// Intl. Morse codes in ASCII order
CONST  	Morse: ARRAY [32..95] OF STRING = (' ','-.-.--','.-..-.','#','...-..-','%','.-...','.----.','-.--.','-.--.-','*','.-.-.','--..--','-....-','.-.-.-','-..-.','-----','.----','..---','...--','....-','.....','-....','--...','---..','----.','---...','-.-.-.','>','-...-','<','..--..','.--.-.','.-','-...','-.-.','-..','.','..-.','--.','....','..','.---','-.-','.-..','--','-.','---','.--.','--.-','.-.','...','-','..-','...-','.--','-..-','-.--','--..','-.--.','\','-.--.-','~','..--.-');
		// lengthen dah by this fraction of dit:
		// best = 0.4; also lengthens pauses
		doh = 0.4;
		// an 0.05 sec dit is around 26 wpm
		dit = 0.05;
		dah = 3 * dit + doh * dit;

VAR 	// OpenAL variables
		buffer : TALuint;
		source : TALuint;
		sourcepos: ARRAY [0..2] OF TALfloat= ( 0.0, 0.0, 0.0 );
		sourcevel: ARRAY [0..2] OF TALfloat= ( 0.0, 0.0, 0.0 );

		argv: ARRAY OF PalByte;
		format: TALEnum;
		size: TALSizei;
		freq: TALSizei;
		loop: TALInt;
		data: TALVoid;

		// rewinding has an effect on the output:
		// <with> and <without> sound rather different
		rewind	: BOOLEAN = FALSE;
		// the high-res timer is from Wolfgang Ehrhardt
		// http://www.wolfgang-ehrhardt.de/misc_en.html
		t		: THRTimer;
		msg		: STRING = 'the quick brown fox jumps over the lazy dog.';
	

	PROCEDURE PlayS(s: Extended);
		BEGIN
			StartTimer(t);
			AlSourcePlay(source);			
			WHILE readseconds(t) < s DO BEGIN END;
			IF rewind THEN AlSourceRewind(source);
			AlSourceStop(source);
		END;
		
	PROCEDURE Pause(s: Extended);
		BEGIN
			StartTimer(t);
			WHILE readseconds(t) < s DO BEGIN END;
		END;
		
	PROCEDURE doDit;
		BEGIN
			PlayS(dit);
			Pause(dit);
		END;

	PROCEDURE doDah;
		BEGIN
			PlayS(dah);
			Pause(dit);
		END;
		
	// ASCII char to Morse CW
	FUNCTION AtoM(ch: CHAR): STRING;
		VAR i: Integer;
			u: CHAR;
		BEGIN
			u := ch;
			IF ch IN ['a'..'z'] THEN u := chr(ord(ch) AND $5F);
			result := Morse[ord(u)];
			FOR i := 1 TO Length(result) DO
				CASE result[i] OF
					'.': BEGIN doDit; Write('. ') END;
					'-': BEGIN doDah; Write('_ ') END;
				END;
			Pause(dah);
			Write('  ');
			IF u = ' ' THEN Write('  ');
		END;

	// ASCII string to Morse CW	
	PROCEDURE StoM(s: STRING);
		VAR i: Integer;
		BEGIN
			FOR i := 1 TO Length(s) DO AtoM(s[i]);
		END;
				
BEGIN
	// OpenAL preparation
	InitOpenAL;
	AlutInit(nil,argv);
	
	AlGenBuffers(1, @buffer);
	// load the 500 Hz 1 sec sine-wave file
	// get it from http://audiocheck.net
	AlutLoadWavFile('audiocheck.net_sin_500Hz_-3dBFS_1s.wav', format, data, size, freq, loop);
	AlBufferData(buffer, format, data, size, freq);
	AlutUnloadWav(format, data, size, freq);
	
	AlGenSources(1, @source);
	AlSourcei ( source, AL_BUFFER, buffer);
	AlSourcef ( source, AL_PITCH, 1.0 );
	AlSourcef ( source, AL_GAIN, 1.0 );
	AlSourcefv ( source, AL_POSITION, @sourcepos);
	AlSourcefv ( source, AL_VELOCITY, @sourcevel);
	AlSourcei ( source, AL_LOOPING, AL_TRUE);	
	
	// Sound and print the Morse
	StoM(msg);
	Pause(1.0);
	
	AlSourceRewind(source);
	AlSourceStop(source);
	
	// Clean up
	AlDeleteBuffers(1, @buffer);
	AlDeleteSources(1, @source);
	AlutExit();
END.
