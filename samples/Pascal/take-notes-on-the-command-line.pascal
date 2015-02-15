{$mode delphi}
PROGRAM notes;
// Notes: a time-stamped command line notebook
// usage: >notes "note"< or >notes< to display contents
USES Classes, SysUtils;

VAR
	Note  : TStringList;
	Fname : STRING = 'Notes.txt';
	Dtime : STRING;
	Ntext : STRING;
	c     : Cardinal;
	
BEGIN
	DTime := FormatDateTime('YYYY-MM-DD-hhnn',Now);
	Note  := TStringList.Create;
	WITH Note DO BEGIN
		TRY
			LoadFromFile(Fname);
		EXCEPT
			Add(DTime);
			NText := 'Notes.txt created.';
		END;
		// command line args present:
		// add note with date & time
		IF ParamStr(1) <> '' THEN BEGIN
			NText := ParamStr(1);
			Add(DTime);
			Add(NText);
			SaveToFile(Fname);
		// command line args absent:
		// display contents of notebook
		END ELSE
			FOR c := 0 TO Count-1 DO
				Writeln(Note[c]);
		Free;
	END;
END.
