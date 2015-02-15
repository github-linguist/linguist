MODULE DateFormat;
IMPORT StdLog, Dates;

PROCEDURE Do*;
VAR
	d: Dates.Date;
	resp: ARRAY 64 OF CHAR;
BEGIN
	Dates.GetDate(d);
	Dates.DateToString(d,Dates.short,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.abbreviated,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.long,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.plainAbbreviated,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
	Dates.DateToString(d,Dates.plainLong,resp);
	StdLog.String(":> " + resp);StdLog.Ln;
END Do;
END DateFormat.
