-- licenselist.adb --
-- run under GPS 4.3-5 (Sidux/Debian)
-- process rosetta.org text_processing/3 example
-- uses linked-list to hold times
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Containers.Doubly_Linked_Lists;
use Ada.Text_IO, Ada.Integer_Text_IO,
    Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO,
    Ada.Containers;

procedure licenselist is

 type logrec is record  -- define a record 'logrec' to place in a list
    logtext : String(1..19);
 end record;

 package dblist is new Doubly_Linked_Lists(logrec);
 use dblist;
       -- declare dblist as a list of logrec's
 licenselog : list;
 logtime    : logrec;
  -- to record the time of max OUT licenses

 infile	: File_Type;		   -- file handle
 str	: Unbounded_String;        -- input string buffer of unknown length
 outcnt, maxoutcnt : integer := 0;
 infilename : string := "license.log";

  procedure trace_times  is
  -- loop thru times list and print
     pntr : cursor := licenselog.first;
  -- pntr is of system type cursor reference to local list 'licenselog'
  begin
       new_line;
       while has_element(pntr) loop
        put(element(pntr).logtext); new_line;
        next(pntr);
       end loop;
  end trace_times;

begin -- main program --
   open ( infile,
         mode=> in_file,
         name=> infilename );

  loop
    exit when End_of_file ( infile );
    str := get_line( infile );
    if index( str, "OUT" ) > 0 then -- test if OUT record
      outcnt := outcnt +1;
    else                            -- else assume IN record
      outcnt := outcnt -1;
    end if;
    if outcnt > maxoutcnt then
         maxoutcnt := outcnt;
	 logtime.logtext := slice(str,15,33); -- date_time field
         licenselog.clear;             -- reset list for new time(s)
         licenselog.append (logtime);  -- put current time into list
    elsif outcnt = maxoutcnt then
         logtime.logtext := slice(str,15,33);  -- date_time field
         licenselog.append (logtime);   -- add current time into list
    end if;  -- have to account for possibility of equal number of OUT's
  end loop;
   put("The max. number of licenses OUT is ");put(maxoutcnt,5); new_line;
   put(" at these times ");

  trace_times;
  close ( infile );
end licenselist;
