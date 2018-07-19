-- Version for sqlite
with GNATCOLL.SQL_Impl;   use GNATCOLL.SQL_Impl;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite; use GNATCOLL.SQL;

procedure Prepared_Query is

   DB_Descr : Database_Description;
   Conn     : Database_Connection;
   Query    : Prepared_Statement;
   --sqlite does not support boolean fields
   True_Str : aliased String          := "TRUE";
   Param    : SQL_Parameters (1 .. 4) :=
     (1 => (Parameter_Text, null),
      2 => (Parameter_Integer, 0),
      3 => (Parameter_Text, null),
      4 => (Parameter_Integer, 0));
begin
   -- Allocate and initialize the description of the connection
   Setup_Database (DB_Descr, "rosetta.db", "", "", "", DBMS_Sqlite);
   -- Allocate the connection
   Conn := Sqlite.Build_Sqlite_Connection (DB_Descr);
   -- Initialize the connection
   Reset_Connection (DB_Descr, Conn);
   Query :=
      Prepare
        ("UPDATE players SET name = ?, score = ?, active = ? " &
         " WHERE jerseyNum = ?");
   declare
      Name : aliased String := "Smith, Steve";
   begin
      Param := ("+" (Name'Access), "+" (42), "+" (True_Str'Access), "+" (99));
      Execute (Conn, Query, Param);
   end;
   Commit_Or_Rollback (Conn);
   Free (Conn);
   Free (DB_Descr);
end Prepared_Query;
