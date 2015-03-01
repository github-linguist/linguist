create procedure runstats (out nr_tables integer, out nr_ok integer)
begin
  declare SQLCODE integer;
  declare stmt varchar(100);

  set nr_tables = 0;
  set nr_ok = 0;

  for line as select tabschema, tabname from syscat.tables where type='T' and tabschema='SPODEN'
  do
    set nr_tables = nr_tables + 1;
    set stmt = 'CALL SYSPROC.ADMIN_CMD (RUNSTATS ON TABLE ' concat rtrim(line.tabschema) concat '.' concat line.tabname concat ')';
    execute immediate stmt;
    if SQLCODE = 0 then
       set nr_ok = nr_ok + 1;
    end if;
  end for;
end!
