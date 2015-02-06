create procedure check_reorg_tables (in v_schema varchar(128), out v_reorg_counter integer)
begin

  declare loc result_set_locator varying;

  declare schema_out        varchar(128);
  declare table_out         varchar(128);
  declare card_out          integer;
  declare overflow_out      integer;
  declare npages_out        integer;
  declare fpages_out        integer;
  declare active_blocks_out integer;
  declare tsize_out         integer;
  declare f1_out            integer;
  declare f2_out            integer;
  declare f3_out            integer;
  declare reorg_out         varchar(3);
  declare cursor_end        smallint default 0;

  declare continue handler for NOT FOUND
 
  set cursor_end = 1;
  set v_reorg_counter = 0;

  call reorgchk_tb_stats('S', v_schema);
  associate result set locator(loc) with procedure reorgchk_tb_stats;
  allocate mycursor cursor for result set loc;

  open mycursor;
  repeat
    fetch from mycursor into schema_out, table_out, card_out, overflow_out, npages_out, fpages_out, active_blocks_out, tsize_out, f1_out, f2_out, f3_out, reorg_out;
     if reorg_out <> '---' then
        set v_reorg_counter = v_reorg_counter + 1;
     end if;
     until cursor_end = 1
  end repeat;
  close mycursor;

end!
