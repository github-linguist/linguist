load 'plpgsql';
load 'plpgsql_lint';

create table t1(a int, b int);

create function f1()
returns void as $$
begin
  if false then
    update t1 set c = 30;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

create function g1(out a int, out b int)
as $$
  select 10,20;
$$ language sql;

create function f1()
returns void as $$
declare r record;
begin
  r := g1();
  if false then 
    raise notice '%', r.c;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();
drop function g1();

create function g1(out a int, out b int)
returns setof record as $$
select * from t1;
$$ language sql;

create function f1()
returns void as $$
declare r record;
begin
  for r in select * from g1()
  loop
    raise notice '%', r.c;
  end loop;
end;
$$ language plpgsql;

select f1();

create or replace function f1()
returns void as $$
declare r record;
begin
  for r in select * from g1()
  loop
    r.c := 20;
  end loop;
end;
$$ language plpgsql;

select f1();

drop function f1();
drop function g1();

create function f1()
returns int as $$
declare r int;
begin
  if false then
    r := a + b;
  end if;
  return r;
end;
$$ language plpgsql;

select f1();

drop function f1();

create or replace function f1()
returns void as $$
begin
  if false then
    raise notice '%', 1, 2;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

create or replace function f1()
returns void as $$
begin
  if false then
    raise notice '% %';
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

create or replace function f1()
returns void as $$
declare r int[];
begin
  if false then
    r[c+10] := 20;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();


create or replace function f1()
returns void as $$
declare r int;
begin
  if false then
    r[10] := 20;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

create type diagnostic_info_type as (
  status text,
  message text,
  detail text,
  row_count int);

create or replace function f1()
returns void as $$
declare  dg record;
begin
  dg := NULL::diagnostic_info_type;
  if false then
    dg.status := '00000';
    dg.mistake := 'hello';
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

create or replace function f1()
returns void as $$
declare  dg record;
begin
  if false then
    dg := 10,20;
  end if;
end;
$$ language plpgsql;

select f1();

drop function f1();

