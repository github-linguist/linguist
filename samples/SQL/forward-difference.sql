create table list (n integer, v real);
insert into list values (0, 90);
insert into list values (1, 47);
insert into list values (2, 58);
insert into list values (3, 29);
insert into list values (4, 22);
insert into list values (5, 32);
insert into list values (6, 55);
insert into list values (7, 5);
insert into list values (8, 55);
insert into list values (9, 73);

create view diff1 as select list.n, (select next.v from list as next where next.n = list.n + 1) - list.v as v from list;
create view diff2 as select list.n, (select next.v from diff1 as next where next.n = list.n + 1) - list.v as v from diff1 as list;

select * from diff1;
    0|-43.0
    1|11.0
    2|-29.0
    3|-7.0
    4|10.0
    5|23.0
    6|-50.0
    7|50.0
    8|18.0
    9|
select * from diff2;
    0|54.0
    1|-40.0
    2|22.0
    3|17.0
    4|13.0
    5|-73.0
    6|100.0
    7|-32.0
    8|
    9|
