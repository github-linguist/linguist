-- Create and populate tables
create table colours (id integer primary key, name varchar(5));
insert into colours (id, name) values ( 1, 'red'  );
insert into colours (id, name) values ( 2, 'white');
insert into colours (id, name) values ( 3, 'blue' );

create table balls ( colour integer references colours );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 2 );
insert into balls ( colour ) values ( 1 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 3 );
insert into balls ( colour ) values ( 2 );

-- Show the balls are unsorted
select
	colours.name
from
	balls
	join colours on balls.colour = colours.id;

-- Show the balls in dutch flag order
select
	colours.name
from
	balls
	join colours on balls.colour = colours.id
order by
	colours.id;

-- Tidy up
drop table balls;
drop table colours;
