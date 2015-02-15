create table sequence (
	n real
)

insert into sequence values (0)
insert into sequence values (1)
insert into sequence select 2+n from sequence
insert into sequence select 4+n from sequence
insert into sequence select 8+n from sequence
insert into sequence select 16+n from sequence
insert into sequence select 32+n from sequence
insert into sequence select 64+n from sequence
insert into sequence select 128+n from sequence
insert into sequence select 256+n from sequence
insert into sequence select 512+n from sequence

select sum(1/n) from sequence where n>=1 and n<=1000
