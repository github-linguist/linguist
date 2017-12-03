create trigger CHECK_HIREDATE
no cascade before insert on EMPLOYEE
referencing new as N
for each row mode db2sql
if n.hiredate > current date
then
  signal SQLSTATE '75000'
  set MESSAGE_TEXT = 'Hire date must be in the past';
end if!
