declare @s varchar(10)
set @s = '1234.56'

print isnumeric(@s) --prints 1 if numeric, 0 if not.

if isnumeric(@s)=1 begin print 'Numeric' end
else print 'Non-numeric'
