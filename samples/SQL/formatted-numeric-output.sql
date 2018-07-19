declare @n int
select @n=123
select substring(convert(char(5), 10000+@n),2,4) as FourDigits

set @n=5
print "TwoDigits: " + substring(convert(char(3), 100+@n),2,2)
--Output: 05
