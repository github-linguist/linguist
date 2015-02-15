SET @txt = REPLACE('In girum imus nocte et consumimur igni', ' ', '');
SELECT REVERSE(@txt) = @txt;
