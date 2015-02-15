on leap_year(y)
	if (y mod 100 is equal to 0) then
		return (y mod 400 is equal to 0)
	end if
	return (y mod 4 is equal to 0)
end leap_year

leap_year(1900)
