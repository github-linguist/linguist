my_flatten({{1}, 2, {{3, 4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}})

on my_flatten(aList)
	if class of aList is not list then
		return {aList}
	else if length of aList is 0 then
		return aList
	else
		return my_flatten(first item of aList) & (my_flatten(rest of aList))
	end if
end my_flatten
