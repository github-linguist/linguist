on max(aList)
	set _curMax to first item of aList
	repeat with i in (rest of aList)
		if i > _curMax then set _curMax to contents of i
	end repeat
	return _curMax
end max
