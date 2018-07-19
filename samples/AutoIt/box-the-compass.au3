Local $avArray[33] = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, _
		151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, _
		303.75, 320.62, 320.63, 337.5, 354.37, 354.38]

For $i = 0 To UBound($avArray) - 1
	Boxing_the_compass($avArray[$i])
Next

Func Boxing_the_compass($Degree)
	Local $namearray[33] = ["North", "North by east", "North-northeast", "Northeast by north", "Northeast", _
			"Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", _
			"Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", _
			"South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", _
			"West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", _
			"North-northwest", "North by west", "North"]
	ConsoleWrite(StringFormat("%-2s", Mod(Floor($Degree / 11.25 + 0.5), 32)) & " : " & _
			StringFormat("%-20s", $namearray[Mod(Floor($Degree / 11.25 + 0.5), 32)]) & " : " & $Degree & @CRLF)
EndFunc   ;==>Boxing_the_compass
