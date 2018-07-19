SuperStrict

Framework BRL.StandardIO

spellIt(        99)
spellIt(       300)
spellIt(       310)
spellIt(      1501)
spellIt(     12609)
spellIt(    512609)
spellIt(  43112609)
spellIt(1234567890)


Type TSpell

	Field smallNumbers:String[] = ["zero", "one", "two", "three", "four", "five", ..
		"six", "seven", "eight", "nine", "ten", ..
		"eleven", "twelve", "thirteen", "fourteen", "fifteen", ..
		"sixteen", "seventeen", "eighteen", "nineteen" ]

	Field decades:String[] = [ "", "", "twenty", "thirty", "forty", ..
		"fifty", "sixty", "seventy", "eighty", "ninety" ]

	Field thousandPowers:String[] = [ " billion", " million",  " thousand", "" ]
		
	Method spellHundreds:String(number:Int)
		Local result:String
		If number > 99 Then
			result = smallNumbers[number / 100]
			result :+ " hundred"
			number = number Mod 100
			If number Then
				result :+ " and "
			End If
		End If
		
		If number >= 20 Then
			result :+ decades[number / 10]
			number = number Mod 10
			If number Then
				result :+ "-"
			End If
		End If
		If number > 0 And number < 20 Then
			result :+ smallNumbers[number]
		End If
		
		Return result
	End Method

	Method spell:String(number:Long)
		If number < 20 Then
			Return smallNumbers[number]
		End If
		Local result:String
		
		Local scaleIndex:Int = 0
		Local scaleFactor:Long = 1000000000:Long ' 1 billion
		While scaleFactor > 0
			If number >= scaleFactor
				Local h:Long = number / scaleFactor
				result :+ spellHundreds(h) + thousandPowers[scaleIndex]
				number = number Mod scaleFactor
				If number Then
					result :+ ", "
				End If
			End If
			scaleFactor :/ 1000
			scaleIndex :+ 1
		Wend
		
		Return result
	End Method
	
End Type

Function spellIt(number:Long)
	Local numberSpell:TSpell = New TSpell
	Print number + " " + numberSpell.spell(number)
End Function
