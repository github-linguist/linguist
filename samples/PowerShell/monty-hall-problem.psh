#Declaring variables
$intIterations = 10000
$intKept = 0
$intSwitched = 0

#Creating a function
Function Play-MontyHall()
    {
    #Using a .NET object for randomization
    $objRandom = New-Object -TypeName System.Random

    #Generating the winning door number
    $intWin = $objRandom.Next(1,4)

    #Generating the chosen door
    $intChoice = $objRandom.Next(1,4)

    #Generating the excluded number
    #Because there is no method to exclude a number from a range,
    #I let it re-generate in case it equals the winning number or
    #in case it equals the chosen door.
    $intLose = $objRandom.Next(1,4)
    While (($intLose -EQ $intWin) -OR ($intLose -EQ $intChoice))
        {$intLose = $objRandom.Next(1,4)}

    #Generating the 'other' door
    #Same logic applies as for the chosen door: it cannot be equal
    #to the winning door nor to the chosen door.
    $intSwitch = $objRandom.Next(1,4)
    While (($intSwitch -EQ $intLose) -OR ($intSwitch -EQ $intChoice))
        {$intSwitch = $objRandom.Next(1,4)}

    #Simple counters per win for both categories
    #Because a child scope cannot change variables in the parent
    #scope, the scope of the counters is expanded script-wide.
    If ($intChoice -EQ $intWin)
        {$script:intKept++}
    If ($intSwitch -EQ $intWin)
        {$script:intSwitched++}

    }

#Looping the Monty Hall function for $intIterations times
While ($intIterationCount -LT $intIterations)
    {
    Play-MontyHall
    $intIterationCount++
    }

#Output
Write-Host "Results through $intIterations iterations:"
Write-Host "Keep  : $intKept ($($intKept/$intIterations*100)%)"
Write-Host "Switch: $intSwitched ($($intSwitched/$intIterations*100)%)"
Write-Host ""
