keepWins := 0
switchWins := 0
doors := 3
times := 100000
pickDoor := method(excludeA, excludeB,
  door := excludeA
  while(door == excludeA or door == excludeB,
    door = (Random value() * doors) floor
  )
  door
)
times repeat(
  playerChoice := pickDoor()
  carDoor := pickDoor()
  shownDoor := pickDoor(carDoor, playerChoice)
  switchDoor := pickDoor(playerChoice, shownDoor)
  (playerChoice == carDoor) ifTrue(keepWins = keepWins + 1)
  (switchDoor == carDoor) ifTrue(switchWins = switchWins + 1)
)
("Switching to the other door won #{switchWins} times.\n"\
    .. "Keeping the same door won #{keepWins} times.\n"\
    .. "Game played #{times} times with #{doors} doors.") interpolate println
