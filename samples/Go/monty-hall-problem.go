package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	games := 100000
	r := rand.New(rand.NewSource(time.Now().UnixNano()))

	var switcherWins, keeperWins, shown int
	for i := 0; i < games; i++ {
		doors := []int{0, 0, 0}
		doors[r.Intn(3)] = 1 // Set which one has the car
		choice := r.Intn(3) // Choose a door
		for shown = r.Intn(3); shown == choice || doors[shown] == 1; shown = r.Intn(3) {}
		switcherWins += doors[3 - choice - shown]
		keeperWins += doors[choice]
	}
	floatGames := float32(games)
	fmt.Printf("Switcher Wins: %d (%3.2f%%)\n",
		switcherWins, (float32(switcherWins) / floatGames * 100))
	fmt.Printf("Keeper Wins: %d (%3.2f%%)",
		keeperWins, (float32(keeperWins) / floatGames * 100))
}
