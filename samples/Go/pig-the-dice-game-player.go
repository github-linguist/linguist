package pig

import (
	"fmt"
	"math/rand"
	"time"
)

type (
	PlayerID   int
	MessageID  int
	StrategyID int

	PigGameData struct {
		player        PlayerID
		turnCount     int
		turnRollCount int
		turnScore     int
		lastRoll      int
		scores        [2]int
		verbose       bool
	}
)

const (
	// Status messages
	gameOver = iota
	piggedOut
	rolls
	pointSpending
	holds
	turn
	gameOverSummary
	// Players
	player1  = PlayerID(0)
	player2  = PlayerID(1)
	noPlayer = PlayerID(-1)
	// Max score
	maxScore = 100
	// Strategies
	scoreChaseStrat = iota
	rollCountStrat
)

// Returns "s" if n != 1
func pluralS(n int) string {
	if n != 1 {
		return "s"
	}
	return ""
}

// Creates an intializes a new PigGameData structure, returns a *PigGameData
func New() *PigGameData {
	return &PigGameData{0, 0, 0, 0, 0, [2]int{0, 0}, false}
}

// Create a status message for a given message ID
func (pg *PigGameData) statusMessage(id MessageID) string {
	var msg string
	switch id {
	case gameOver:
		msg = fmt.Sprintf("Game is over after %d turns", pg.turnCount)
	case piggedOut:
		msg = fmt.Sprintf("    Pigged out after %d roll%s", pg.turnRollCount, pluralS(pg.turnRollCount))
	case rolls:
		msg = fmt.Sprintf("    Rolls %d", pg.lastRoll)
	case pointSpending:
		msg = fmt.Sprintf("    %d point%s pending", pg.turnScore, pluralS(pg.turnScore))
	case holds:
		msg = fmt.Sprintf("    Holds after %d turns, adding %d points for a total of %d", pg.turnRollCount, pg.turnScore, pg.PlayerScore(noPlayer))
	case turn:
		msg = fmt.Sprintf("Player %d's turn:", pg.player+1)
	case gameOverSummary:
		msg = fmt.Sprintf("Game over after %d turns\n player 1 %d\n player 2 %d\n", pg.turnCount, pg.PlayerScore(player1), pg.PlayerScore(player2))
	}
	return msg
}

// Print a status message, if pg.Verbose is true
func (pg *PigGameData) PrintStatus(id MessageID) {
	if pg.verbose {
		fmt.Println(pg.statusMessage(id))
	}
}

// Play a given strategy
func (pg *PigGameData) Play(id StrategyID) (keepPlaying bool) {
	if pg.GameOver() {
		pg.PrintStatus(gameOver)
		return false
	}

	if pg.turnCount == 0 {
		pg.player = player2
		pg.NextPlayer()
	}

	pg.lastRoll = rand.Intn(6) + 1
	pg.PrintStatus(rolls)
	pg.turnRollCount++
	if pg.lastRoll == 1 {
		pg.PrintStatus(piggedOut)
		pg.NextPlayer()
	} else {
		pg.turnScore += pg.lastRoll
		pg.PrintStatus(pointSpending)
		success := false
		switch id {
		case scoreChaseStrat:
			success = pg.scoreChaseStrategy()
		case rollCountStrat:
			success = pg.rollCountStrategy()
		}
		if success {
			pg.Hold()
			pg.NextPlayer()
		}
	}
	return true
}

// Get the score for a given player
func (pg *PigGameData) PlayerScore(id PlayerID) int {
	if id == noPlayer {
		return pg.scores[pg.player]
	}
	return pg.scores[id]
}

// Check if the game is over
func (pg *PigGameData) GameOver() bool {
	return pg.scores[player1] >= maxScore || pg.scores[player2] >= maxScore
}

// Returns the Player ID if there is a winner, or -1
func (pg *PigGameData) Winner() PlayerID {
	for index, score := range pg.scores {
		if score >= maxScore {
			return PlayerID(index)
		}
	}
	return noPlayer
}

// Get the ID of the other player
func (pg *PigGameData) otherPlayer() PlayerID {
	// 0 becomes 1, 1 becomes 0
	return 1 - pg.player
}

func (pg *PigGameData) Hold() {
	pg.scores[pg.player] += pg.turnScore
	pg.PrintStatus(holds)
	pg.turnRollCount, pg.turnScore = 0, 0
}

func (pg *PigGameData) NextPlayer() {
	pg.turnCount++
	pg.turnRollCount, pg.turnScore = 0, 0
	pg.player = pg.otherPlayer()
	pg.PrintStatus(turn)
}

func (pg *PigGameData) rollCountStrategy() bool {
	return pg.turnRollCount >= 3
}

func (pg *PigGameData) scoreChaseStrategy() bool {
	myScore := pg.PlayerScore(pg.player)
	otherScore := pg.PlayerScore(pg.otherPlayer())
	myPendingScore := pg.turnScore + myScore
	return myPendingScore >= maxScore || myPendingScore > otherScore || pg.turnRollCount >= 5
}

// Run the simulation
func main() {
	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())

	// Start a new game
	pg := New()
	pg.verbose = true
	strategies := [2]StrategyID{scoreChaseStrat, rollCountStrat}

	// Play until game over
	for !pg.GameOver() {
		pg.Play(strategies[pg.player])
	}
	pg.PrintStatus(gameOverSummary)
}
