class PigDice {

	final static int maxScore = 100;
	final static yesses = ["yes", "y", "", "Y", "YES"]

	static main(args) {
		def playersCount = 2
		Scanner sc = new Scanner(System.in)
		Map scores = [:]
		def current = 0
		def player = 0
		def gameOver = false
		def firstThrow = true
		Random rnd = new Random()

		// Initialise the players' scores
		(0..(playersCount-1)).each{ it->
			scores[it] = 0
		}

		// Game starts
		while (!gameOver) {
			def nextPlayer = false
			String ln

			// Automatic rolls for the first dice roll
			if (firstThrow){
				println "player ${player+1} Auto Rolling... "
				ln = 'y'
				firstThrow = false
			} else {
				println "player ${player+1} Rolling? Yes(y) or No(n) "
				ln = sc.nextLine()
			}

			if (ln in yesses){
				// if yes then roll the dice
				int rolled = rnd.nextInt(6) + 1
				print  "The Roll was $rolled ---  "

				if (rolled == 1) {
					println " Bust! Player ${player+1} loses $current but keep ${scores[player]}"
					current = 0
					nextPlayer = true
					firstThrow = true
				} else {
					//  dice rolls 2 to 6
					current = current + rolled
					if ((current + scores[player]) > maxScore){
						gameOver = true
					}else{
						// as a session score gets larger the message returned changes
						switch (current){
							case 6..15:
								print "Good. "
								break
							case 15..29:
								print "lucky! "
								break
							case 29..39:
								print "Great! "
								break
							default:
								print "Amazing "
						}
						println "Player ${player+1} now has $current this session (possible score of ${current + scores[player]})"
					}
				}
			} else{
				// if no then bank the session score
				nextPlayer = true
				firstThrow = true
				scores[player] = scores[player] + current
				current = 0
				println "chicken! player ${player+1} now has ${scores[player]} and $gameOver"
				println  "Current scores :"
				for (i in scores){
					println "player ${i.key + 1}| ${i.value} "
				}
				println "------------------------------"

			}
			println ""

			if (nextPlayer) {
				player = (player+1)%playersCount
				println "** Next player is ${player+1}"
			}
		}

		// Game ends
		println "Player ${player+1} wins"
	}
}
