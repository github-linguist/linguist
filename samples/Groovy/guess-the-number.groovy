def random = new Random()
def keyboard = new Scanner(System.in)
def number = random.nextInt(10) + 1
println "Guess the number which is between 1 and 10: "
def guess = keyboard.nextInt()
while (number != guess) {
    println "Guess again: "
    guess = keyboard.nextInt()
}
println "Hurray! You guessed correctly!"
