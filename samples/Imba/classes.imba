class Animal

	def initialize name
		@name = name

	def move meters
		console.log "{@name} moved {meters}m."

class Snake < Animal
	def move
		console.log "Slithering..."
		super 5

class Horse < Animal
	def move
		console.log "Galloping..."
		super 45

var sam = Snake.new "Sammy the Python"
var tom = Horse.new "Tommy the Palomino"

sam.move
tom.move