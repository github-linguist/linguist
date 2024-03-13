let stdout = process.stdout
let clear_screen = do stdout.write "\x1b[2J"
let hide_cursor = do stdout.write "\x1b[?25l"
let show_cursor = do stdout.write "\x1b[?25h"
let smcup = do stdout.write "\x1b[?1049h"
let rmcup = do stdout.write "\x1b[?1049l"
let place_cursor = do stdout.write "\x1b[1;1H"

let alive_cell_char = "◼"
let dead_cell_char = " "

class Game

	constructor rows, cols
		self.rows = rows
		self.cols = cols
		self.board = get_empty_board!
		randomize_board!

	def get_empty_board
		new Array((rows + 2) * (cols + 2)).fill(no)

	def idx i, j
		(i + 1) * (cols + 2) + (j + 1)

	def randomize_board
		for i in [0 .. rows - 1]
			for j in [0 .. cols - 1]
				board[idx(i, j)] = Math.random! < 0.5

	def print_whole_board
		let s = ""
		for cell, i in board
			if cell
				s += alive_cell_char
			else
				s += dead_cell_char
			if (i + 1) % (cols + 2) == 0
				s += "\n"
		stdout.write s

	def print_board
		let s = ""
		for i in [0 .. rows - 1]
			for j in [0 .. cols - 1]
				if board[idx(i, j)]
					s += alive_cell_char
				else
					s += dead_cell_char
			s += "\n"
		stdout.write s

	def count_neighbors i, j
		board[idx(i - 1, j - 1)] +
		board[idx(i - 1, j)] +
		board[idx(i - 1, j + 1)] +
		board[idx(i, j - 1)] +
		board[idx(i, j + 1)] +
		board[idx(i + 1, j - 1)] +
		board[idx(i + 1, j)] +
		board[idx(i + 1, j + 1)]

	def tick
		let next = get_empty_board!
		let nc
		for i in [0 .. rows - 1]
			for j in [0 .. cols - 1]
				nc = count_neighbors i, j
				next[idx(i, j)] = board[idx(i, j)] and nc == 2 or nc == 3
		board = next

process.on('exit') do
	clear_screen!
	show_cursor!
	rmcup!

process.on('SIGINT', process.exit)

def main
	smcup!
	hide_cursor!
	let game = new Game rows, cols
	let count = 0
	setInterval(&, 50) do
		if count > max_generations
			clearInterval this
		place_cursor!
		stdout.write "{count}/{max_generations}\n"
		game.print_board!
		game.tick!
		count += 1

let max_generations = 500
let delay = 50
let rows = stdout.rows >>> 1
let cols = stdout.columns >>> 1
main!
