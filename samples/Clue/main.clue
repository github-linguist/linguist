static font = love.graphics.newFont("assets/clacon2.ttf", 24, "mono")
static fontheight = 23 //force it to 23 cause windows and linux use different sizes...
static screenheight = love.graphics.getHeight() - 10
static leveltime = 0

io.stdout::setvbuf("no")

local utf8 = require("utf8")
local terminal = import("terminal")
import("player")

local screen = love.graphics.newCanvas()
local hum = love.audio.newSource("assets/hum.ogg", "stream")
local win = love.audio.newSource("assets/win.ogg", "static")
local wrong = love.audio.newSource("assets/wrong.ogg", "static")

love.graphics.setFont(font)
love.keyboard.setKeyRepeat(true)
love.graphics.setShader(love.graphics.newShader("assets/shader.glsl"))

local start_command = commands.start

method love.load() {
	terminal::write("╔═╗ ╔═════╗     ╔═════╗ ╔═╗     ╔═════╗ ╔═════╗ ╔═════╗ ╔═════╗", 400)
	terminal::write("║ ║ ║ ╔═╗ ║     ║ ╔═╗ ║ ║ ║     ║ ╔═══╝ ║ ╔═╗ ║ ║ ╔═══╝ ║ ╔═══╝", 400)
	terminal::write("║ ║ ║ ╚═╝ ║     ║ ╚═╝ ║ ║ ║     ║ ╚═══╗ ║ ╚═╝ ║ ║ ╚═══╗ ║ ╚═══╗", 400)
	terminal::write("║ ║ ║ ╔═══╝     ║ ╔═══╝ ║ ║     ║ ╔═══╝ ║ ╔═╗ ║ ╚═══╗ ║ ║ ╔═══╝", 400)
	terminal::write("║ ║ ║ ║         ║ ║     ║ ╚═══╗ ║ ╚═══╗ ║ ║ ║ ║ ╔═══╝ ║ ║ ╚═══╗", 400)
	terminal::write("╚═╝ ╚═╝         ╚═╝     ╚═════╝ ╚═════╝ ╚═╝ ╚═╝ ╚═════╝ ╚═════╝", 400)
	terminal::write("A game for the LÖVE Jam 2023 - Created by Maiori")
	terminal::write("(Type 'guide' for instructions)")
	hum::setLooping(true)
	hum::setVolume(0.10)
	hum::play()
}

method love.update(dt) {
	leveltime += dt
	local unfinished = terminal.unfinished[pointer]
	if unfinished {
		unfinished.progress += dt * unfinished.speed * 2
		if unfinished.progress >= #unfinished.text {
			unfinished.progress = nil
			table.remove(terminal.unfinished, 1)
			love.wheelmoved(0, -fontheight)
			match unfinished.text::sub(-7) { //this is a hacky way to do this...
				"glitch!" => {
					terminal::corrupt()
				}
				"choice!" => {
					win::play()
				}
				"o join." || " troll." || "ntions." => {
					wrong::play()
				}
			}
		}
	} elseif hacked && leveltime > hacked {
		terminal::corrupt()
		hacked = leveltime + hackamount
		hackamount /= 1.22
		if hackamount < 0.00000000000001 {
			hacked = nil
			hackamount = 0.5
			commands.clear.code()
			setmetatable(commands, {})
			commands.restart = start_command
			terminal::write("GAME OVER", 1, 1, 0, 0)
			terminal::write("Your server was destroyed...will you try again?", 15, 1, 0, 0)
			terminal::write("Final Score: " .. score, nil, 1, 0, 0)
			terminal::write("You messed up " .. ((mistakes - 1) * 2) .. " times.", nil, 1, 0, 0)
			terminal::write("(Type 'restart' to try again)", nil, 1, 0, 0)
			score = 0
			mistakes = 1
		}
	}
}

global fn UpdateCanvas() {
	love.graphics.clear()
	local y = terminal::draw()
	if terminal::finished() && !hacked {
		love.graphics.setColor(0.125490196, 0.760784314, 0.054901961)
		local text = "$ " .. terminal.input[1]
		love.graphics.print(text, 5, y)
		if math.floor(leveltime) % 2 == 0 {
			love.graphics.print("_", 5 + font::getWidth(text), y)
		}	
	}
}

method love.draw() {
	screen::renderTo(UpdateCanvas)
	love.graphics.setColor(1, 1, 1)
	love.graphics.draw(screen)
}

method love.resize(_, height) {
	screenheight = height - 10
	screen = love.graphics.newCanvas()
	love.wheelmoved(0, 0)
}