static commands
static offset = 0
static pointer = 1

local utf8 = require("utf8")
local terminal = import("terminal")

local exiting = false
local beep = love.audio.newSource("assets/beep.ogg", "static")
local key_press = love.audio.newSource("assets/key.ogg", "static")

commands = {
	exit = {
		desc = "Quit the game."
		code = fn {
			if exiting {
				love.event.quit()
			} else {
				exiting = true
				terminal::write("Are you sure?", nil, 1, 0, 0)
				terminal::write("(Type 'exit' again to confirm)")
			}
		}
	}
	help = {
		desc = "Display this message."
		code = fn {
			terminal::write("List of currently available commands:")
			for k, v of commands {
				terminal::write(string.format("%s - %s", k::upper(), v.desc))
			}
			local game_commands = getmetatable(commands)?.__index
			if game_commands {
				for k, v of game_commands {
					terminal::write(string.format("%s - %s", k::upper(), v.desc))
				}
			}
		}
	}
	fps = {
		desc = "Display the game's FPS."
		code = fn {
			local fps = love.timer.getFPS()
			terminal::write("FPS: " .. tostring(fps), 30, (1 - fps / 60) * 2, fps / 60, 0)
		}
	}
	memory = {
		desc = "Display the amount of memory in use."
		code = fn {
			terminal::write(string.format("Used memory: %dkb", collectgarbage("count")))
		}
	}
	clear = {
		desc = "Clears the terminal."
		code = fn {
			for k, _ in terminal {
				terminal[k] = nil
			}
			love.wheelmoved(0, 0)
			collectgarbage("collect")
		}
	}
	guide = {
		desc = "Explains how to play the game."
		code = fn {
			terminal::write("Welcome to IP Please!")
			terminal::write("You are the admin of a private server.")
			terminal::write("Many people want to join, your job is to let them in.")
			terminal::write("(Type 'allow' to let a user in)")
			terminal::write("Hackers may try to infiltrate, do not let them in.")
			terminal::write("(Type 'deny' to not let a user in)")
			terminal::write("Hackers will use older entry codes that are no longer valid.")
			terminal::write("Valid codes change every 30 seconds.")
			terminal::write("(Type 'codes' to get a list of currently valid codes.)")
			terminal::write("Trolls will try to join with high ages (80+), deny them.")
			terminal::write("Also, deny anyone younger than 14.")
			terminal::write("Lastly, the server may glitch, you have to prevent that.")
			terminal::write("To do that, deny any user with a number above 255 in their IP.")
			terminal::write("Allowing and denying users correctly will grant you points.")
			terminal::write("(The faster you are the more points you get)")
			terminal::write("Messing up may lead to the end of the server.")
			terminal::write("Good luck, and thanks for playing!", 60, 1, 1, 0)
			terminal::write("(Type 'start' to begin)")
		}
	}
	start = {
		desc = "Starts the game."
		code = fn {
			beep::play()
			leveltime = 0
			terminal::write("Starting server...", 5)
			terminal::write("Done!")
			terminal::write("Reading first user's information...", 15)
			commands.start = nil
			commands.restart = nil
			setmetatable(commands, {
				__index = import("game")
			})
			commands.user.code(3)
		}
	}
}

method love.keypressed(key, _, isrepeat) {
	if hacked {
		return
	}
	if !isrepeat {
		key_press::seek(0)
		key_press::play()
	}
	love.wheelmoved(0, -math.huge)
	if terminal::finished() {
		local input = terminal.input[1]::gsub("%s+", "")
		match key {
			"up" if pointer < #terminal.input => {
				pointer += 1
				terminal.input[1] = terminal.input[pointer]
			}
			"down" if pointer > 1 => {
				pointer -= 1
				terminal.input[1] = terminal.input[pointer]
			}
			"backspace" => {
				local byteoffset = utf8.offset(input, -1)
				if byteoffset {
					terminal.input[1] = $::sub(1, byteoffset - 1)
				}
			}
			"return" => {
				pointer = 1
				if input == "" {
					terminal.input[1] = ""
					return
				}
				local command = commands[input::lower()]?.code
				if command {
					command()
				} else {
					terminal::write(input .. ": command not found", nil, 1, 0, 0)
				}
				if input != "exit" {
					exiting = false
				}
				table.insert(terminal.input, 1, "")
			}
		}
	}
}

method love.textinput(t) {
	terminal.input[1] ..= t
	love.wheelmoved(0, -math.huge)
}

method love.wheelmoved(_, y) {
	if hackamount < 0.5 {
		return
	}
	local terminal_size = -(((#terminal - #terminal.unfinished) + 1) * fontheight - screenheight)
	offset = math.min(math.max($ + y * fontheight, math.min(terminal_size, 0)), 0)
}