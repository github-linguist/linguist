static score, mistakes = 0, 1
static hackamount, hacked = 0.5

local terminal = import("terminal")

local names = {}

local i = 1
for name with love.filesystem.lines("assets/names.txt") {
	names[i] = name
	i += 1
}

local commands
local seed2 = love.math.random(10, 10000) 

local fn EntryCode(seed) {
	local rng = love.math.newRandomGenerator((seed2 + math.floor(leveltime / 30)) * 10 + seed)
	local code = ""
	for _ = 1, 20 {
		code ..= string.char(rng::random(33, 122))
	}
	return code
}

local fn GenerateIP(hacker) {
	return math.ceil(math.abs(love.math.randomNormal(75, 128)) % (hacker ? 256 : 1000))
}

local user = {
	new = fn(self) {
		local hacker = love.math.random() > 0.8
		self.ip1 = GenerateIP(hacker)
		self.ip2 = GenerateIP(hacker)
		self.ip3 = GenerateIP(hacker)
		self.ip4 = GenerateIP(hacker)
		self.glitch = self.ip1 > 255 || self.ip2 > 255 || self.ip3 > 255 || self.ip4 > 255
		self.name = names[love.math.random(6000)]
		self.age = math.floor((hacker || self.glitch) ? love.math.random(14, 79) : love.math.random(9, 145))
		self.code = hacker ? love.math.random(-20, -1) : love.math.random(0, 9)
		self.time = leveltime
		self.hacker = hacker
	}
	next = fn(self) {
		commands.score.code()
		terminal::write("Reading next user's information...", 15)
		self::new()
		commands.user.code(4)
	}
}

user::new()

local fn AwardScore(amount) {
	if amount < 0 {
		amount = math.ceil($ * mistakes)
		terminal::write(string.format("(%d points)", amount), 60, 1, 0, 0)
		mistakes += 0.5
	} else {
		terminal::write(string.format("(+%d points)", amount), 60, 1, 1, 0)
	}
	score += amount
}

commands = {
	score = {
		desc = "Display current score."
		code = fn {
			terminal::write("Score: " .. score, nil, 1, 0, 1)
			local mistakes = (mistakes - 1) * 2
			if mistakes == 0 {
				terminal::write("You never messed up so far.", nil, 1, 0, 1)
			} else {
				terminal::write("You messed up " .. mistakes .. " times.", nil, 1, 0, 1)
			}
		}
	}
	user = {
		desc = "Display the info of the user trying to connect."
		code = fn(s = 0) {
			terminal::write("Name: " .. user.name, 60, 1, 1, 0)
			terminal::write("Age: " .. user.age, 60, 1, 1, 0)
			terminal::write(string.format("IP: %d.%d.%d.%d", user.ip1, user.ip2, user.ip3, user.ip4), 60, 1, 1, 0)
			terminal::write(string.format(
				"Entry Code: %s   (changes in %d seconds)",
				EntryCode(user.code),
				math.max(29 - math.floor(leveltime % 30) - s, 0)
			), 60, 1, 1, 0)
		}
	}
	codes = {
		desc = "Display the 10 currently valid codes."
		code = fn {
			terminal::write("Currently valid codes:")
			for i = 0, 9 {
				terminal::write(EntryCode(i), 60, 1, 1, 0)
			}
			terminal::write(string.format(
				"The codes will change in %d seconds.",
				29 - math.floor(leveltime % 30)
			))
		}
	}
	allow = {
		desc = "Allow the current user."
		code = fn {
			terminal::write(user.name .. " was allowed connection...", 10)
			if user.hacker {
				terminal::write("But they were an hacker!", 60, 1, 0, 0)
				hacked = leveltime + 0.5
				mistakes += 0.5
				errors = 2
				user::new()
				return
			} elseif user.glitch {
				terminal::write("But they were a glitch!", 60, 1, 0, 0)
				AwardScore(math.min(score / -2.5, -500))
				errors = 1
			} elseif user.age < 14 {
				terminal::write("But they were too young to join.", 60, 1, 0, 0)
				AwardScore(-300)
			} elseif user.age >= 80 {
				terminal::write("But they were a troll.", 60, 1, 0, 0)
				AwardScore(-200)
			} else {
				terminal::write("And that was the correct choice!", 60, 1, 1, 0)
				AwardScore(math.max(500 + math.floor((user.time - leveltime) * 20), 10))
			}
			user::next()
		}
	}
	deny = {
		desc = "Deny the current user."
		code = fn {
			terminal::write(user.name .. " was denied connection...", 10)
			if !user.hacker && !user.glitch && user.age >= 14 && user.age < 80 {
				terminal::write("But they had no bad intentions.", 60, 1, 0, 0)
				AwardScore(-500)
			} else {
				terminal::write("And that was the correct choice!", 60, 1, 1, 0)
				AwardScore(math.max(300 + math.floor((user.time - leveltime) * 20), 10))
			}
			user::next()
		}
	}
}

return commands