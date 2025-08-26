static errors = 0

local utf8 = require("utf8")

local error = love.audio.newSource("assets/error.ogg", "static")

return {
	input = {""}
	unfinished = {}
	write = fn(self, text, speed = 120, r = 0.125490196, g = 0.760784314, b = 0.054901961) {
		local text = {
			progress = 0
			speed = $
			text = $
			r = $
			g = $
			b = $
		}
		table.insert(self, text)
		table.insert(self.unfinished, text)
	}
	corrupt = fn(self) {
		if !error::isPlaying() && errors > 0 {
			error::play()
			errors -= 1
		}
		local y = 5 + offset - fontheight
		for _, line in self {
			y += fontheight
			if y < -fontheight || y > screenheight + fontheight {
				continue
			}
			local len = utf8.len(line.text)
			local i = love.math.random(len)
			local corrupted = ""
			for p, c with utf8.codes(line.text) {
				corrupted ..= p == i || p == love.math.random(len)
					? (hackamount > 0.00005
						? string.char(love.math.random(33, 122))
						: " ")
					: utf8.char(c)
			}
			line.text = corrupted
			line.r = love.math.random()
			line.g = love.math.random()
			line.b = love.math.random()
		}
	}
	finished = fn(self) {
		return #self.unfinished == 0
	}
	draw = fn(self) {
		local y = 5 + offset - fontheight
		for i, line in self {
			y += fontheight
			if y < -fontheight || y > screenheight + fontheight {
				continue
			}
			local {progress, text, r, g, b} = line
			love.graphics.setColor(r, g, b)
			if !progress {
				love.graphics.print(text, 5, y)
			} elseif progress > 0 {
				local toprint = ""
				local progress = math.ceil(progress)
				for p, c with utf8.codes(text) {
					toprint ..= utf8.char(c)
					if p >= progress {
						break
					}
				}
				love.graphics.print(toprint .. "▇", 5, y)
			}
		}
		return y + fontheight
	}
}