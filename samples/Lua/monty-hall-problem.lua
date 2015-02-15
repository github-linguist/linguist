function playgame(player)
   local car = math.random(3)
   local pchoice = player.choice()
   local function neither(a, b) --slow, but it works
      local el = math.random(3)
      return (el ~= a and el ~= b) and el or neither(a, b)
   end
   local el = neither(car, pchoice)
   if(player.switch) then pchoice = neither(pchoice, el) end
   player.wins = player.wins + (pchoice == car and 1 or 0)
end
for _, v in ipairs{true, false} do
   player = {choice = function() return math.random(3) end,
      wins = 0, switch = v}
   for i = 1, 20000 do playgame(player) end
   print(player.wins)
end
