function isBalanced(s)
  --Lua pattern matching has a 'balanced' pattern that matches sets of balanced characters.
  --Any two characters can be used.
  return s:gsub('%b[]','')=='' and true or false
end

function randomString()
  math.randomseed(os.time())
  math.random()math.random()math.random()math.random()
  local tokens={'[',']'}
  local result={}
  for i=1,8 do
    table.insert(result,tokens[math.random(1,2)])
  end
  return table.concat(result)
end

local RS=randomString()
print(RS)
print(isBalanced(RS))
