function choice(choices)
  for i, v in ipairs(choices) do print(i, v) end

  print"Enter your choice"
  local selection = io.read() + 0

  if choices[selection] then print(choices[selection])
  else choice(choices)
  end
end

choice{"fee fie", "huff and puff", "mirror mirror", "tick tock"}
