list = {"mouse", "hat", "cup", "deodorant", "television", "soap", "methamphetamine", "severed cat heads"} --contents of my desk

item = io.read()

for i,v in ipairs(list)
  if v == item then print(i) end
end
