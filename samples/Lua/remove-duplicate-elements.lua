items = {1,2,3,4,1,2,3,4,"bird","cat","dog","dog","bird"}
flags = {}
io.write('Unique items are:')
for i=1,#items do
   if not flags[items[i]] then
      io.write(' ' .. items[i])
      flags[items[i]] = true
   end
end
io.write('\n')
