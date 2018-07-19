l = {}
l[1] = 1      -- Index starts with 1, not 0.
l[0] = 'zero' -- But you can use 0 if you want
l[10] = 2     -- Indexes need not be continuous
l.a = 3       -- Treated as l['a']. Any object can be used as index
l[l] = l      -- Again, any object can be used as an index. Even other tables
for i,v in next,l do print (i,v) end
