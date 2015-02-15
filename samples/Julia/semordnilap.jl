raw = readdlm("unixdict.txt",String)[:]
inter = intersect(raw,map(reverse,raw)) #find the matching strings/revstrings
res = String[b == 1 && a != reverse(a) && a < reverse(a) ? a : reverse(a) for a in inter, b in 1:2] #create pairs
res = res[res[:,1] .!= res[:,2],:] #get rid of duplicates, palindromes
