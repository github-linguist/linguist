entropy = function(s)
   {freq = prop.table(table(strsplit(s, '')[1]))
    -sum(freq * log(freq, base = 2))}

print(entropy("1223334444"))   # 1.846439
