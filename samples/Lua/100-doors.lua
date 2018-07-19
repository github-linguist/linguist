is_open = {}

for door = 1,100 do is_open[door] = false end

for pass = 1,100 do
    for door = pass,100,pass do
        is_open[door] = not is_open[door]
    end
end

for i,v in next,is_open do
    if v then
        print ('Door '..i..':','open')
    else
        print ('Door '..i..':', 'close')
    end
end
