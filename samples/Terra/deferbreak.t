C = terralib.includec("stdio.h")

cur = global(int)

terra d(a : int)
    if cur ~= a then
        C.printf("found %d while expecting %d\n",a,cur)
        error(cur)
    end
    cur = cur + 1
    C.printf("d%d\n",a)
end
d:setinlined(false)

terra side(a : int, b : {})
    d(a)
end

terra doit()
    d(0)
    for i = 0,10 do
        defer d(2*i+2)
        if true then
            defer d(2*i + 1)
            if i == 8 then
                break
            end
        end
    end
    d(19)
end
cur:set(0)
doit()
doit:printpretty()

