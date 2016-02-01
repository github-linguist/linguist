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

terra foo()
    d(0)
    defer d(14)
	var a = 0
	::begin::
	defer d(terralib.select(a == 10,13,a + 1))
	if a >= 10 then
		defer d(11)
		goto theend
	end
	a = a + 1
	goto begin
	::theend::
	defer d(12)
	return a
end

foo:printpretty()
cur:set(0)

test = require("test")
test.eq(foo(),10)