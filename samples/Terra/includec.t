
local c = terralib.includec("mytest.h")

terra foo()
    var a : int = 3
    return c.myfoobarthing(1,2,3.5,&a) + a
end

terra bar()
	return c.myotherthing(4,5)
end

terra bar2()
	return c.myfnptr(bar)
end

terra bar3()
	var a : c.size_t = 3
	return a
end

terra bar4()
	var opaquething : c.MyStruct2
	var my = c.MyStruct { 3, 4}
	return my.a + my.b
end

terra writefile()
	c.printf("%f %f %f\n",3.0,4.0,5.0)
	var f = c.fopen("afile.txt","w")
	c.fputs("a string\n",f)
	c.fclose(f)
end

local test = require("test")

test.eq(foo(),15)
test.eq(c.myotherthing(1,2),3)
test.eq(bar2(),9)
test.eq(bar3(),3)
test.eq(bar4(),7)
writefile()
