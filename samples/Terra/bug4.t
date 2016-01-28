c = terralib.includecstring [[
	#include<stdio.h>
	#include<string.h>
]]
struct exception { slug : int8[60]; code : int; msg : int8[960]; }
EXC_INFO = terralib.new(exception)    
terra bar() c.memcpy(EXC_INFO.slug + 0, 'foobar', 7); c.printf('%s\n', EXC_INFO.slug + 0); end
bar()

terra zoo() EXC_INFO.slug[0] = 65; EXC_INFO.slug[1] = 0; c.printf('%s\n', EXC_INFO.slug + 0); end
zoo()

terra zoo2() EXC_INFO.slug[0] = 65; EXC_INFO.slug[1] = 0; return EXC_INFO.slug[0] end
assert(zoo2() == 65)