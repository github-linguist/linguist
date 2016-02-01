local alignment = 16
local aligned = terralib.aligned
terra foobar(a : &float)
	terralib.attrstore(a,terralib.attrload(a+3,{ align = alignment }), { align = alignment })
end

foobar:disas()