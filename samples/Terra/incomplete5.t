


--terra things that should complete the type
local ct = {}

--terra things that should not complete the type
local dct = {}



function ct.a(A)
	return terra(a : A) return a end
end

function ct.b(A)
	return terra() var a : A end
end

function ct.c(A)
	return terra(a : &A) return a + 1 end
end
function ct.d(A)
	local at = A[3]
	return terra(a : A[3]) return a[0] end
end
function ct.e(A)
	local struct B { a : A }
	return terra(a : B) end
end

function dct.c(A)
	local at = A[3]
	return terra(a : &A[3]) return a end
end

function dct.b(A)
	return terra(a : &A) return a end
end

function dct.d(A)
	local struct B {
		a : &A;
		b : int
	}
	return terra(a : &B) return a.b end
end


local function testthem(stuff,completestype)
	for k,v in pairs(stuff) do
		local struct A
		if completestype then
			struct A {
				a : int
			}
		end
		local result = v(A)
		result:compile()
		assert(A:iscomplete() == completestype)
	end
end

testthem(ct,true)
testthem(dct,false)