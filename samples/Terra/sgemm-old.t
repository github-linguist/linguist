
local IO = terralib.includec("stdio.h")
local stdlib = terralib.includec("stdlib.h")



local NB = 64
local V = 8

terra vecload(data : &float, idx : int)
	var addr = &data[idx]
	return @[&vector(float,V)](addr)
end

haddavx = terralib.intrinsic("llvm.x86.avx.hadd.ps.256", { vector(float,8), vector(float,8) } -> vector(float,8))
terra hadd(v : vector(float,8))
	var v1 = haddavx(v,v)
	var v2 = haddavx(v1,v1)
	return v2[0] + v2[4]
end


local AR = 2
local BR = 2
local KR = 2
local NK = 64

local BLOCKB = true
local BLOCKA = true

local function isinteger(x) return math.floor(x) == x end

assert(isinteger(NK / (KR * V)))
assert(isinteger(NB / AR ))
assert(isinteger(NB / BR ))



blockregisters = macro(function(C,A,B,K,lda,ldc,mm,nn,kk,m,n)
	local function mkmatrix(nm,I,J)
		local r = {}
		for i = 0,I-1 do
			r[i] = {}
			for j = 0,J-1 do
				r[i][j] = symbol(nm..tostring(i)..tostring(j))
			end
		end
		return r
	end
	local as,bs,cs = mkmatrix("a",AR,KR),mkmatrix("b",BR,KR),mkmatrix("c",AR,BR)
	local stmts = terralib.newlist()
	for i = 0, AR-1 do
		for j = 0, BR-1 do
			stmts:insert(quote var [cs[i][j]] : vector(float,V) = 0.f end)
		end
	end

	local k = symbol("k")
	local kloopbody = terralib.newlist()

	local alreadyloaded = {}
	local function get(vs,i,j,loadfn)
		if not alreadyloaded[vs[i][j]] then
			alreadyloaded[vs[i][j]] = true
			kloopbody:insert(loadfn(vs[i][j]))
		end
		return vs[i][j]
	end

	local function getA(i,j)
		return get(as,i,j,function(sym)
			local result
			if BLOCKA then
				result = quote 
					var [sym] = vecload(A, mm * K + NB*(kk + m - mm + i) + k - kk + j *V) 
				end
			else
				result = quote 
					var [sym] = vecload(A, (m + i) * lda + k + j * V) 
				end
			end
			return result
		end)
	end

	local function getB(i,j)
		return get(bs,i,j,function(sym)
			local result
			if BLOCKB then
				result = quote 
					var [sym] = vecload(B, nn * K + NB*(kk + n - nn + i) + k - kk + j * V)
				end
			else
				result = quote
					var [sym] = vecload(B, (n + i) * K + k + j * V) 
				end
			end
			return result
		end)
	end

	for l = 0, KR-1 do
		for i = 0, AR-1 do
			for j = 0, BR-1 do
				local aa = getA(i,l)
				local bb = getB(j,l)
				kloopbody:insert(quote 
					[cs[i][j]] = [cs[i][j]] + aa * bb
				end)
			end
		end
	end

	stmts:insert(quote
		for [k] = kk, kk + NK, V*KR do
			kloopbody
		end
	end)

	for i = 0, AR-1 do
		for j = 0, BR-1 do
			local function getsum(b,e)
				if b + 1 == e then
					return `[cs[i][j]][b]
				else
					local mid = (e + b)/2
					assert(math.floor(mid) == mid)
					local lhs = getsum(b,mid)
					local rhs = getsum(mid,e)
					return `lhs + rhs
				end
			end
			local sum
			if V == 8 and false then
				sum = `hadd([cs[i][j]])
			else
				sum = getsum(0,V)
			end
			stmts:insert(quote
				var r = sum
				if kk == 0 then
					C[(m + i)*ldc + (n + j)] = r
				else
					C[(m + i)*ldc + (n + j)] = C[(m + i)*ldc + (n + j)] + r
				end
			end)
		end
	end

	return stmts
end)

terra my_sgemm(gettime : {} -> double, M : int, N : int, K : int, alpha : float, A : &float, lda : int, B : &float, ldb : int, 
	           beta : float, C : &float, ldc : int)
	
	var TB = [&float](stdlib.malloc(K * N * sizeof(float)))
	var TA = A
	for kk = 0,K,NK do
		for nn = 0,N,NB do
			for k = kk,kk+NK do
				for n = nn,nn+NB do
					if BLOCKB then
						TB[nn * K + NB*(kk + n - nn) + k - kk] = B[k*ldb + n]
					else 
						TB[n*K + k] = B[k*ldb + n]
					end
				end
			end
		end
	end

	if BLOCKA then
		TA = [&float](stdlib.malloc(M * K * sizeof(float)))
		for mm = 0,M,NB do
			for kk = 0,K,NK do
				for m = mm,mm+NB do
					for k = kk,kk+NK do
						TA[mm * K + NB*(kk + m - mm) + k - kk] = A[m*lda + k]
					end
				end
			end
		end
	end

	for mm = 0,M,NB do
		for nn = 0, N,NB do
			for kk = 0, K, NK do
				for m = mm,mm+NB,AR do
					for n = nn,nn+NB,BR do
						blockregisters(C,TA,TB,K,lda,ldc,mm,nn,kk,m,n)
					end
				end
			end
		end
	end
	stdlib.free(TB)
	if BLOCKA then
		stdlib.free(TA)
	end
end

my_sgemm:compile()
my_sgemm:printpretty()

terralib.saveobj("my_sgemm.o", {my_sgemm = my_sgemm})