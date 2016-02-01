local number = double
local alignment = 8
local dotune = false

function symmat(name,I,...)
	if not I then return symbol(name) end
	local r = {}
	for i = 0,I-1 do
		r[i] = symmat(name..tostring(i),...)
	end
	return r
end

local function isinteger(x) return math.floor(x) == x end

llvmprefetch = terralib.intrinsic("llvm.prefetch",{&opaque,int,int,int} -> {})
local function unalignedload(addr)
	return `terralib.attrload(addr, { align = alignment })
end
local function unalignedstore(addr,v)
	return `terralib.attrstore(addr,v, { align = alignment })
end

unalignedload,unalignedstore = macro(unalignedload),macro(unalignedstore)

function genkernel(NB, RM, RN, V,alpha,boundary)

	local M,N,K, boundaryargs
	if boundary then
		M,N,K = symbol(int64,"M"),symbol(int64,"N"),symbol(int64,"K")
		boundaryargs = terralib.newlist({M,N,K})
	else
		boundaryargs = terralib.newlist()
		M,N,K = NB,NB,NB
	end

	local A,B,C,mm,nn,ld = symbol("A"),symbol("B"),symbol("C"),symbol("mn"),symbol("nn"),symbol("ld")
	local lda,ldb,ldc = symbol("lda"),symbol("ldb"),symbol("ldc")
	local a,b,c,caddr = symmat("a",RM), symmat("b",RN), symmat("c",RM,RN), symmat("caddr",RM,RN)
	local k = symbol("k")
	
	local loadc,storec = terralib.newlist(),terralib.newlist()
	local VT = vector(number,V)
	local VP = &VT
	for m = 0, RM-1 do
		for n = 0, RN-1 do
			loadc:insert(quote
				var [caddr[m][n]] = C + m*ldc + n*V
				var [c[m][n]] = alpha * unalignedload(VP([caddr[m][n]]))
			end)
			storec:insert(quote
				unalignedstore(VP([caddr[m][n]]),[c[m][n]])
			end)
		end
	end

	local calcc = terralib.newlist()
	
	for n = 0, RN-1 do
		calcc:insert(quote
			var [b[n]] = unalignedload(VP(&B[n*V]))
		end)
	end
	for m = 0, RM-1 do
		calcc:insert(quote
			var [a[m]] = VT(A[m*lda])
		end)
	end
	for m = 0, RM-1 do 
		for n = 0, RN-1 do
			calcc:insert(quote
				[c[m][n]] = [c[m][n]] + [a[m]] * [b[n]]
			end)
		end
	end
	
	local result = terra([A] : &number, [B] : &number, [C] : &number, [lda] : int64,[ldb] : int64,[ldc] : int64,[boundaryargs])
		for [mm] = 0, M, RM do
			for [nn] = 0, N,RN*V do
				[loadc];
				for [k] = 0, K do
					llvmprefetch(B + 4*ldb,0,3,1);
					[calcc];
					B = B + ldb
					A = A + 1
				end
				[storec];
				A = A - K
				B = B - ldb*K + RN*V
				C = C + RN*V
			end
			C = C + RM * ldb - N
			B = B - N
			A = A + lda*RM
		end
	end
	return result
end

local stdlib = terralib.includec("stdlib.h")
local IO = terralib.includec("stdio.h")

local terra min(a : int, b : int)
	return terralib.select(a < b, a, b)
end
function blockedloop(N,M,K,blocksizes,bodyfn)
  local function generatelevel(n,ii,jj,kk,bb0,bb1,bb2)
    if n > #blocksizes then
      return bodyfn(ii,jj,kk)
    end
    local blocksize = blocksizes[n]
    return quote for i = ii,min(ii+bb0,N),blocksize do
                   for j = jj,min(jj+bb1,M),blocksize do
                      for k = kk,min(kk+bb2,K),blocksize do
                        [ generatelevel(n+1,i,j,k,blocksize,blocksize,blocksize) ]
           end end end end
  end
  return generatelevel(1,0,0,0,N,M,K)
end

function generatedgemm(NB,NBF,RM,RN,V)
	if not isinteger(NB/(RN*V)) or not isinteger(NB/RM) then
		return false
	end

	local NB2 = NBF * NB
	local l1dgemm0 = genkernel(NB,RM,RN,V,0,false)
	local l1dgemm1 = genkernel(NB,RM,RN,V,1,false)
	local l1dgemm0b = genkernel(NB,1,1,1,0,true)
	local l1dgemm1b = genkernel(NB,1,1,1,1,true)

	return terra(gettime : {} -> double, M : int, N : int, K : int, alpha : number, A : &number, lda : int, B : &number, ldb : int, 
		           beta : number, C : &number, ldc : int)
		[ blockedloop(N,M,K,{NB2,NB},function(m,n,k) return quote
								var MM,NN,KK = min(M-m,NB),min(N-n,NB),min(K-k,NB)
								var isboundary = MM < NB or NN < NB or KK < NB
								var AA,BB,CC = A + (m*lda + k),B + (k*ldb + n),C + (m*ldc + n)
								if k == 0 then
									if isboundary then
										--IO.printf("b0 %d %d %d\n",MM,NN,KK)
										l1dgemm0b(AA,BB,CC,lda,ldb,ldc,MM,NN,KK)

										--IO.printf("be %d %d %d\n",MM,NN,KK)
									else
										l1dgemm0(AA,BB,CC,lda,ldb,ldc)
									end
								else
									if isboundary then

										--IO.printf("b %d %d %d\n",MM,NN,KK)
										l1dgemm1b(AA,BB,CC,lda,ldb,ldc,MM,NN,KK)

										--IO.printf("be %d %d %d\n",MM,NN,KK)
									else
										l1dgemm1(AA,BB,CC,lda,ldb,ldc)
									end
								end
		end end) ]
	end
end

local blocksizes = {16,24,32,40,48,56,64}
local regblocks = {1,2,4}
local vectors = {1,2,4,8,16}
--local best = { gflops = 0, b = 56, rm = 4, rn = 1, v = 8 }
local best = { gflops = 0, b = 40, rm = 4, rn = 2, v = 4 }
--local best = { gflops = 0, b = 40, rm = 1, rn = 1, v = 1 }

if dotune then
	local tunefor = 1024
	local harness = require("lib/matrixtestharness")
	for _,b in ipairs(blocksizes) do
		for _,rm in ipairs(regblocks) do
			for _,rn in ipairs(regblocks) do
				for _,v in ipairs(vectors) do
					local my_dgemm = generatedgemm(b,5,rm,rn,v)
					if my_dgemm then
						print(b,rm,rn,v)
						my_dgemm:compile()
						local i = math.floor(tunefor / b) * b
						local avg = 0
						local ctyp
						local s, times = harness.timefunctions(tostring(number),i,i,i,function(M,K,N,A,B,C)
							my_dgemm(nil,M,N,K,1.0,A,K,B,N,0.0,C,N)
						end)
						if not s then
							print("<error>")
							break
						end
						print(i,unpack(times))
						local avg = times[1]	
						if  best.gflops < avg then
							best = { gflops = avg, b = b, rm = rm, rn = rn, v = v }
							terralib.tree.printraw(best)
						end
					end
				end
			end
		end
	end
end
local my_dgemm = generatedgemm(best.b, 5, best.rm, best.rn, best.v)
if number == double then
	terralib.saveobj("my_dgemm.o", { my_dgemm = my_dgemm })
else
	terralib.saveobj("my_sgemm.o", { my_sgemm = my_dgemm })
end