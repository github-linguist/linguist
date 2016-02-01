
function symmat(name,I,...)
	if not I then return symbol(name) end
	local r = {}
	for i = 0,I-1 do
		r[i] = symmat(name..tostring(i),...)
	end
	return r
end


llvmprefetch = terralib.intrinsic("llvm.prefetch",{&opaque,int,int,int} -> {})



function genkernel(NB, RM, RN, V,alpha)

	local A,B,C,mm,nn,ld = symbol("A"),symbol("B"),symbol("C"),symbol("mn"),symbol("nn"),symbol("ld")
	local lda,ldb,ldc = ld,ld,ld
	local a,b,c,caddr = symmat("a",RM), symmat("b",RN), symmat("c",RM,RN), symmat("caddr",RM,RN)
	local k = symbol("k")
	
	local loadc,storec = terralib.newlist(),terralib.newlist()
	local VT = vector(float,V)
	local VP = &VT
	for m = 0, RM-1 do
		for n = 0, RN-1 do
			loadc:insert(quote
				var [caddr[m][n]] = C + m*ldc + n*V
				var [c[m][n]] = alpha * @VP([caddr[m][n]])
			end)
			storec:insert(quote
				@VP([caddr[m][n]]) = [c[m][n]]
			end)
		end
	end

	local calcc = terralib.newlist()
	
	for n = 0, RN-1 do
		calcc:insert(quote
			var [b[n]] = @VP(&B[n*V])
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
	
	
	return terra([A] : &float, [B] : &float, [C] : &float, [ld] : int64)
		for [mm] = 0, NB, RM do
			for [nn] = 0, NB,RN*V do
				[loadc];
				for [k] = 0, NB do
					llvmprefetch(B + 4*ldb,0,3,1);
					[calcc];
					B = B + ldb
					A = A + 1
				end
				[storec];
				A = A - NB
				C = C + RN*V
				B = B - ldb*NB + RN*V
			end
			C = C + RM * ldb - NB
			B = B - NB
			A = A + lda*RM
		end
	end
end

local NB = 48
local NB2 = 5 * NB

local V = 1

l1dgemm0 = genkernel(NB,1,1,V,0)
l1dgemm1 = genkernel(NB,1,1,V,1)

terra min(a : int, b : int)
	return terralib.select(a < b, a, b)
end

local stdlib = terralib.includec("stdlib.h")
local IO = terralib.includec("stdio.h")

terra my_sgemm(gettime : {} -> double, M : int, N : int, K : int, alpha : float, A : &float, lda : int, B : &float, ldb : int, 
	           beta : float, C : &float, ldc : int)
	

	for mm = 0,M,NB2 do
		for nn = 0,N,NB2 do
			for kk = 0,K, NB2 do
				for m = mm,min(mm+NB2,M),NB do
					for n = nn,min(nn+NB2,N),NB do
						for k = kk,min(kk+NB2,K),NB do
							--IO.printf("%d %d starting at %d\n",m,k,m*lda + NB*k)
							if k == 0 then
								l1dgemm0(A + (m*lda + k),
							         	 B + (k*ldb + n),
							             C + (m*ldc + n),lda)
							else
								l1dgemm1(A + (m*lda + k),
							         	 B + (k*ldb + n),
							             C + (m*ldc + n),lda)
							end
						end
					end
				end
			end
		end
	end
end

terralib.saveobj("my_sgemm.o", { my_sgemm = my_sgemm })