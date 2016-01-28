
function symmat(name,I,...)
	if not I then return symbol(name) end
	local r = {}
	for i = 0,I-1 do
		r[i] = symmat(name..tostring(i),...)
	end
	return r
end


function genkernel(NB, RM, RN, V,alpha)

	local terra vecload(data : &double, idx : int)
		var addr = &data[idx]
		return @addr:as(&vector(double,V))
	end
	local terra vecstore(data : &double, idx : int, v : vector(double,V))
		var addr = &data[idx]
		@addr:as(&vector(double,V)) = v
	end

	local A,B,C,mm,nn = symbol("A"),symbol("B"),symbol("C"),symbol("mn"),symbol("nn")
	local lda,ldb,ldc = NB,NB,NB
	local a,b,c,caddr = symmat("a",RM), symmat("b",RN), symmat("c",RM,RN), symmat("caddr",RM,RN)
	local k = symbol("k")
	
	local loadc,storec = terralib.newlist(),terralib.newlist()
	local VT = vector(double,V)
	local VP = &VT
	for m = 0, RM-1 do
		for n = 0, RN-1 do
			loadc:insert(quote
				var [caddr[m][n]] = C + (mm+m)*ldc + nn + n*V
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
			var [b[n]] = @VP(&B[k*ldb + nn + n*V])
		end)
	end
	for m = 0, RM-1 do
		calcc:insert(quote
			var [a[m]] = VT(A[(mm+m)*lda + k])
		end)
	end
	for m = 0, RM-1 do 
		for n = 0, RN-1 do
			calcc:insert(quote
				[c[m][n]] = [c[m][n]] + [a[m]] * [b[n]]
			end)
		end
	end
	
	
	return terra([A] : &double, [B] : &double, [C] : &double)
		for [mm] = 0, NB, RM do
			for [nn] = 0, NB,RN*V do
				[loadc];
				for [k] = 0, NB do
					[calcc];
				end
				[storec];
			end
		end
	end
end

local NB = 48
local NB2 = 8 * NB

local V = 4

l1dgemm0 = genkernel(NB,2,4,V,0)
l1dgemm1 = genkernel(NB,2,4,V,1)

terra min(a : int, b : int)
	return terralib.select(a < b, a, b)
end

local stdlib = terralib.includec("stdlib.h")
local IO = terralib.includec("stdio.h")
local VP = &vector(double,V)

terra my_dgemm(gettime : {} -> double, M : int, N : int, K : int, alpha : double, A : &double, lda : int, B : &double, ldb : int, 
	           beta : double, C : &double, ldc : int)
	
	var AA = [&double](stdlib.malloc(sizeof(double)*M*K))
	var BB = [&double](stdlib.malloc(sizeof(double)*K*N))
	var CC = [&double](stdlib.malloc(sizeof(double)*M*N))

	var i = 0
	for mm = 0,M,NB do
		for kk = 0,K,NB do
			for m = mm,mm+NB do
				for k = kk,kk+NB,V do
					@VP(&AA[i]) = @VP(&A[m*lda + k])
					i = i + V
				end
			end
		end
	end
	i = 0 
	for kk = 0,K,NB do
		for nn = 0,N,NB do
			for k = kk,kk+NB do
				for n = nn,nn+NB,V do
					@VP(&BB[i]) = @VP(&B[k*ldb + n])
					i = i + V
				end
			end
		end
	end	

	for mm = 0,M,NB2 do
		for nn = 0,N,NB2 do
			for kk = 0,K, NB2 do
				for m = mm,min(mm+NB2,M),NB do
					for n = nn,min(nn+NB2,N),NB do
						for k = kk,min(kk+NB2,K),NB do
							--IO.printf("%d %d starting at %d\n",m,k,m*lda + NB*k)
							if k == 0 then
								l1dgemm0(AA + (m*lda + NB*k),
							         	 BB + (k*ldb + NB*n),
							             CC + (m*ldc + NB*n))
							else
								l1dgemm1(AA + (m*lda + NB*k),
							         	 BB + (k*ldb + NB*n),
							             CC + (m*ldc + NB*n))
							end
						end
					end
				end
			end
		end
	end

	i = 0
	for mm = 0,M,NB do
		for nn = 0,N,NB do
			for m = mm,mm+NB do
				for n = nn,nn+NB,V do
					@VP(&C[m*ldc + n]) = @VP(&CC[i])
					i = i + V
				end
			end
		end
	end	

	stdlib.free(AA)
	stdlib.free(BB)
	stdlib.free(CC)
end

terralib.saveobj("my_dgemm.o", { my_dgemm = my_dgemm })