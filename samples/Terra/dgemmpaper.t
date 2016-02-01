
function symmat(name,I,...)
  if not I then return symbol(name) end
  local r = {}
  for i = 0,I-1 do
    r[i] = symmat(name..tostring(i),...)
  end
  return r
end
prefetch = terralib.intrinsic("llvm.prefetch",{&opaque,int,int,int} -> {})


function genkernel(NB, RM, RN, V,alpha)
  local A,B,C = symbol("A"),symbol("B"),symbol("C")
  local mm,nn = symbol("mn"),symbol("nn")
  local lda,ldb,ldc = symbol("lda"),symbol("ldb"),symbol("ldc")
  local a,b = symmat("a",RM), symmat("b",RN)
  local c,caddr = symmat("c",RM,RN), symmat("caddr",RM,RN)
  local k = symbol("k")
  local loadc,storec = terralib.newlist(),terralib.newlist()
  local VT = vector(double,V)
  local VP = &VT
  for m = 0, RM-1 do for n = 0, RN-1 do
      loadc:insert(quote
        var [caddr[m][n]] = C + m*ldc + n*V
        var [c[m][n]] = 
          alpha * @VP([caddr[m][n]])
      end)
      storec:insert(quote
        @VP([caddr[m][n]]) = [c[m][n]]
      end)
  end end
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
  for m = 0, RM-1 do for n = 0, RN-1 do
      calcc:insert(quote
        [c[m][n]] = [c[m][n]] + [a[m]] * [b[n]]
      end) 
  end end
  return terra([A] : &double, [B] : &double, [C] : &double,
               [lda] : int64,[ldb] : int64,[ldc] : int64)
    for [mm] = 0, NB, RM do
      for [nn] = 0, NB, RN*V do
        [loadc];
        for [k] = 0, NB do
          prefetch(B + 4*ldb,0,3,1);
          [calcc];
          B,A = B + ldb,A + 1
        end
        [storec];
        A,B,C = A - NB,B - ldb*NB + RN*V,C + RN*V
      end
      A,B,C = A + lda*RM, B - NB, C + RM * ldb - NB
    end
  end
end

local a = genkernel(40,4,2,8,1)
a:compile()
a:printpretty()

terra short_saxpy(a : float, 
      x : vector(float,4), y : vector(float,4))
    return a*x + y
end
short_saxpy:printpretty()