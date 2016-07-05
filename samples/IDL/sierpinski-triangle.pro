pro sierp,n
  s = (t = bytarr(3+2^(n+1))+32b)
  t[2^n+1] = 42b
  for lines = 1,2^n do begin
        print,string( (s = t) )
        for i=1,n_elements(t)-2 do if s[i-1] eq s[i+1] then t[i]=32b else t[i]=42b
  end
end
