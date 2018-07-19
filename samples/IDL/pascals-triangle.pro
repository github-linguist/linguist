Pro Pascal, n
;n is the number of lines of the triangle to be displayed
 r=[1]
 print, r
  for i=0, (n-2) do begin
    pascalrow,r
  endfor
End

Pro PascalRow, r
  for i=0,(n_elements(r)-2) do begin
    r[i]=r[i]+r[i+1]
  endfor
r= [1, r]
print, r

End
