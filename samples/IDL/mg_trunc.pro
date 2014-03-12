; docformat = 'rst'

;+
; Truncate argument towards 0.0, i.e., takes the `FLOOR` of positive values
; and the `CEIL` of negative values.
;
; :Examples:
;   Try the main-level program at the end of this file. It does::
;
;      IDL> print, mg_trunc([1.2, -1.2, 0.0])
;                 1          -1           0
;      IDL> print, floor([1.2, -1.2, 0.0])
;                 1          -2           0
;      IDL> print, ceil([1.2, -1.2, 0.0])
;                 2          -1           0
;
; :Returns:
;    array of same type as argument
;
; :Params:
;    x : in, required, type=float/double
;       array containing values to truncate
;-
function mg_trunc, x
  compile_opt strictarr
  
  result = ceil(x)
  posInd = where(x gt 0, nposInd)
  
  if (nposInd gt 0L) then begin
    result[posInd] = floor(x[posInd])
  endif
  
  return, result
end


; main-level example program

print, mg_trunc([1.2, -1.2, 0.0])

end
