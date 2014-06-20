; docformat = 'rst'

;+
; Find the greatest common denominator (GCD) for two positive integers.
; 
; :Returns:
;    integer
;
; :Params:
;    a : in, required, type=integer
;       first integer
;    b : in, required, type=integer
;       second integer
;-
function mg_gcd, a, b
  compile_opt strictarr
  on_error, 2
  
  if (n_params() ne 2) then message, 'incorrect number of arguments'
  if (~mg_isinteger(a) || ~mg_isinteger(b)) then begin
    message, 'integer arguments required'
  endif
  
  _a = abs(a)
  _b = abs(b)
  minArg = _a < _b
  maxArg = _a > _b
  
  if (minArg eq 0) then return, maxArg
  
  remainder = maxArg mod minArg
  if (remainder eq 0) then return, minArg
  
  return, mg_gcd(minArg, remainder)
end
