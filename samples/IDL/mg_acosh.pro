; docformat = 'rst'

;+
; Inverse hyperbolic cosine. Uses the formula:
; 
; $$\text{acosh}(z) = \ln(z + \sqrt{z + 1} \sqrt{z - 1})$$
;
; :Examples:
;    The arc hyperbolic sine function looks like::
;
;       IDL> x = 2.5 * findgen(1000) / 999. + 1.
;       IDL> plot, x, mg_acosh(x), xstyle=1
;
;    This should look like:
;
;    .. image:: acosh.png
;
; :Returns:
;    float, double, complex, or double complex depending on the input
;
; :Params:
;    z : in, required, type=numeric
;       input
;-
function mg_acosh, z
  compile_opt strictarr
  
  return, alog(z + sqrt(z + 1) * sqrt(z - 1))
end