;================================================;
;  maponly_1.ncl
;================================================;
;
; Concepts illustrated:
;   - Drawing a default cylindrical equidistant map
;
;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;
begin

  wks  = gsn_open_wks("ps","maponly")     ; open a ps file
  plot = gsn_csm_map_ce(wks,False)        ; draw global map

  end
