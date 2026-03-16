;----------------------------------------------------------------------
; cosmolib_1.ncl
;
; Concepts illustrated:
;   - Plotting COSMO model data from MeteoSwiss
;   - Plotting data from a rotated lat-lon grid
;   - Using functions from COSMOlib
;----------------------------------------------------------------------
; To learn more about COSMOlib, visit:
;   https://wiki.c2sm.ethz.ch/Wiki/VisNCLCosmoLibrary
;----------------------------------------------------------------------

begin

 ; open file
 cname = "lfff00000000c"
 ftype = "grb"
 cfile = addfile(cname+"."+ftype,"r")

 ; read topography and land-fraction
 topo = jmb_getvar(cfile, "HSURF", False)
 frland = jmb_getvar(cfile, "FR_LAND", False)

 ; geo-reference
 jmb_getgrid(cfile, cfile, topo, False)

 ; close file
 delete(cfile)

 ; set water points to -1.0 elevation
 topo = where(topo .lt. 0.0, 0.0, topo)
 topo = where(frland .gt. 0.5, topo, -1.0)

 ; rename topography (for correct plot title)
 topo@long_name = "Topography"

 ; open graphic port
 ptype = "png"                                 ; send graphics to PNG file
 wks = gsn_open_wks(ptype,"cosmolib")

 ; set color table
 rc = jmb_set_ct(wks, "topo_15lev", False)

 ; plot map
 res                        = True
 res@gsnMaximize            = True             ; maxmize plot in frame
 mp = jmb_map(wks, topo, res)
 delete(res)

 ; plot contour plot
 res                        = rc
 cn = jmb_contour(wks, topo, res)
 delete(res)

 ; draw plot
 pl = jmb_overlays(wks, (/mp,cn/), False)

 ; cleanup
 delete(wks)

end

