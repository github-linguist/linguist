load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl" 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl" 
;**************************************************************
; User Input
;***************************************************************
                                             ; INPUT
   diri   = "./"                             ; input directory
   fili   = "wv_LV3_MET08_20050102_12345678_L00013712E00013712.hdf"

   pltDir  = "./"                        ; directory for plot output
   sfx     = get_file_suffix(fili,1) 
  ;pltName = sfx@fBase                   ; output graphic name
   pltName = "hdf4sds"
   pltType = "ps"                                            

;***************************************************************
; End User Input
;***************************************************************

;***************************************************************
; Open SEVIRI L3 'wv' HDF file
;***************************************************************
; Note the rather unusual data format: flag *prepended* to data value
;***************************************************************
;     integer twc_lv3 ( fakeDim0, fakeDim1 )
;        long_name :    total water vapour column + flag
;        units :        fmmmm
;        format :       I4
;        valid_range :  ( 10000, 38000 )
;        _FillValue :   -99
;        legend_01 :    f = flag
;        legend_02 :    f = 1 averaged level 2 values
;        legend_03 :    f = 2 interpolated from averaged level 2 values
;        legend_04 :    f = 3 gaps filled with NVAP climatology
;        legend_05 :    mmmm = water vapour column in mm * 100. as integer
;        legend_06 :    Example: 11025 means: flag = 1, 10.25 mm water vapour column
;        min_lat :      -74.75
;        max_lat :      61.75
;        min_lon :      -75.25
;        max_lon :      75.25
;        dlat : 0.5
;        dlon : 0.5
;---------------------------------------------------------------

   f      = addfile (diri+fili, "r")
   ifx    = f->twc_lv3                  ; fmmmm (integer)                     
   printVarSummary(ifx)

   flag   = ifx/10000                   ; extract flag
   ix     = ifx - flag*10000            ; extract mmmm
   x      = ix*0.01                     ; scale

; create meta data for 'x'

   dimx   = dimsizes(x)
   nlat   = dimx(0)                     ; grid size x(nlat,mlon)
   mlon   = dimx(1)

   lat    = fspan(ifx@min_lat, ifx@max_lat, nlat)
   lat@units = "degrees_north"
   lon    = fspan(ifx@min_lon, ifx@max_lon, mlon)
   lon@units = "degrees_east"

   x!0    = "lat"
   x!1    = "lon"
   x&lat  =  lat
   x&lon  =  lon
   x@long_name = "SEVIRI: Total Water Vapor"
   x@units     = "mm"

   delete( [/ifx, ix/] )                ; no longer needed

;***************************************************************
; Create plot 
;***************************************************************
   wks    = gsn_open_wks(pltType, pltDir+pltName)

   plot   = new (2, "graphic")
   
   res                      = True     ; plot mods desired
   res@gsnAddCyclic         = False    ; data noty global
   res@gsnDraw              = False
   res@gsnFrame             = False
     
   res@cnFillOn             = True     ; turn on color fill
   res@cnLinesOn            = False    ; turn of contour lines
   res@cnFillMode           = "RasterFill" ; Raster Mode
   res@cnLinesOn            =  False       ; Turn off contour lines
   res@cnLineLabelsOn       =  False       ; Turn off contour lines
   res@cnMissingValFillColor= "background" ; "foreground"
   
   res@mpCenterLonF         = 0.5*(min(x&lon) + max(x&lon))
   res@mpMinLatF            = min(x&lat)
   res@mpMaxLatF            = max(x&lat)
   res@mpMinLonF            = min(x&lon)
   res@mpMaxLonF            = max(x&lon)

  ;res@lbOrientation        = "Vertical"

   plot(0) = gsn_csm_contour_map_ce(wks,x, res)

; plot flag

   copy_VarCoords(x, flag)
   flag@long_name = "Flag"
   flag@units     = "1=avg(L2), 2=int(L2), 3=NVAP"
   print(flag&lat+"   "+flag(:,{30}))

   res@cnLevelSelectionMode = "ManualLevels"     ; set manual contour levels
   res@cnMinLevelValF       =   2                ; set min contour level
   res@cnMaxLevelValF       =   3                ; one less than max
   res@cnLevelSpacingF      =   1                ; set contour spacing
 
   res@lbLabelStrings       = ispan(1,3,1)       ; 1, 2, 3
   res@lbLabelPosition      = "Center"           ; label position
   res@lbLabelAlignment     = "BoxCenters"
 
   res@gsnLeftString        = ""
   res@gsnRightString       = ""
   res@gsnCenterString      = "flag: 1=avg(L2), 2=int(L2), 3=NVAP"

   plot(1) = gsn_csm_contour_map_ce(wks,flag, res)

   resP                     = True                ; modify the panel plot
   resP@txString            = fili
   resP@gsnMaximize         = True 
   gsn_panel(wks,plot,(/1,2/),resP)               ; now draw as one plot
