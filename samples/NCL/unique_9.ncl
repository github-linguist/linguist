;************************************
; unique_9.ncl
;
; Concepts illustrated:
;   - Drawing raster contours over a map
;   - Creating a topography plot using raster contours
;   - Reading data from binary files
;   - Manually creating lat/lon coordinate arrays
;   - Customizing a labelbar for a contour plot
;************************************
; This example generates a topo map over
; the area of Trinidad, Colorado.
;************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

 wks = gsn_open_wks("ps","unique")

;----------------- read the west binary data -------------------------
 binfile = "trinidad-w.bin"

 quad_name = fbinrecread(binfile,0,60,"character")

 map_cornersW = fbinrecread(binfile,1,4,"double")

 lonW = fbinrecread(binfile,2,(/1201/),"double")

 latW = fbinrecread(binfile,3,(/1201/),"double")

 minmax_elevW = fbinrecread(binfile,4,2,"double")

 tmpW = fbinrecread(binfile,5,(/1201,1201/),"integer")
 
;----------------- read the east binary data -------------------------
 binfile = "trinidad-e.bin"

 quad_name = fbinrecread(binfile,0,60,"character")

 map_cornersE = fbinrecread(binfile,1,4,"double")

 lonE = fbinrecread(binfile,2,(/1201/),"double")

 latE = fbinrecread(binfile,3,(/1201/),"double")

 minmax_elevE = fbinrecread(binfile,4,2,"double")

 tmpE = fbinrecread(binfile,5,(/1201,1201/),"integer")
 
;----------------------------------------------------------------------
 min_elev = min((/minmax_elevW(0),minmax_elevE(0)/))*3.28
 max_elev = max((/minmax_elevW(1),minmax_elevE(1)/))*3.28

 lat           = new(1201,"double")
 lat           = latW
 lat!0         = "lat"
 lat&lat       = latW               ; same as latE
 lat@long_name = "latitude"
 lat@units     = "degrees_north"

 lon            = new(2401,"double")
 lon(0:1200)    = lonW
 lon(1201:2400) = lonE(1:1200)
 lon!0          = "lon"
 lon&lon        = lon
 lon@long_name  = "longitude"
 lon@units      = "degrees_east"

 data     = new((/1201,2401/),"float")  ; (lat,lon)
 data!0   = "lat"
 data&lat = lat
 data!1   = "lon"
 data&lon = lon
 data(:,0:1200)    = (/tmpW*3.28/)            ; convert to feet
 data(:,1201:2400) = (/tmpE(:,1:1200)*3.28/)  ; convert to feet
;-------------------------------------------------------------

;
; Define colormap.
;
  cmap = (/(/1.00, 1.00, 1.00/),(/0.00, 0.00, 0.00/), \
           (/0.51, 0.13, 0.94/),(/0.00, 0.00, 0.59/), \
           (/0.00, 0.00, 0.80/),(/0.25, 0.41, 0.88/), \
           (/0.12, 0.56, 1.00/),(/0.00, 0.75, 1.00/), \
           (/0.63, 0.82, 1.00/),(/0.82, 0.96, 1.00/), \
           (/1.00, 1.00, 0.78/),(/1.00, 0.88, 0.20/), \
           (/1.00, 0.67, 0.00/),(/1.00, 0.43, 0.00/), \
           (/1.00, 0.00, 0.00/),(/0.78, 0.00, 0.00/), \
           (/0.63, 0.14, 0.14/),(/1.00, 0.41, 0.70/)/)

 gsn_define_colormap(wks,cmap)

 res              = True
 res@gsnMaximize  = True
 res@gsnAddCyclic = False

; map plot resources
 res@mpFillOn              = False
 res@mpLimitMode           = "Corners"
 res@mpDataBaseVersion     = "Ncarg4_1"
 res@mpOutlineBoundarySets = "AllBoundaries"
 res@mpLeftCornerLonF      = map_cornersW(0) 
 res@mpLeftCornerLatF      = map_cornersW(1)
 res@mpRightCornerLonF     = map_cornersE(2)
 res@mpRightCornerLatF     = map_cornersE(3)

; contour resources
 res@cnFillOn             = True
 res@cnLinesOn            = False
 res@cnFillMode           = "RasterFill"
 res@cnLevelSelectionMode = "ExplicitLevels"
 res@cnLevels             = (/ 5000., 6000., 7000., 8000., 8500., 9000., \
                               9500.,10000.,10500.,11000.,11500.,12000., \
                              12500.,13000.,13500./)

; tickmark resources
 res@pmTickMarkDisplayMode = "Always"
 res@tmXBLabelFontHeightF   = 0.010

; labelbar resources
 res@pmLabelBarWidthF         = 0.60
 res@txFontHeightF            = 0.012
 res@lbTitleString            = "elevation above mean sea level (feet)"
 res@lbTitleFontHeightF       = 0.012
 res@lbLabelFontHeightF       = 0.008
 res@lbTitleOffsetF           = -0.27
 res@lbBoxMinorExtentF        = 0.15
 res@pmLabelBarOrthogonalPosF = -.05

; title resources
 res@tiMainString      = "USGS DEM TRINIDAD (1 x 2 degrees)" 
 res@tiMainOffsetYF    = -0.02    ; Move title down towards graphic.
 res@tiMainFontHeightF = 0.015
 res@gsnLeftString     = "Min Elevation: "+min_elev
 res@gsnRightString    = "Max Elevation: "+max_elev
 res@gsnCenterString   = "Scale 1:250,000"

 plot = gsn_csm_contour_map(wks,data,res)

end
