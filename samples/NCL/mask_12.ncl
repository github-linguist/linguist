;----------------------------------------------------------------------
; mask_12.ncl
;
; Concepts illustrated:
;   - Using a worldwide shapefile to create a land/ocean mask
;   - Masking a data array based on a geographical area
;   - Attaching shapefile polylines to a map plot
;   - Attaching lat/lon points to a map using gsn_coordinates
;----------------------------------------------------------------------
; Downloaded GSHHS shapefiles from:
;
;  http://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/
;
; Used the "coarsest" one: "GSHHS_shp/c/GSHHS_c_L1.shp".
;----------------------------------------------------------------------

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "./shapefile_mask_data.ncl"

;----------------------------------------------------------------------
; Main code
;----------------------------------------------------------------------
begin
  WRITE_MASK = True
  DEBUG      = False

;---Read data to plot and mask
  dir        = "$NCARG_ROOT/lib/ncarg/data/cdf/"
  cdf_prefix = "uv300"
  cdf_file   = dir + cdf_prefix + ".nc"
  fin        = addfile(cdf_file,"r")
  u          = fin->U(1,:,:)
;
; Create a mask array the same size as "u", using
; lat/lon data read off a shapefile.
;
  shpfile   = "GSHHS_shp/c/GSHHS_c_L1.shp"
  opt             = True
  opt@return_mask = True

  land_mask = shapefile_mask_data(u,shpfile,opt)

;---Mask "u" against land and ocean.
  u_land_mask  = where(land_mask.eq.1,u,u@_FillValue)
  u_ocean_mask = where(land_mask.eq.0,u,u@_FillValue)
  copy_VarMeta(u,u_land_mask)
  copy_VarMeta(u,u_ocean_mask)

;---Start the graphics
  wks = gsn_open_wks("ps","mask")

  res                       = True

  res@gsnMaximize           = True           ; maximize plot in frame
  res@gsnDraw               = False          ; don't draw plot yet
  res@gsnFrame              = False          ; don't advance frame yet

  res@cnFillOn              = True
  res@cnLineLabelsOn        = False
  res@cnLinesOn             = False

;---Make sure both plots have same contour levels
  mnmxint                   = nice_mnmxintvl(min(u),max(u),25,False)
  res@cnLevelSelectionMode  = "ManualLevels"
  res@cnMinLevelValF        = mnmxint(0)
  res@cnMaxLevelValF        = mnmxint(1)
  res@cnLevelSpacingF       = mnmxint(2)

  res@lbLabelBarOn          = False
  res@gsnAddCyclic          = False

  res@mpFillOn              = False
  res@mpOutlineOn           = False

  res@gsnRightString        = ""
  res@gsnLeftString         = ""

;---Create plot of original data and attach shapefile outlines
  res@tiMainString         = "Original data with shapefile outlines"
  map_data = gsn_csm_contour_map(wks,u,res)
  dum1     = gsn_add_shapefile_polylines(wks,map_data,shpfile,False)

;---Create plots of masked data
  res@tiMainString         = "Original data masked against land"
  map_land_mask  = gsn_csm_contour_map(wks,u_land_mask,res)
  res@tiMainString         = "Original data masked against ocean"
  map_ocean_mask = gsn_csm_contour_map(wks,u_ocean_mask,res)

  if(DEBUG) then
    mkres                 = True
;    mkres@gsMarkerSizeF   = 0.007
    mkres@gsnCoordsAttach = True
    gsn_coordinates(wks,map_data,u,mkres)
    mkres@gsnCoordsNonMissingColor = "yellow"
    mkres@gsnCoordsMissingColor    = "black"
    gsn_coordinates(wks,map_land_mask,u_land_mask,mkres)
    gsn_coordinates(wks,map_ocean_mask,u_ocean_mask,mkres)
  end if

;---Add shapefile outlines
  dum2 = gsn_add_shapefile_polylines(wks,map_land_mask,shpfile,False)
  dum3 = gsn_add_shapefile_polylines(wks,map_ocean_mask,shpfile,False)

;---Draw all three plots on one page
  pres                  = True
  pres@gsnMaximize      = True
  pres@gsnPanelLabelBar = True
  gsn_panel(wks,(/map_data,map_land_mask,map_ocean_mask/),(/3,1/),pres)

  if(WRITE_MASK) then
    delete(fin)     ; Close file before we open again.
;
; Make copy of file so we don't overwrite original.
; This is not necessary, but it's safer.
;
    new_cdf_file = cdf_prefix + "_with_mask.nc"
    system("/bin/cp " + cdf_file + " " + new_cdf_file)
    finout = addfile(new_cdf_file,"w")
    filevardef(finout, "land_mask", typeof(land_mask), (/ "lat", "lon" /) )
    finout->land_mask = (/land_mask/)
  end if
end

