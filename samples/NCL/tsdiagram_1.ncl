; Read potential temp (TEMP), salinity (SALT)
; Compute potential density (PD) for specified range PD(t,s)
; (use ncl function based on Yeager's algorithm for rho computation)
; Assumes annual and zonally avgeraged input data set (i.e, one time slice)
; Used K.Lindsay's "za" for zonal avg -- already binned into basins
; Plots temp vs salt (scatter plot), pd overlay

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"  

begin
; ================================>  ; PARAMETERS
  case    = "PHC2_gx1v3"
  ocnfile = "za_PHC2_T_S_gx1v3.nc"

  depth_min = 14895.82   ; in cm, depth of first layer to be included 
  depth_max =  537499.9
;
; plot limits
;
  smincn = 32.5 
  smaxcn = 37.0
  tmincn = -2.
  tmaxcn = 22.
;
; Choose basin index
;
; 0 = global  1 = southern ocean 2 = pacific 3 = indian 6 = atlantic 
; 8 = labrador 9 = GIN 10 = arctic
;
  bi = 2 

;=====> basin check 

  if(bi.lt.0.or.bi.gt.10) then
    print("basin index "+ bi + " not supported")
    exit
  end if

  if(bi.eq.0) then
    basin = "Global"
    blab = "global"
  end if
  if(bi.eq.1) then
      basin = "Southern Ocean"
      blab = "so"
  end if
  if(bi.eq.2) then
    basin = "Pacific Ocean"
    blab = "pacific"
  end if
  if(bi.eq.3) then
    basin = "Indian Ocean"
    blab = "indian"
  end if
  if(bi.eq.6) then
    basin = "Atlantic Ocean"
    blab = "atlanticn"
  end if
  if(bi.eq.8) then
    basin = "Labrador Sea"
    blab = "lab"
  end if
  if(bi.eq.9) then
    basin = "GIN Sea"
    blab = "gin"
  end if
  if(bi.eq.10) then
    basin = "Arctic Ocean"
    blab = "arctic"
  end if

;=====> initial resource settings

  wks = gsn_open_wks("ps","tsdiagram")    ; Open a Postscript file

;===== data 
  focn = addfile(ocnfile, "r")
  salt = focn->SALT(0,:,{depth_min:depth_max},:)   ;(basins, z_t, lat_t)
  temp = focn->TEMP(0,:,{depth_min:depth_max},:)

;====section out choice basin
  temp_ba = temp(bi,:,:)
  salt_ba = salt(bi,:,:)

;===== put into scatter array format
  tdata_ba = ndtooned(temp_ba)
  sdata_ba = ndtooned(salt_ba)

  ydata = tdata_ba
  xdata = sdata_ba

;============== compute potenial density (PD), using rho_mwjf
;
; for potential density, depth = 0. (i.e. density as if brought to surface)
;
;===========================================================================
; WARNING: T-S diagrams use POTENTIAL DENSITY... if set depth to something
; other then 0, then you will be plotting density contours computed for the
; specified depth layer.
;===========================================================================

  depth = 0.  ;in meters
  tspan = fspan(tmincn,tmaxcn,51)
  sspan = fspan(smincn,smaxcn,51)

  ; the more points the better... using Yeager's numbers

  t_range = conform_dims((/51,51/),tspan,0)
  s_range = conform_dims((/51,51/),sspan,1)

  pd = rho_mwjf(t_range,s_range,depth)

  pd!0    = "temp"
  pd!1    = "salt"
  pd&temp = tspan
  pd&salt = sspan
  pd      = 1000.*(pd-1.)        ; Put into kg/m3 pot den units

;  printVarSummary(pd)
;  printVarInfo(pd,"rho_mwjf")

;=================Graphics

;--- scatter plot
  res                     = True
  res@gsnMaximize         = True
  res@gsnDraw             = False
  res@gsnFrame            = False

  res@xyMarkLineModes     = "Markers"
  res@xyMarkers           = 16
  res@xyMarkerColors      = "black"
  res@pmLegendDisplayMode = "Never"
  res@txFontHeightF       = 0.01
  res@tiMainString        = case + " ANN AVG:  T-S Diagram"
  res@tiXAxisString       = salt@units
  res@tiXAxisFontHeightF  = 0.02
  res@tiYAxisString       = temp@units
  res@tiYAxisFontHeightF  = 0.02
  res@trXMinF             = smincn
  res@trXMaxF             = smaxcn
  res@trYMinF             = tmincn
  res@trYMaxF             = tmaxcn
  res@gsnRightString      = depth_min/100. + "-"+depth_max/100. +"m"
  res@gsnLeftString       = basin

  plot = gsn_csm_xy(wks,xdata,ydata,res)

;----- pd overlay
  resov                          = True
  resov@gsnDraw                  = False
  resov@gsnFrame                 = False
  resov@cnLevelSelectionMode     = "AutomaticLevels"
  resov@cnInfoLabelOn            = "False"
  resov@cnLineLabelPlacementMode = "Constant"
  resov@cnLineLabelFontHeightF     = ".02"
  
  plotpd = gsn_csm_contour(wks,pd,resov)
  overlay(plot,plotpd)

  draw(plot)
  frame(wks)

end
