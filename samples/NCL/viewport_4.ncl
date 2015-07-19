; ***********************************************
; viewport_4.ncl
;
; Concepts illustrated:
;   - Drawing an XY plot with multiple curves
;   - Using drawNDCGrid to draw a nicely labeled NDC grid
;   - Changing the size/shape of an XY plot using viewport resources
;   - Drawing two XY plots on the same page using viewport resources
;   - Drawing polylines, polymarkers, and text in NDC space
;   - Using "getvalues" to retrieve resource values
;   - Maximizing plots after they've been created
; ***********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

;********************************************************************
; Draw a box around the viewport of the given object..
;********************************************************************
procedure draw_vp_box(wks,plot)
local vpx, vpy, vpw, vph, xbox, ybox, lnres, mkres, txres
begin

; Retrieve the viewport values of the drawable object.
  getvalues plot
    "vpXF"      : vpx
    "vpYF"      : vpy
    "vpWidthF"  : vpw
    "vpHeightF" : vph
  end getvalues

; Set up some marker resources.
  mkres                  = True
  mkres@gsMarkerIndex    = 16     ; filled dot
  mkres@gsMarkerSizeF    = 0.02   ; larger than default
  mkres@gsMarkerColor    = "Red"

; Draw a single marker at the vpXF/vpYF location.
  gsn_polymarker_ndc(wks,vpx,vpy,mkres)


; Set up some text resources.
  txres                       = True  
  txres@txJust                = "BottomLeft"
  txres@txFontHeightF         = 0.018
  txres@txFontColor           = "Blue"
  txres@txBackgroundFillColor = "white"

  gsn_text_ndc(wks,"(vpXF="+vpx+", vpYF="+vpy+")",vpx,vpy+0.02,txres)
; Set up some line resources.
  lnres                  = True
  lnres@gsLineColor      = "Red"       ; line color
  lnres@gsLineThicknessF = 2.0         ; 3.5 times as thick

; Draw lines indicating the width and height
  xline = (/vpx,     vpx+vpw/)
  yline = (/vpy-0.05,vpy-0.05/)
  gsn_polyline_ndc(wks,xline,yline,lnres)

  xline = (/vpx+0.05,vpx+0.05/)
  yline = (/vpy,vpy-vph/)
  gsn_polyline_ndc(wks,xline,yline,lnres)

  txres@txJust                = "CenterCenter"
  gsn_text_ndc(wks,"vpWidthF = " + vpw,vpx+vpw/2.,vpy-0.05,txres)

  txres@txAngleF              = 90.
  gsn_text_ndc(wks,"vpHeightF = " + vph,vpx+0.05,vpy-vph/2.,txres)
end

;********************************************************************
; Main code
;********************************************************************
begin
;************************************************
; read in data
;************************************************
  f     = addfile ("$NCARG_ROOT/lib/ncarg/data/cdf/uv300.nc","r")
  u     = f->U                                    ; get u data
;************************************************
; plotting parameters
;************************************************
  wks   = gsn_open_wks ("ps","viewport")          ; open workstation
  
  res                  = True                     ; plot mods desired

  res@gsnFrame         = False                    ; don't advance frame yet

  res@vpWidthF         = 0.8                      ; set width and height
  res@vpHeightF        = 0.3
  
; First plot  
  res@tiMainString     = "Plot 1"

  res@vpXF             = 0.15
  res@vpYF             = 0.9        ; Higher on the page

  plot1  = gsn_csm_xy (wks,u&lat,u(0,:,{82}),res) ; create plot
  
; Second plot  
  res@tiMainString     = "Plot 2"

  res@vpXF             = 0.15       ; Same X location as first plot
  res@vpYF             = 0.4        ; Lower on the page

  plot2  = gsn_csm_xy (wks,u&lat,u(0,:,{3}),res) ; create plot

; Advance the frame
  frame(wks)
  
; Now draw the two plots with illustrations.

  drawNDCGrid(wks)   ; Draw helpful grid lines showing NDC square.

  draw(plot1)        ; Draw the two plots
  draw(plot2)

  draw_vp_box(wks,plot1)  ; Draw boxes around the two viewports.
  draw_vp_box(wks,plot2)

  frame(wks)              ; Advance the frame.


;
; Uncomment the next two lines if you want to maximize these plots for
; PS or PDF output.
;
;  psres = True                                                               
;  maximize_output(wks,psres)  ; calls draw and frame for you 
end
