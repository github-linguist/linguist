;*************************************************
; traj_3.ncl
;*************************************************
load          "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
external TRAJ "./particle.so"
;*************************************************
begin

  path  = "./data.asc"
  data  = asciiread(path,(/500,6/),"float")
;*************************************************
; some parameters
;*************************************************
  np = 1
  nq = 500
  ncor= 8
  xrot   = new((/np,nq/),float)
  yrot   = new((/np,nq/),float)
  xaxis  = new(ncor,float)
  yaxis  = new(ncor,float)
;**************************************************
; convert data into rotated format
;**************************************************
  TRAJ::particle(path,xrot,yrot,nq,np,xaxis,yaxis,ncor)
;**************************************************
; create plot
;**************************************************
  wks = gsn_open_wks("ps","traj")      ; Open an ps file

  xyres = True
  xyres@gsnFrame     = False      ; don't advance the frame
  xyres@gsnDraw      = False      ; don't draw indivdual plots
  xyres@tmXTBorderOn = False      ; don't draw top axis
  xyres@tmXBBorderOn = False      ; don't draw bottom axis
  xyres@tmYRBorderOn = False      ; don't draw right axis
  xyres@tmYLBorderOn = False      ; don't draw left axis
  xyres@tmXTOn       = False      ; don't draw top-axis tick marks 
  xyres@tmXBOn       = False      ; don't draw bottom-axis tick marks
  xyres@tmYROn       = False      ; don't draw right-axis tick marks
  xyres@tmYLOn       = False      ; don't draw left-axis tick marks

  xyres@xyLineColors      = (/"red"/)    ; set the line color to red
  xyres@xyLineThicknessF  = 4.0          ; 4 times the line thickness 

  xyres@trXMaxF      =  15000            ; choose range of axis even though
  xyres@trXMinF      = -10000            ; we don't see them
  xyres@trYMaxF      =  1000
  xyres@trYMinF      = -1000

  plot = gsn_xy(wks,xrot,yrot,xyres)     ; Draw trajectory
;**********************************************
; create arrays needed for the bounding box
;**********************************************
  a1 = new(5,float)
  b1 = new(5,float)
  a2 = new(5,float)
  b2 = new(5,float)
  a3 = new(2,float)
  b3 = new(2,float)
  a4 = new(2,float)
  b4 = new(2,float)
  a5 = new(2,float)
  b5 = new(2,float)
  a6 = new(2,float)
  b6 = new(2,float)
  a0 = new(2,float)
  b0 = new(2,float)
;**********************************************
; determine values of each bounding line from information
; returned from particle.f
;**********************************************
  a1(0:3) = xaxis(:3)             
  b1(0:3) = yaxis(:3)
  a1(4)   = xaxis(0)
  b1(4)   = yaxis(0)

  a2(0:3) = xaxis(4:)
  b2(0:3) = yaxis(4:) 
  a2(4)   = xaxis(4)
  b2(4)   = yaxis(4) 

  a3      = xaxis(0:4:4)
  b3      = yaxis(0:4:4) 
  a4      = xaxis(1:5:4)
  b4      = yaxis(1:5:4)
 
  a5      = xaxis(2:6:4)
  b5      = yaxis(2:6:4) 
  a6      = xaxis(3:7:4)
  b6      = yaxis(3:7:4) 

  a0(0)   = xaxis(3)
  b0(0)   = yaxis(3)
  a0(1)   = xrot(0,0)
  b0(1)   = yrot(0,0)
;*************************************************************** 
; create bounding box by drawing multiple xy plots on top of
; each other. each with their individual axis turned off.
;***************************************************************
  xyres@xyLineColors      =  (/"black"/)   ; line color 
  xyres@xyLineThicknessF  = 1.0            ; regular line thickness 

  bottom = gsn_xy(wks,a1,b1,xyres) ; Draw the bottom bounding box.
  top    = gsn_xy(wks,a2,b2,xyres) ; Draw the top bounding box.
  side1  = gsn_xy(wks,a3,b3,xyres) ; Draw a side line.
  side2  = gsn_xy(wks,a4,b4,xyres) ; Draw a side line.
  side3  = gsn_xy(wks,a5,b5,xyres) ; Draw a side line.
  side4  = gsn_xy(wks,a6,b6,xyres) ; Draw a side line.
;*************************************************************** 
; now draw a large brown line to represent the chimney
;***************************************************************
  xyres@xyLineColors      = (/"brown"/)   ; chimney color
  xyres@xyLineThicknessF  = 9.0           ; thick line 
  xyres@tiMainString      = "Pollutant Trajectory in a 3D Volume"
  chimney = gsn_xy(wks,a0,b0,xyres)       ; Draw the chimney.

  draw(wks)
  frame(wks)

end
