;*************************************************
; WRF static: panel different variables 
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
begin
;************************************************
; open file and read in data
;************************************************
  f     = addfile("static.wrfsi.nc", "r")
;************************************************
; Read variables
;************************************************
  use   = f->use(0,0,:,:)           ; land use dominant category
  stl   = f->stl(0,0,:,:)           ; top layer (0-30cm) dom cat soiltype
  sbl   = f->sbl(0,0,:,:)           ; bottom layer (30-90cm) dom cat soiltype
  lat2d = f->lat(0,0,:,:)
  lon2d = f->lon(0,0,:,:)
  lsMask= f->lnd(0,0,:,:)           ; land (1) water (0) mas

;************************************************
; Use mask function to set all ocean areas to _FillValue
;************************************************
  use   = mask(use,lsMask,1)
  stl   = mask(stl,lsMask,1)
  sbl   = mask(sbl,lsMask,1)

;************************************************
; Associate 2D coordinates with variables for plotting
;************************************************
  use@lat2d = lat2d
  use@lon2d = lon2d
  stl@lat2d = lat2d
  stl@lon2d = lon2d
  sbl@lat2d = lat2d
  sbl@lon2d = lon2d

;************************************************  
; The file should be examined via: ncdump -v grid_type static.wrsi 
; This will print the print type. then enter below. 
;************************************************  
  projection = "mercator"   

;************************************************
; create plots  
;************************************************
  wks = gsn_open_wks("ps" ,"WRF_static")      ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

  res                       = True             ; plot mods desired
  res@gsnSpreadColors       = True             ; use full range of colormap
  res@cnFillOn              = True             ; color plot desired
  res@cnLinesOn             = False            ; turn off contour lines
  res@cnLineLabelsOn        = False            ; turn off contour labels
  res@cnLevelSpacingF       = 1                ; manually specify interval
  res@cnFillMode            = "RasterFill"     ; activate raster mode
  res@lbLabelAutoStride     = True             ; let NCL figure lb stride

;************************************************
; Turn on lat / lon labeling
;************************************************
;;res@pmTickMarkDisplayMode = "Always"         ; turn on tickmarks

  dimll = dimsizes(lat2d)
  nlat  = dimll(0)
  mlon  = dimll(1)

  res@mpProjection          = projection
  res@mpLimitMode           = "Corners"  
  res@mpLeftCornerLatF      = lat2d(0,0)
  res@mpLeftCornerLonF      = lon2d(0,0)
  res@mpRightCornerLatF     = lat2d(nlat-1,mlon-1)
  res@mpRightCornerLonF     = lon2d(nlat-1,mlon-1)

  res@mpCenterLonF          = f->LoV          ; set center logitude         
                                    
  if (projection.eq."LambertConformal") then    
      res@mpLambertParallel1F   = f->Latin1    
      res@mpLambertParallel2F   = f->Latin2   
      res@mpLambertMeridianF    = f->LoV     
  end if

  res@mpFillOn              = False            ; turn off map fill
  res@mpOutlineDrawOrder    = "PostDraw"       ; draw continental outline last
  res@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries

;;res@tfDoNDCOverlay        = True             ; True only for 'native' grid
  res@gsnAddCyclic          = False            ; data are not cyclic

;************************************************
; allocate array for 3 plots
;************************************************
  plts                      = new (3,"graphic")   

;************************************************
; Tell NCL not to draw or advance frame for individual plots
;************************************************
  res@gsnDraw               = False            ; (a) do not draw 
  res@gsnFrame              = False            ; (b) do not advance 'frame'

  plts(0)                   = gsn_csm_contour_map(wks,use,res)
  plts(1)                   = gsn_csm_contour_map(wks,stl,res)
  plts(2)                   = gsn_csm_contour_map(wks,sbl,res)
;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@txString         = "Land Use and Soil Type"     
  resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level  
  gsn_panel(wks,plts,(/1,2/),resP)               ; now draw as one plot

end

