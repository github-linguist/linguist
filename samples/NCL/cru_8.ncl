;*****************************************************
; cru_8.ncl
;
; Concepts illustrated:
;   - Plotting CRU (Climate Research Unit)/ BADC data
;   - Selecting a sub-period
;   - calculating a climatology
;   - Drawing raster contours; very basic graphics
;
;*****************************************************

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"    ; not needed 6.20 onward 
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

; create references (pointers) to the files

  diri = "./"
  fcld = addfile(diri+"cru_ts3.21.1901.2012.cld.dat.nc", "r")
  fdtr = addfile(diri+"cru_ts3.21.1901.2012.dtr.dat.nc", "r")
  ffrs = addfile(diri+"cru_ts3.21.1901.2012.frs.dat.nc", "r")
  fpet = addfile(diri+"cru_ts3.21.1901.2012.pet.dat.nc", "r")
  fpre = addfile(diri+"cru_ts3.21.1901.2012.pre.dat.nc", "r")
  ftmn = addfile(diri+"cru_ts3.21.1901.2012.tmn.dat.nc", "r")
  ftmp = addfile(diri+"cru_ts3.21.1901.2012.tmp.dat.nc", "r")
  ftmx = addfile(diri+"cru_ts3.21.1901.2012.tmx.dat.nc", "r")
  fvap = addfile(diri+"cru_ts3.21.1901.2012.vap.dat.nc", "r")
  fwet = addfile(diri+"cru_ts3.21.1901.2012.wet.dat.nc", "r")

; specify start & last dates (arbitrary)

  ymStrt = 199101
  ymLast = 200012
  
; get index values of start/lat dates

  time   = fcld->time
  yyyymm = cd_calendar(time, -1)
  
  ntStrt = ind(yyyymm.eq.ymStrt)   ; index values
  ntLast = ind(yyyymm.eq.ymLast)
  
; read time segment

  cld    = fcld->cld(ntStrt:ntLast,:,:)
  dtr    = fdtr->dtr(ntStrt:ntLast,:,:)
  frs    = ffrs->frs(ntStrt:ntLast,:,:) 
  pet    = fpet->pet(ntStrt:ntLast,:,:)  
  pre    = fpre->pre(ntStrt:ntLast,:,:) 
  tmn    = ftmn->tmn(ntStrt:ntLast,:,:) 
  tmp    = ftmp->tmp(ntStrt:ntLast,:,:)      
  tmx    = ftmx->tmx(ntStrt:ntLast,:,:)   
  vap    = fvap->vap(ntStrt:ntLast,:,:) 
  wet    = fwet->wet(ntStrt:ntLast,:,:) 
  
  printVarSummary(cld)        ;  [time | 120] x [lat | 360] x [lon | 720]
  
; calculate monthly climatologies

  cldclm = clmMonTLL(cld)     
  dtrclm = clmMonTLL(dtr)
  frsclm = clmMonTLL(frs)
  petclm = clmMonTLL(pet)
  preclm = clmMonTLL(pre)
  tmnclm = clmMonTLL(tmn)
  tmpclm = clmMonTLL(tmp)
  tmxclm = clmMonTLL(tmx)
  vapclm = clmMonTLL(vap)
  wetclm = clmMonTLL(wet)
  
    
  printVarSummary(cldclm)     ;  [month | 12] x [lat | 360] x [lon | 720]
 
;************************************
; create plots ... very simple
;************************************
 
  nt     = 6                         
  month  = "July"
  yrStrt = ymStrt/100
  yrLast = ymLast/100
  title  = month+": "+yrStrt+"-"+yrLast

  wks = gsn_open_wks("ps","cru")          ; open a ps  file
  gsn_define_colormap(wks,"ncl_default")  ; choose colormap; not needed 6.20 onward 
  plot = new(2,graphic)                   ; create graphic array

  res                      = True
  res@cnFillOn             = True         ; turn on color fill; not needed 6.20 onward 
  res@cnFillMode           = "RasterFill" ; Raster Mode
  res@cnLinesOn            = False        ; Turn off contour lines
  
  res@gsnDraw              = False        ; do not draw picture
  res@gsnFrame             = False        ; do not advance frame
  res@lbOrientation        = "Vertical"   ; vertical label bar
  
  resp                     = True
  resp@gsnMaximize         = True         ; make ps, eps, pdf large

  resp@txString            = title+": CLD, FRS"
  plot(0)=gsn_csm_contour_map_ce(wks,cldclm(nt,:,:),res)
  plot(1)=gsn_csm_contour_map_ce(wks,frsclm(nt,:,:),res)
  gsn_panel(wks,plot,(/2,1/),resp)
  
  resp@txString            = title+": PET, VAP"
  plot(0)=gsn_csm_contour_map_ce(wks,petclm(nt,:,:),res)
  plot(1)=gsn_csm_contour_map_ce(wks,vapclm(nt,:,:),res)
  gsn_panel(wks,plot,(/2,1/),resp)

  resp@txString            = title+": TMN, TMX"
  plot(0)=gsn_csm_contour_map_ce(wks,tmnclm(nt,:,:),res)
  plot(1)=gsn_csm_contour_map_ce(wks,tmxclm(nt,:,:),res)
  gsn_panel(wks,plot,(/2,1/),resp)

  resp@txString            = title+": TMP, DTR"
  plot(0)=gsn_csm_contour_map_ce(wks,tmpclm(nt,:,:),res)
  plot(1)=gsn_csm_contour_map_ce(wks,dtrclm(nt,:,:),res)
  gsn_panel(wks,plot,(/2,1/),resp)

  resp@txString            = title+": WET, PRE"
  plot(0)=gsn_csm_contour_map_ce(wks,wetclm(nt,:,:),res)

 ;colors = (/ ... /)             
 ;res@cnFillPalette    = colors                ; optional: distinct colors for categories
  res@cnLevelSelectionMode = "ExplicitLevels"  ; use unequal spacing
  res@cnLevels             = (/2.0,10,25,37.5,50,75,100,125,150,175,200,300,400,500,750/) 

  plot(1)=gsn_csm_contour_map_ce(wks,preclm(nt,:,:),res)
  gsn_panel(wks,plot,(/2,1/),resp)
