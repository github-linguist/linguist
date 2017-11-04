; xy_29.ncl
;
; Concepts illustrated:
;   - Reading data from an ASCII file with headers
;   - Creating a separate procedure to create a specific plot
;   - Attaching polymarkers to an XY plot
;
; This script was originally from Dr. Birgit Hassler (NOAA)
;****************************************************

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

;************************************************
;             Plot Procedure
;************************************************
procedure plotTCOPolym(pltName[1]:string, pltType[1]:string, filName[1]:string \
                   ,xTitle[1]:string , yTitle[1]:string \ 
                   ,year[*]:numeric, y[*]:numeric)
local wks, res, ntim, gsres, MarkerCol, OldYear, i, xmarker, ymarker
  
begin
  wks = gsn_open_wks(pltType,pltName)
  gsn_define_colormap(wks,"default")
  
  res = True
  res@gsnMaximize    = True            ; make "ps", "eps", "pdf" large

  res@vpHeightF      = 0.5             ; change aspect ratio of plot
  res@vpWidthF       = 0.75                 
  res@vpXF           = 0.15            ; start plot at x ndc coord 
  res@tiXAxisString  = xTitle     
  res@tiYAxisString  = yTitle     
  res@tiMainString   = filName

  ntim   = dimsizes(year)
  res@trXMinF = year(0)-1        
  res@trXMaxF = year(ntim-1)+1        

  res@gsnDraw        = False
  res@gsnFrame       = False
  res@xyMarkLineMode = "markers"
  res@xyMarker       = 16
  res@xyMarkerColor  = "Background"                     
  plot               = gsn_csm_xy (wks,year,y,res) ; create plot frame ork
  
                     ; add different color polymarkers for each year 
  gsres     = True
  MarkerCol = 2
  OldYear   = year(0)
  
  do i=0,ntim-1 
    xmarker = year(i)
    ymarker = y(i)
    
    if (i.gt.0) then 
      if (year(i).gt.OldYear) then
        MarkerCol = MarkerCol+1
      end if
      OldYear = year(i)
    end if
    
    gsres@gsMarkerColor = MarkerCol
    gsres@gsMarkerIndex = 16
   ;gsres@gsMarkerSizeF = 15.0
                        ; add (attach) polymarkers to existing plot object 
    plot@$unique_string("dum")$ = gsn_add_polymarker(wks,plot,xmarker,ymarker,gsres)
  end do

  draw(plot)
  frame(wks)
end

;***********************************************************
;                   MAIN
;***********************************************************
   pltType = "ps"      ; "ps", "eps", "png", "x11"

                       ; read multiple ascii file names
 ;;fili = "Southpole_TCOTimeSeries_11.dat"

   diri = "./"
   fili = systemfunc("cd "+diri+" ; ls *TCOT*dat")
   print(fili)

   nfil = dimsizes(fili)

   nhead= 4      ; number of header lines on ascii file(s)
   ncol = 4      ; year, month, day, O3

   do nf=0,nfil-1
      sfx  = get_file_suffix(fili(nf), 0) ; sfx = ".dat"
      filx = sfx@fBase                    ; filx= "Southpole_TCOTimeSeries_11"
                                          ; read ascii files
      data = readAsciiTable(diri+fili(nf), ncol, "float", nhead)
      dimd = dimsizes(data)
      ntim = dimd(0)                      ; # rows

      year = toint( data(:,0) )           ; user decision ... convert to integer
      mon  = toint( data(:,1) )
      day  = toint( data(:,2) )
 
      hour = new (ntim, "integer", "No_FillValue")
      mn   = new (ntim, "integer", "No_FillValue")
      sec  = new (ntim, "double" , "No_FillValue")
      hour = 0
      mn   = 0
      sec  = 0d0
                                          ; create COARDS/udunits time variable
    ;;tunits = "days since 1900-01-01 00:00:0.0"
      tunits = "days since "+year(0)+"-"+mon(0)+"-"+day(0)+" 00:00:0.0"
      time   = cd_inv_calendar(year,mon,day,hour,mn,sec,tunits, 0)
      time!0 = "time"
      time&time = time
      ;printVarSummary(time)

                                          ; create a Gregorin 'date' variable
      date = year*10000 + mon*100 + day
      date!0 = "time"
      date@units = "yyyymmdd"
      date&time = time
     ;printVarSummary(date)

      O3   = data(:,3) 
      O3@long_name = "total column ozone"
      O3@units     = "DU"

      O3!0         = "time"
      O3&time      = time
     ;printVarSummary(O3)
     ;print(" ")
     ;print(date+"  "+time+"   "+O3)

                                          ; plot
      yTitle = O3@long_name
      year@long_name = "YEAR"

      plotTCOPolym (filx, pltType, fili(nf), year@long_name, yTitle,  year, O3) 
     
      delete(time) ; delete ... size (# rows) may change in the next file
      delete(date)
      delete(year)
      delete(mon )
      delete(day )
      delete(mn  )
      delete(sec )
      delete(O3  )
      delete(data)
   end do

