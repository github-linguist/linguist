;----------------------------------------------------------------------
; topo_9.ncl
;
; Concepts illustrated:
;   - Recreating a jpeg topographic image as an NCL map object
;   - Zooming in on a jpeg image
;   - Drawing a box around an area of interest on a map
;   - Attaching polylines to a map
;   - Using "overlay" to overlay multiple contour plots
;   - Using more than 256 colors per frame
;   - Using functions for cleaner code
;----------------------------------------------------------------------
; NOTE: This example will only work with NCL V6.1.0 and later.
;
; This script recreates a JPEG image that was converted to a NetCDF
; file with color separated bands using the open source tool
; "gdal_translate":
;
;  gdal_translate -ot Int16 -of netCDF EarthMap_2500x1250.jpg \
;           EarthMap_2500x1250.nc
;----------------------------------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   

;----------------------------------------------------------------------
; This function imports a JPEG image that's on the whole globe,
; and recreates it as an NCL map object that is zoomed in on the
; southern tip of Africa.
;----------------------------------------------------------------------
undef("recreate_jpeg_image")
function recreate_jpeg_image(wks,minlat,maxlat,minlon,maxlon)
begin
  orig_jpg_filename = "EarthMap_2500x1250.jpg"
  nc_filename       = "EarthMap_2500x1250.nc"

;--You could use a system call to do the NetCDF conversion
;  cmd = "gdal_translate -ot Int16 -of netCDF " + jpeg_filename + \
;         " " + nc_filename)
;  system(cmd)

;---Read the three bands of data
  f     = addfile(nc_filename,"r")
  Band1 = where(f->Band1.gt.255, 255, f->Band1)  ; red channel
  Band2 = where(f->Band2.gt.255, 255, f->Band2)  ; green channel
  Band3 = where(f->Band3.gt.255, 255, f->Band3)  ; blue channel

  band_dims = dimsizes(Band3)
  nlat      = band_dims(0)
  nlon      = band_dims(1)
  print("dimensions of image = " + nlat + " x " + nlon)

;
; Add lat/lon data so we can overlay on a map, and/or 
; overlay contours. We know the image is global,
; cylindrical equidistant, and centered about lon=0.
;
  lat       = fspan( -90, 90,nlat)
  lon       = fspan(-180,180,nlon)
  lat@units = "degrees_north"
  lon@units = "degrees_east"

  Band1!0   = "lat"
  Band1!1   = "lon"
  Band2!0   = "lat"
  Band2!1   = "lon"
  Band3!0   = "lat"
  Band3!1   = "lon"
  Band1&lat = lat
  Band1&lon = lon
  Band2&lat = lat
  Band2&lon = lon
  Band3&lat = lat
  Band3&lon = lon

  res                 = True

  res@gsnMaximize     = True

  res@gsnFrame        = False        ; Don't draw or advance
  res@gsnDraw         = False        ; frame yet.

  res@cnFillOn        = True         
  res@cnFillMode      = "RasterFill" ; Raster fill can be faster

  res@cnLevelSelectionMode  = "EqualSpacedLevels"
  res@cnMaxLevelCount       = 254  
  res@cnFillBackgroundColor = (/ 1., 1., 1., 1./)

  res@cnLinesOn       = False              ; Turn off contour lines      .
  res@cnLineLabelsOn  = False              ; Turn off contour labels
  res@cnInfoLabelOn   = False              ; Turn off info label
  res@lbLabelBarOn    = False              ; Turn off labelbar
  res@gsnRightString  = ""                 ; Turn off subtitles
  res@gsnLeftString   = ""
  res@pmTickMarkDisplayMode    = "Always"

;---Construct RGBA colormaps...
  ramp   = fspan(0., 1., 255)
  reds   = new((/255, 4/), float)
  greens = new((/255, 4/), float)
  blues  = new((/255, 4/), float)

  reds   = 0
  greens = 0
  blues  = 0

  reds(:,0)   = ramp
  greens(:,1) = ramp
  blues(:,2)  = ramp

  ; The red contour map is plotted fully opaque; the green and blue
  ; are plotted completely transparent. When overlain, the colors 
  ; combine (rather magically).
  reds(:,3)   = 1.
  greens(:,3) = 0 
  blues(:,3)  = 0

  res@cnFillColors = greens 
  greenMap = gsn_csm_contour(wks, Band2, res) 

  res@cnFillColors = blues
  blueMap = gsn_csm_contour(wks, Band3, res) 
 
;---This will be our base, so make it a map plot.
  res@cnFillColors             = reds 
  res@gsnAddCyclic             = False

  res@mpFillOn                 = False  

;---Zoom in on area of interest
  res@mpMinLatF                = minlat
  res@mpMaxLatF                = maxlat
  res@mpMinLonF                = minlon
  res@mpMaxLonF                = maxlon

  redMap = gsn_csm_contour_map(wks, Band1, res) 

;---Overlay everything to create the topo map
  overlay(redMap, greenMap)
  overlay(redMap, blueMap)

  return(redMap)
end

;----------------------------------------------------------------------
; Main code
;----------------------------------------------------------------------
begin
;---Recreating jpeg images only works for X11 and PNG.
  wks = gsn_open_wks("png","topo")

;---Southern part of Africa
  minlat = -40
  maxlat =  5
  minlon = 10
  maxlon = 40

  map = recreate_jpeg_image(wks,minlat,maxlat,minlon,maxlon)

;---Overlay a red box
  lonbox = (/ 15, 35, 35, 15, 15/)
  latbox = (/-30,-30,-10,-10,-30/)

  lnres                  = True
  lnres@gsLineColor      = "red" ; red box 
  lnres@gsLineThicknessF = 4.0   ; make box thicker
  box = gsn_add_polyline(wks,map,lonbox,latbox,lnres)

  draw(map)       ; Drawing the map will draw the red box
  frame(wks)

end
