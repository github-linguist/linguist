    ...
    ;===================================================================  
    ; Assume variables T, PS and ORO exist and that they have 
    ; associated meta data: (a) coordinate variables time, lev, lat, lon       
    ; and (b) attributes
    ;===================================================================  
        ntim  = dimsizes(time)                 ; get dimension sizes  
	klev  = dimsizes(lev)                                               
	nlat  = dimsizes(lat)  
	nlon  = dimsizes(lon)      

        diro = "./"                     ; Output directory
        filo = "example.nc"             ; Output file
	system("/bin/rm -f " + diro + filo)    ; remove if exists
	fout  = addfile (diro + filo, "c")  ; open output file

    ;===================================================================
    ; explicitly declare file definition mode. Improve efficiency.
    ;===================================================================
        setfileoption(fout,"DefineMode",True)

    ;===================================================================
    ; create global attributes of the file
    ;===================================================================
        fAtt               = True            ; assign file attributes
	fAtt@title         = "NCL Efficient Approach to netCDF Creation"  
	fAtt@source_file   =  "original-file.nc"
	fAtt@Conventions   = "None"   
	fAtt@creation_date = systemfunc ("date")        
	fileattdef( fout, fAtt )            ; copy file attributes    

    ;===================================================================
    ; predefine the coordinate variables and their dimensionality
    ; Note: to get an UNLIMITED record dimension, we set the dimensionality
    ; to -1 (or the actual size) and set the dimension name to True.
    ;===================================================================
        dimNames = (/"time", "lat", "lon", "lev"/)  
	dimSizes = (/ -1   ,  nlat,  nlon, nlev /) 
	dimUnlim = (/ True , False, False, False/)   
	filedimdef(fout,dimNames,dimSizes,dimUnlim)

    ;===================================================================
    ; predefine the the dimensionality of the variables to be written out
    ;===================================================================
    ; Here we are using NCL functions to facilitate defining 
    ; each variable's dimension name(s) and type. 
    ; The following could be replaced with explicit, user defined dimension 
    ; names different from those associated with the variable in memory. 
    ; Say, PS(time,lat,lon) in the NCL script. They could be redefined for the file via: 
    ; filevardef(fout, "PS"   ,typeof(PS) ,(/"TIME","latitude","longitude"/)) 
    ;===================================================================
       filevardef(fout, "time" ,typeof(time),getvardims(time)) 
       filevardef(fout, "lev"  ,typeof(lev),getvardims(lev) )                           
       filevardef(fout, "lat"  ,typeof(lat),getvardims(lat))                          
       filevardef(fout, "lon"  ,typeof(lon),getvardims(lon))                          
       filevardef(fout, "T"    ,typeof(T)  ,getvardims(T))    
       filevardef(fout, "PS"   ,typeof(PS) ,getvardims(PS))          
       filevardef(fout, "TOPOG",typeof(ORO),getvardims(ORO))  ; variable name on the file                
                                                              ; different from name on script
    ;===================================================================
    ; Copy attributes associated with each variable to the file
    ; All attributes associated with each variable will be copied.
    ;====================================================================
       filevarattdef(fout,"T",T)                           ; copy T attributes
       filevarattdef(fout,"time" ,time)                    ; copy time attributes
       filevarattdef(fout,"lev"  ,lev)                     ; copy lev attributes
       filevarattdef(fout,"lat"  ,lat)                     ; copy lat attributes
       filevarattdef(fout,"lon"  ,lon)                     ; copy lon attributes
       filevarattdef(fout,"PS"   ,PS)                      ; copy PS attributes
       filevarattdef(fout,"TOPOG",ORO)                     ; copy TOPOG attributes

    ;===================================================================
    ; explicitly exit file definition mode. **NOT REQUIRED**
    ;===================================================================
        setfileoption(fout,"DefineMode",False)

    ;===================================================================
    ; output only the data values since the dimensionality and such have
    ; been predefined. The "(/", "/)" syntax tells NCL to only output the
    ; data values to the predefined locations on the file.
    ;====================================================================
       fout->time   = (/time/)     
       fout->lev    = (/lev/)
       fout->lat    = (/lat/)
       fout->lon    = (/lon/) 
       fout->T      = (/T/)
       fout->Z      = (/PS/)
       fout->TOPOG  = (/ORO/)
