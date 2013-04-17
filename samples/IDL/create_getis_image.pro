@Scale_Vector

PRO CREATE_GETIS_IMAGE, event
  COMPILE_OPT STRICTARR
  ; Use the ENVI dialog box to select a file
  ENVI_SELECT, fid=file,dims=dims,pos=pos, m_fid=m_fid, m_pos=m_pos, /mask, title="Select the image you want to perform the Getis calculation on"
  
  ; If the dialog box was cancelled then stop the procedure
  IF file[0] EQ -1 THEN RETURN
  
  ; Create dialog box window
  TLB = WIDGET_AUTO_BASE(title="Create Getis Image")
  
  ; Create dropdown list to select distance value
  list = ['d = 1 (3x3 square)', 'd = 2 (5x5 square)', 'd = 3 (7x7 square)', 'd = 4 (9x9 square)']
  
  W_Distance = WIDGET_PMENU(TLB, /AUTO_MANAGE, list=list, uvalue='d')
  
  ; Create the widget to let the user select file or memory output
  W_FileOrMem = WIDGET_OUTFM(TLB, /AUTO_MANAGE, uvalue='fm')
  
  ; Start the automatic management of the window
  result = AUTO_WID_MNG(TLB) 
  
  ; If the OK button was pressed
  IF result.accept EQ 0 THEN RETURN
  
  ; Get the details of the file, ready to write to the disk if needed
  ENVI_FILE_QUERY, file, fname=fname, data_type=data_type, xstart=xstart, $
    ystart=ystart, INTERLEAVE=interleave
    
  ; Get the map info of the file so that we can output it to the new file
  map_info = ENVI_GET_MAP_INFO(FID=file)
  
  ; Initialise the progress bar window - differently depending if the output is
  ; to memory or to file
  IF result.fm.in_memory EQ 1 THEN BEGIN
    ENVI_REPORT_INIT, ['Input File: ' + fname, 'Output to memory'], title='Getis status', base=base, /INTERRUPT
  ENDIF ELSE BEGIN
    ENVI_REPORT_INIT, ['Input File: ' + fname, 'Output File: ' + result.fm.name], title='Getis status', base=base, /INTERRUPT
  ENDELSE
  
  ; Call the function to create the Getis image
  GetisImage = CREATE_GETIS_IMAGE(file, dims, pos, result.d + 1, base, m_fid, m_pos)

  IF result.fm.in_memory EQ 1 THEN BEGIN
    ; If the user wanted the result to go to memory then just output it there
    ENVI_ENTER_DATA, GetisImage
  ENDIF ELSE BEGIN
    ; If the output is to file then open the file, write the binary data
    ; and close the file
    OpenW, unit, result.fm.name, /GET_LUN
    WriteU, unit, GetisImage
    FREE_LUN, unit
    
    ; Then calculate the values needed to create the header file, and create it
    NSamples = dims[2] - dims[1] + 1
    NLines = dims[4] - dims[3] + 1
    NBands = N_ELEMENTS(pos)
    ENVI_SETUP_HEAD, FNAME=result.fm.name, NS=NSamples, NL=NLines, NB=NBands, $
      DATA_TYPE=4, offset=0, INTERLEAVE=interleave, $
      XSTART=xstart+dims[1], YSTART=ystart+dims[3], $
      DESCRIP="Getis Image Output", MAP_INFO=map_info, /OPEN, /WRITE
  ENDELSE
  
 
END

FUNCTION GETIS_NOGUI, file, dims, pos, m_fid, m_pos, distance
  COMPILE_OPT STRICTARR
  ; Get the details of the file, ready to write to the disk
  ENVI_FILE_QUERY, file, fname=fname, data_type=data_type, xstart=xstart, $
    ystart=ystart, INTERLEAVE=interleave
    
  ; Get the map info of the file so that we can output it to the new file
  map_info = ENVI_GET_MAP_INFO(FID=file)
  
  output_file = fname + "_getis_distance_" + strcompress(string(distance)) + ".bsq"
  
  ; Initialise the progress bar window
  ENVI_REPORT_INIT, ['Input File: ' + fname, 'Output File: ' + output_file], title='Getis status', base=base, /INTERRUPT
  
  ; Call the function to create the Getis image - DISTANCE HARD CODED AS 1
  GetisImage = CREATE_GETIS_IMAGE(file, dims, pos, distance, base, m_fid, m_pos)
  
  help, GetisImage
  
  ; If the output is to file then open the file, write the binary data
  ; and close the file
  OpenW, unit, output_file, /GET_LUN
  WriteU, unit, GetisImage
  FREE_LUN, unit
    
  ; Then calculate the values needed to create the header file, and create it
  NSamples = dims[2] - dims[1] + 1
  NLines = dims[4] - dims[3] + 1
  NBands = N_ELEMENTS(pos)
  ENVI_SETUP_HEAD, FNAME=output_file, NS=NSamples, NL=NLines, NB=NBands, $
    DATA_TYPE=4, offset=0, INTERLEAVE=interleave, $
    XSTART=xstart+dims[1], YSTART=ystart+dims[3], $
    DESCRIP="Getis Image Output", MAP_INFO=map_info, /OPEN, /WRITE
    
  ENVI_OPEN_FILE, output_file, r_fid=fid
  
  return, fid
END

; Creates a Getis image given a FID, the dimensions of the file, a distance to use for the getis routine
; and a base window to send progress updates to as well as a fid and pos for the mask (if any)
FUNCTION CREATE_GETIS_IMAGE, file, dims, pos, distance, report_base, m_fid, m_pos
  COMPILE_OPT STRICTARR
  NumRows = dims[2] - dims[1]
  NumCols = dims[4] - dims[3]
  
  NumPos = N_ELEMENTS(pos)
  
  ; Let the progress bar know how many bands we're dealing with (denom. of fraction)
  ENVI_REPORT_INC, report_base, NumPos
  
  FOR CurrPos = 0, NumPos - 1 DO BEGIN
    ; Send an update to the progress window telling it to let us know if cancel has been pressed
    ENVI_REPORT_STAT, report_base, CurrPos, NumPos
    
    ; Get the data for the current band
    WholeBand = float(ENVI_GET_DATA(fid=file, dims=dims, pos=pos[CurrPos]))
    CountOfNAN = 0
     ; Perform the masking (if needed)
    if m_fid NE -1 THEN BEGIN
      ; Load the mask band
      ENVI_FILE_QUERY, m_fid, dims=dims
      MaskBand = ENVI_GET_DATA(fid=m_fid, dims=dims, pos=m_pos)
      
      indices = WHERE(MaskBand EQ 0, count)
      CountOfNAN = count
      IF count GT 0 THEN WholeBand[indices] = !Values.F_NAN
    endif
    
    ; Get the global mean
    GlobMean = MEAN(WholeBand, /NAN)
    
    ; Get the global variance
    GlobVariance = VARIANCE(WholeBand, /NAN)
    
    
    ; Get the number of values in the whole image
    GlobNumber = (NumRows * NumCols - CountOfNAN)
    
    ; Converts a distance to the length of each side of the square
    ; Eg. A distance of 1 to a length of 3
    DimOfArray = (distance * 2) + 1
    
    NumOfElements = DimOfArray * DimOfArray
    
    ; Create the kernel for the summing CONVOL operation
    Kernel = FLTARR(DimOfArray, DimOfArray)
    Kernel = Kernel + 1
    
    ; Create an image where each element is the sum of the elements within
    ; d around it
    SummedImage = CONVOL(FLOAT(WholeBand), Kernel, /CENTER, /EDGE_TRUNCATE, /NAN)
    
    ; Create an image where each element is the result of the top fraction part
    ; of the getis formula
    TopFraction = SummedImage - (FLOAT(NumOfElements) * GlobMean)
    
    ; Calculate the square root bit of the formula and then create a single variable
    ; with the bottom fraction part of the formula (this is constant for all pixels)
    SquareRootAnswer = SQRT((FLOAT(NumOfElements) * (GlobNumber - NumOfElements))/(GlobNumber - 1))
    BottomFraction = SQRT(GlobVariance) * SquareRootAnswer
    
    ; Create an image with the getis values in it
    Getis = FLOAT(TopFraction) / BottomFraction
    
    ; If it's the first time then copy the Getis result to OutputArray,
    ; if not then append it to the end of OutputArray
    IF (CurrPos EQ 0) THEN OutputArray = Getis ELSE OutputArray = [ [[OutputArray]], [[Getis]] ]
  ENDFOR
  
  ; Perform the masking (if needed)
  if m_fid NE -1 THEN BEGIN
    ; Load the mask band
    ENVI_FILE_QUERY, m_fid, dims=dims
    MaskBand = ENVI_GET_DATA(fid=m_fid, dims=dims, pos=m_pos)
    
    ;Apply the mask for each of the bands
    FOR i=0, NumPos-1 DO BEGIN
      indices = WHERE(MaskBand EQ 0, count)
      IF count GT 0 THEN BEGIN
          a_indices = ARRAY_INDICES(MaskBand, indices)
          x = a_indices[0,*]
          y = a_indices[1,*]
          band_index = replicate(i, N_ELEMENTS(x))
          OutputArray[x, y, band_index] = !Values.F_NAN
      ENDIF
    ENDFOR
  endif
  
  ; Close the progress window
  ENVI_REPORT_INIT,base=report_base, /FINISH
  
  ; Return the result
  RETURN, OutputArray
END
