FUNCTION INIT_GRIDTILES_ARRAY
  COMPILE_OPT STRICTARR
  
  TileStruct = {x:"", y:""}
  GridTiles = replicate(TileStruct, 7, 13)
  
  ; First column of diagram
  GridTiles[0, 0].x = "S"
  GridTiles[0, 0].y = "V"
  
  GridTiles[0, 7].x = "N"
  GridTiles[0, 7].y = "L"
  
  GridTiles[0, 8].x = "N"
  GridTiles[0, 8].y = "F"
  
  GridTiles[0, 9].x = "N"
  GridTiles[0, 9].y = "A"
  
  ; Second column of diagram
  GridTiles[1, 0].x = "S"
  GridTiles[1, 0].y = "W"
  
  GridTiles[1, 1].x = "S"
  GridTiles[1, 1].y = "R"
  
  GridTiles[1, 2].x = "S"
  GridTiles[1, 2].y = "M"
  
  GridTiles[1, 5].x = "N"
  GridTiles[1, 5].y = "W"
  
  GridTiles[1, 6].x = "N"
  GridTiles[1, 6].y = "R"
  
  GridTiles[1, 7].x = "N"
  GridTiles[1, 7].y = "M"
  
  GridTiles[1, 8].x = "N"
  GridTiles[1, 8].y = "G"
  
  GridTiles[1, 9].x = "N"
  GridTiles[1, 9].y = "B"
  
  ; Third column of diagram
  GridTiles[2, 0].x = "S"
  GridTiles[2, 0].y = "X"
  
  GridTiles[2, 1].x = "S"
  GridTiles[2, 1].y = "S"
  
  GridTiles[2, 2].x = "S"
  GridTiles[2, 2].y = "N"
  
  GridTiles[2, 3].x = "S"
  GridTiles[2, 3].y = "H"
  
  GridTiles[2, 4].x = "S"
  GridTiles[2, 4].y = "C"
  
  GridTiles[2, 5].x = "N"
  GridTiles[2, 5].y = "X"
  
  GridTiles[2, 6].x = "N"
  GridTiles[2, 6].y = "S"
  
  GridTiles[2, 7].x = "N"
  GridTiles[2, 7].y = "N"
  
  GridTiles[2, 8].x = "N"
  GridTiles[2, 8].y = "H"
  
  GridTiles[2, 9].x = "N"
  GridTiles[2, 9].y = "C"
  
  ; 4th column
  GridTiles[3, 0].x = "S"
  GridTiles[3, 0].y = "Y"
  
  GridTiles[3, 1].x = "S"
  GridTiles[3, 1].y = "T"
  
  GridTiles[3, 2].x = "S"
  GridTiles[3, 2].y = "O"
  
  GridTiles[3, 3].x = "S"
  GridTiles[3, 3].y = "J"
  
  GridTiles[3, 4].x = "S"
  GridTiles[3, 4].y = "D"
  
  GridTiles[3, 5].x = "N"
  GridTiles[3, 5].y = "Y"
  
  GridTiles[3, 6].x = "N"
  GridTiles[3, 6].y = "T"
  
  GridTiles[3, 7].x = "N"
  GridTiles[3, 7].y = "O"
  
  GridTiles[3, 8].x = "N"
  GridTiles[3, 8].y = "J"
  
  GridTiles[3, 9].x = "N"
  GridTiles[3, 9].y = "D"
  
  ; 5th column
  GridTiles[4, 0].x = "S"
  GridTiles[4, 0].y = "Z"
  
  GridTiles[4, 1].x = "S"
  GridTiles[4, 1].y = "U"
  
  GridTiles[4, 2].x = "S"
  GridTiles[4, 2].y = "P"
  
  GridTiles[4, 3].x = "S"
  GridTiles[4, 3].y = "K"

  GridTiles[4, 4].x = "S"
  GridTiles[4, 4].y = "E"
  
  GridTiles[4, 5].x = "N"
  GridTiles[4, 5].y = "Z"
  
  GridTiles[4, 6].x = "N"
  GridTiles[4, 6].y = "U"
  
  GridTiles[4, 8].x = "N"
  GridTiles[4, 8].y = "K"
  
  ;6th column
  GridTiles[5, 0].x = "T"
  GridTiles[5, 0].y = "V"
  
  GridTiles[5, 1].x = "T"
  GridTiles[5, 1].y = "Q"
  
  GridTiles[5, 2].x = "T"
  GridTiles[5, 2].y = "L"
  
  GridTiles[5, 3].x = "T"
  GridTiles[5, 3].y = "F"
  
  GridTiles[5, 4].x = "T"
  GridTiles[5, 4].y = "A"
  
  ; 7th column
  GridTiles[6, 1].x = "T"
  GridTiles[6, 1].y = "R"
  
  GridTiles[6, 2].x = "T"
  GridTiles[6, 2].y = "M"
  
  GridTiles[6, 3].x = "T"
  GridTiles[6, 3].y = "G"
  
  ; Row at the top
  GridTiles[4, 12].x = "H"
  GridTiles[4, 12].y = "P"
  
  GridTiles[3, 11].x = "H"
  GridTiles[3, 11].y = "T"
  
  GridTiles[4, 11].x = "H"
  GridTiles[4, 11].y = "U"
  
  GridTiles[3, 10].x = "H"
  GridTiles[3, 10].y = "Y"
  
  GridTiles[4, 10].x = "H"
  GridTiles[4, 10].y = "Z"  
  
  return, GridTiles
END

FUNCTION EVERY_GRID_BETWEEN, bottom, top
  COMPILE_OPT STRICTARR
  print, top
  print, bottom
  print, top - bottom
  
  n = bottom / 10000
  
  array = lonarr(10000)
  
  i = 0
  WHILE (n*10000 LT top) DO BEGIN
    ;print, n*10000
    array[i] = n*10000
    n=n+1
    i=i+1
  ENDWHILE
  
  new_array = array[WHERE(array)]
  
  return, new_array
END

FUNCTION GET_LETTERS_FROM_GRIDREF, x, y
  COMPILE_OPT STRICTARR
  x_First = STRMID(x, 1, 1)
  y_First = STRMID(y, 1, 1)
  
  GridTiles = INIT_GRIDTILES_ARRAY()
  
  Chars = GridTiles[fix(x_First), fix(y_First)]
  
  return, Chars
END

FUNCTION CONVERT_GRIDREF_TO_TILE_REF, x, y
  COMPILE_OPT STRICTARR
  Chars = GET_LETTERS_FROM_GRIDREF(x, y)
  FirstNum = STRMID(x, 2, 1)
  SecondNum = STRMID(y, 2, 1)
  
  return, Chars.x + Chars.y + STRCOMPRESS(STRING(FirstNum)) + STRCOMPRESS(STRING(SecondNum))
END

; Variables need to be input as strings in order bottom, top, left, right
FUNCTION SELECT_NM_TILES, bl_y, tl_y, bl_x, br_x
  COMPILE_OPT STRICTARR
  bottom = long(bl_y)
  top = long(tl_y)
  
  left = long(bl_x)
  right = long(br_x)
 
  selected_tiles = strarr(1000)
 
  y_intervals = EVERY_GRID_BETWEEN(bottom, top)
  x_intervals = EVERY_GRID_BETWEEN(left, right)
  
  k = 0
  FOR i = 0, N_ELEMENTS(y_intervals) - 1 DO BEGIN
    FOR j = 0, N_ELEMENTS(x_intervals) - 1 DO BEGIN
      selected_tiles[k] = CONVERT_GRIDREF_TO_TILE_REF(STRCOMPRESS(STRING(x_intervals[j])), STRCOMPRESS(STRING(y_intervals[i])))
      k = k + 1
    END
  END
  
  return, selected_tiles[WHERE(selected_tiles)]
END

PRO GUI_LEN_WIDTH_TO_NM_TILES, event
  COMPILE_OPT STRICTARR
  tlb = WIDGET_AUTO_BASE(title="NextMap Tile Selection")
  bl_x = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Bottom Left X", uvalue="bl_x")
  bl_y = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Bottom Left Y", uvalue="bl_y")
  tl_y = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="X Length (metres)", uvalue="x_len")
  br_x = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Y Length (metres)", uvalue="y_len")
  
  result = AUTO_WID_MNG(tlb)
  
  ; If the cancel button was pressed then exit
  IF result.accept EQ 0 THEN RETURN
  
  ; Get the tiles that are needed
  selected_tiles = SELECT_NM_TILES(result.bl_y, result.bl_y + result.y_len, result.bl_x, result.bl_x + result.x_len)
  
  ; Display the result to the user
  dlg_result = DIALOG_MESSAGE(["The required NextMAP tiles are:", selected_tiles[sort(selected_tiles)]], /information, title="NextMAP tiles")
END

PRO GUI_TO_NM_TILES, event
  COMPILE_OPT STRICTARR
  tlb = WIDGET_AUTO_BASE(title="NextMap Tile Selection")
  bl_x = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Bottom Left X", uvalue="bl_x")
  bl_y = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Bottom Left Y", uvalue="bl_y")
  tl_y = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Top Left Y", uvalue="tl_y")
  br_x = WIDGET_PARAM(tlb, /auto_manage, dt=3, xsize=20, prompt="Bottom Right X", uvalue="br_x")
  
  result = AUTO_WID_MNG(tlb)
  
  ; If the cancel button was pressed then exit
  IF result.accept EQ 0 THEN RETURN
  
  ; Get the tiles that are needed
  selected_tiles = SELECT_NM_TILES(result.bl_y, result.tl_y, result.bl_x, result.br_x)
  
  ; Display the result to the user
  dlg_result = DIALOG_MESSAGE(["The required NextMAP tiles are:", selected_tiles[sort(selected_tiles)]], /information, title="NextMAP tiles")
END

PRO IMAGE_TO_NM_TILES, event
  COMPILE_OPT STRICTARR
  ; Let the user select the file
  ENVI_SELECT, fid=fid
  
  ; Exit if dialog box cancelled
  if fid EQ -1 THEN RETURN;
  
  ; Find out how many lines and samples the file has
  ENVI_FILE_QUERY, fid, ns=ns, nl=nl
  
  ; Convert all four corners of the image to map co-ordinates
  ENVI_CONVERT_FILE_COORDINATES, fid, /TO_MAP, [0, ns, 0, nl], [0, 0, nl, ns], xmap, ymap
  
  ; Convert the map co-ords to strings
  bl_y = STRCOMPRESS(STRING(ymap[2]))
  tl_y = STRCOMPRESS(STRING(ymap[0]))
  bl_x = STRCOMPRESS(STRING(xmap[2]))
  br_x = STRCOMPRESS(STRING(xmap[3]))
  
  ; Get the tiles that are needed
  selected_tiles = SELECT_NM_TILES(bl_y, tl_y, bl_x, br_x)
  
  ; Display the result to the user
  result = DIALOG_MESSAGE(["The required NextMAP tiles are:", selected_tiles[sort(selected_tiles)]], /information, title="NextMAP tiles")
  
  print, selected_tiles[sort(selected_tiles)]
END
