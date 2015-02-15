color(r, g, b){
   static color
   If !color
      color := Object("base", Object("R", r, "G", g, "B", b
                                    ,"GetRGB", "Color_GetRGB"))
   return  Object("base", Color)
}
Color_GetRGB(clr) {
    return "not implemented"
}

waterColor(r, g, b){
   static waterColor
   If !waterColor
      waterColor := Object("base", color(r, g, b),"GetRGB", "WaterColor_GetRGB")
   return  Object("base", WaterColor)
}

WaterColor_GetRGB(clr){
return clr.R << 16 | clr.G << 8 | clr.B
}

test:
blue := color(0, 0, 255)
msgbox % blue.GetRGB() ; displays "not implemented"
blue := waterColor(0, 0, 255)
msgbox % blue.GetRGB() ; displays 255
return
