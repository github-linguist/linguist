script "colorToRGB"

function colorToRGB pColor
    local tColorRGB

    if pColor is a color then
        set the colorOverlay["color"] of the templateGraphic to pColor
        put the colorOverlay["color"] of the templateGraphic into tColorRGB
        reset the templateGraphic
        return tColorRGB  for value

    else return empty for value
end colorToRGB
