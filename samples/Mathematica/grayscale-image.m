toGrayscale[rgb_Image] := ImageApply[#.{0.2126, 0.7152, 0.0722}&, rgb]
toFakeRGB[L_Image] := ImageApply[{#, #, #}&, L]
