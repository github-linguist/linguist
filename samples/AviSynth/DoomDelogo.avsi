##================================================================================================##
#                                                                                                  #
#                   ____                        ____       __                                      #
#                  / __ \____  ____  ____ ___  / __ \___  / /___  ____ _____                       #
#                 / / / / __ \/ __ \/ __ `__ \/ / / / _ \/ / __ \/ __ `/ __ \                      #
#                / /_/ / /_/ / /_/ / / / / / / /_/ /  __/ / /_/ / /_/ / /_/ /                      #
#               /_____/\____/\____/_/ /_/ /_/_____/\___/_/\____/\__, /\____/                       #
#                                                              /____/                              #
#                                                                                                  #
##================================================================================================##


#  DoomDelogo is a state of the no-art wobbly watermark remover.
# Maybe more than 95% of various commercial and pseudo-free apps use such method.


##================================================================================================##
#   Version: 1.0                                                                                   #
##================================================================================================##


# Author       : VoodooFX

# Requirement  : https://github.com/AviSynth/AviSynthPlus
# Doom9 link   : https://forum.doom9.org/showthread.php?t=184881
# GitHub link  : https://github.com/Purfview/DoomDelogo


# Version 1.0  : 2023/05/10.  First public release.


##================================================================================================##
#   DoomDelogo()  manual:                                                                          #
##================================================================================================##


                        #   Usage:  #
                        #===========#

#  Just add coords same as to Crop(). Both coords systems and odd numbers are supported.
# Examples: DoomDelogo(102,60,262,80)
#           DoomDelogo(102,60,-462,-340)
#           DoomDelogo(102,60,-462,-340, Show=1)


                        #   Optional arguments:  #
                        #==============--------==#

# int "Show"    :  Shows some stuff.
#
#                 0 - Disabled. (Default)
#                 1 - Helps to set coords. Marks area selected by coords.
#                 2 - Shows some dev stuff.


# int "Px"      :  No need to touch it.
#
#                 1 - 1 pixel around coords is used.
#                 2 - 2 pixels around coords are used. (Default)


##================================================================================================##
#   DoomDelogo function:                                                                           #
##================================================================================================##


function DoomDelogo(clip clp, int x1, int x2, int x3, int x4, int "Px", int "Show") {

    Show     = Default(Show, 0)    Assert (!(Show    <  0 || Show    > 3), """"Show" value must be between 0 and 2.""")
    Px       = Default(  Px, 2)    Assert (!(Px      <  1 || Px      > 2), """"Px" value must be between 1 and 2.""")
                                   Assert (!(x1      <  2 || x2      < 2), "Left and Top coords can't be less than 2.")

    clp
    x        = Width(clp)
    isHD     = (x > 720) ? True : False
    matrixID = (isHD == True) ? "Rec709" : "Rec601"
    clpYV24  = clp.ConvertToYV24(matrix=matrixID)
    loc      = clpYV24.Crop(x1,x2,x3,x4)
    locW     = loc.Width
    locH     = loc.Height
    isCS     = (clp.IsYV12 || clp.IsYV16) ? True : False
    not_mod2 = (locW %2 != 0 || locH %2 != 0) ? True : False
    use444   = (isCS && not_mod2) ? True : False
    ratio    = 1.0 * locW / locH
    weight   = (ratio > 1) ? 0.5-(0.5/ratio)+0.5 : 1.0-(0.5-(0.5/ratio)+0.5)

    Assert (!(clp.Width -x1 -locW <  2 || clp.Height -x2 -locH < 2), "Logo must have 2px of video around it.")

    left  = clpYV24.Crop(x1 - px  ,        x2,   px, locH)
    right = clpYV24.Crop(x1 + locW,        x2,   px, locH)
    top   = clpYV24.Crop(       x1, x2 - px  , locW,   px)
    bot   = clpYV24.Crop(       x1, x2 + locH, locW,   px)

    blr = (Px == 2) ? 1.0 : 0.1
    hor = StackHorizontal(left, right).Blur(blr).BilinearResize(locW,locH)
    ver = StackVertical  ( top, bot  ).Blur(blr).BilinearResize(locW,locH)

    grad1  = 47.0               # %
    grad2  = grad1 / (ratio *2)
    grad_ver = (ratio >= 1) ? grad1 * locH / 100.0 : grad2 * locH / 100.0
    grad_hor = (ratio >= 1) ? grad2 * locW / 100.0 : grad1 * locW / 100.0
    grad_ver_clamp = (grad_ver < grad_hor /2) ? grad_hor /2 : grad_ver
    grad_hor_clamp = (grad_hor < grad_ver /2) ? grad_ver /2 : grad_hor

    if (locH < 16 || locW < 16) {
        mix  = Merge(hor, ver, weight)
    } else {
        blk  = BlankClip(height=4, width=1, color_yuv=$000000, pixel_type="Y8", length=1)
        gry1 = BlankClip(height=4, width=1, color_yuv=$555555, pixel_type="Y8", length=1)
        gry2 = BlankClip(height=4, width=1, color_yuv=$AAAAAA, pixel_type="Y8", length=1)
        wht  = BlankClip(height=4, width=1, color_yuv=$FFFFFF, pixel_type="Y8", length=1)
        bgw  = StackHorizontal(wht, gry2, gry1, blk).Blur(0.5)

        hor_l  = bgw.BicubicResize(Round(grad_hor_clamp), locH)
        hor_r  = hor_l.Turn180
        hor_m  = BlankClip(height=locH, width=locW - hor_l.Width *2, color_yuv=$000000, pixel_type="Y8", length=1)
        h_mask = StackHorizontal(hor_l, hor_m, hor_r)

        ver_t  = bgw.TurnRight.BicubicResize(locW, Round(grad_ver_clamp))
        ver_b  = ver_t.Turn180
        ver_m  = BlankClip(height=locH - ver_t.Height *2, width=locW, color_yuv=$000000, pixel_type="Y8", length=1)
        v_mask = StackVertical(ver_t, ver_m, ver_b)

        mix_0 = Merge(hor, ver, weight)
        if (ratio >= 1) {
            mix_v = Overlay(mix_0, ver, 0, 0, v_mask)
            mix_h = Overlay(mix_v, hor, 0, 0, h_mask)
            mix   = mix_h
        } else {
            mix_h = Overlay(mix_0, hor, 0, 0, h_mask)
            mix_v = Overlay(mix_h, ver, 0, 0, v_mask)
            mix   = mix_v
        }
    }

    if (Show == 1) {
        box = BlankClip(width=locW, height=locH, color=$96ff2d, pixel_type="RGB32", length=1)
        end = clp.ConvertToRGB32.Layer(box.Mask(BlankClip(box, color=$444444)), x=x1, y=x2)
    } else if (Show == 2) {
        if (locH < 16 || locW < 16) {
            s1  = StackHorizontal(hor.AddBorders(0,0,2,0,$F0F080), ver).AddBorders(0,0,0,2,$F0F080)
            bk  = BlankClip(mix)
            s2  = StackHorizontal(bk.AddBorders(0,0,2,0,$F0F080), mix).AddBorders(0,0,0,2,$F0F080)
            end = StackVertical(s1, s2)
        } else {
            m1  = h_mask.Subtitle(String(Round(grad_hor_clamp)), size=14).ConvertToYV24
            m2  = v_mask.Subtitle(String(Round(grad_ver_clamp)), size=14).ConvertToYV24
            m   = StackHorizontal(m1.AddBorders(0,0,2,0,$F0F080), m2).AddBorders(0,0,0,2,$F0F080)
            s1  = StackHorizontal(hor.AddBorders(0,0,2,0,$F0F080), ver).AddBorders(0,0,0,2,$F0F080)
            s2  = StackHorizontal(mix_0.Subtitle(LeftStr(String(weight),5),size=14).AddBorders(0,0,2,0,$F0F080), mix).AddBorders(0,0,0,2,$F0F080)
            end = StackVertical(s1, m, s2)
        }
    } else {
        end = clp.Overlay(mix, x1, x2, use444=use444)
    }

    return end
}
