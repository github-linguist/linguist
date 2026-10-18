###
### Logo v14.0  (https://forum.doom9.org/showthread.php?t=182881)
###
###  (25-02-2022) by Dogway
###
### With help from Gavino and Didée             (https://forum.doom9.org/showthread.php?t=160285)
### Watermark code adapted from:Didée's Ylevels (https://forum.doom9.org/showthread.php?p=525465#post525465)
###
### -It is a fast and handy function for adding logos or other type of elements upfront the video image in YV12 space.
### -It will stack on the video using a mask if the image has alpha. Moreover you can blend it using a blending mode.
### -It will only call the logo once, so you might want to add several instances of Logo() for multiple displays.
### -For help on parameters, read the #comments right next to the "Default()" column.
###
### Dependencies:
###     ExTools
###     MaskTools2
###     GradePack
###     ResizersPack
###     TransformsPack
###     Immaavs (for GIFs)
###
### Todo / Known bugs:
###   -Add interlaced support
###   -Fix Chroma in Blending Modes
###   -Fix FadeOut problem when end=Framecount-1
###   -Use dissolve and masktools2 to avoid using Blackness in FadeX()(?)
###   -Wiser trim logic(?)
###   -Add a loop option (repeat every n frames)
###   -Replace frames with seconds (or add option)
###   -Add option to load an external mask
###   -Add horizontal and vertical logo panning option
###   -Add ability to resize logo internally
###   -Better matte workaround
###
### Changelog:
###
### 14.0 -Rebasing to latest AviSynth+ and ExTools versions (now accepts partial hidden logo)
###
### 13.0 -Replace with MatchClip() from TransformsPack, and nmod() and mmod() from ResizersPack
###
### 12.3 -Rebase to latest ex_blend()
###
### 12.2 -Fixed chroma in blend modes
###      -Replaced remaining Expr to ExTools wrappers
###
### 12.1 -Replaced blur() with ex_blur()
###
### 12.0 -Updated Expr with ex_blend()
###      -Replaced mt_inpand() with ex_inpand()
###      -Reverted ex_merge() with faster mt_merge()
###
### 11.1 -Updated to ExTools wrappers, including convolutions
###
### 11.0 -Updated to internal Expr() (faster and HBD support)
###      -Replaced resizers to Bicubic (faster without quality loss)
###      -Cosmetics
###
### 10.2 -Swapped CoronaSequence for ImageSource
###
### 10.1 -Added MT support
###      -Defaulted FadeIn FadeOut to 1 second
###
### 10.0 -Optimized (and fixed) modes code
###
### 9.0c -Changed DAR parameter to PAR
###      -Added option to copy over logo chroma in screen/multiply modes
###      -Minor tweaks
###
### 9.0  -Fixed minor bugs
###
### 8.1  -Corrected bug for logo only first frame
###
### 8.0  -Fixed frame alignment bug
###      -Others
###
### 7.1  -Optimized mmod2 function
###      -Fixed some bugs for "end" parameter
###      -Other minor adjustments
###
### 6.0  -Added DAR flag for anamorphic sources
###      -Added watermark "blending" mode
###      -Fixed FadeIn/Out bug
###      -Other minor adjustments
###
### 5.0  -Fixed and Optimized code (Working Release)
###
### 4.0  -Code rethinking
###      -Fixed code
###      -Added Over blending mode
###      -Added option for matte carving
###
### 3.0  -Automatic alpha detection
###      -Fixed and optimized code
###      -Changed parameters
###      -Added option for animated gifs
###      -Added Multiply blending mode
###
### 2.0  -Fixed and optimized code
###      -Added alpha option
###
### 1.5  -Added logo masking for images with alpha
###      -Added blur parameter
###      -Optimized code
###
### 1.0  -Initial release (26-03-2011)
###
###
###
### Usage:
###
###      Logo("C:\MyLogo.png",x=30,y=30,start=200,end=300)
###      Logo("C:\MyAnimatedLogo00%02d.png",30,30,1510,2000,30,30,Opac=0.8,blur=0.2,mode="Screen",chr=true,PAR=0.911)
###
########################################################################


function Logo (clip c, string path, int "x", int "y", int "start", int "end", int "I", int "O", float "Opac",
\              string "mode", float "Wstr", bool "Chr", float "blur", bool "matte", float "PAR", bool "anim_gif") {

    w  = c.Width ()
    h  = c.Height()
    Fc = c.FrameCount()-1
    fps= c.Framerate ()


    path     = String(path)            # Your logo PATH
    x        = Default(x, 0)           # Horizontal offset for logo placement from top,left corner
    y        = Default(y, 0)           # Vertical offset for logo placement from top,left corner
    start    = Default(start, 0)       # Define the start frame for your logo
    end      = Default(end, 0)         # Define the end frame for your logo
    I        = Default(I, int(fps))    # Length in frames for your logo FadeIn
    O        = Default(O, I)           # Length in frames for your logo FadeOut
    Opac     = Default(Opac, 1.0)      # Opacity for your logo
    mode     = Default(mode,"Over")    # Blending mode, it can be Screen, Multiply, Over and Watermark (using alpha)
    Wstr     = Default(Wstr, 1.2)      # Intensity of Watermark, over 1.0 for bright, and below for dark watermaks
    Ch       = Default(Chr,  false)    # Enables displaying chroma in Screen and Multiply mode, it is just copied not processed
    Sblur    = Default(blur, 0.0)      # Post-blur your logo for better integration
    mat      = Default(matte,false)    # Matte carving for better masking at edges of images with alpha
    PAR      = Default(PAR,1.0)        # Indicate the PAR of your source, if anamorphic output, so logo will show correct at display
    gif      = Default(anim_gif,false) # Is your logo a packed animated gif?

    !(mode == "Over" || mode == "Watermark" || mode == "Screen"  || mode == "Multiply") ? \
    Assert(false,           "Logo: Unsupported Blending Mode: " +string(mode))  : nop()
    end > 0 ?\
    Assert(start <= end,     "Logo: Invalid start frame: "       +String(start)) : nop()
    Assert(start <=  Fc,     "Logo: Start frame out of bounds: " +String(start))
    Assert(start >=   0,     "Logo: Start frame out of bounds: " +String(start))
    Assert(end   < Fc+1,     "Logo: End frame out of bounds: "   +String(end))
    Assert(end   >=  -1,     "Logo: End frame out of bounds: "   +String(end))
    end2 = end == 0 || end == Fc-1 ? Fc : end


    logo0 = gif ? ImmaRead(   path, start=start, end= end == -1 ? start : end2, animation=true)
    \           : ImageSource(path, start=start, end= end == -1 ? start : end2, use_Devil=true, pixel_type="RGB32")


    logo0 = isRGB(logo0) ? isPlanarRGB(logo0) ? logo0 : logo0.ConvertToPlanarRGBA() : logo0

    logo0 = PAR !=  1.0  ? logo0.BicubicResize(nmod(logo0.Width()/par,2),nmod(logo0.Height(),2),0.0,0.75)
    \                    : logo0.mmod(2)

    lw = logo0.Width ()    mw = mat ? lw : nop()
    lh = logo0.Height()    mh = mat ? lh : nop()

    logo0 = logo0.addborders(0,0,w,h).PadResize(w,h,2,"$108080",x+lw/2,y+lh/2).ex_blur(Sblur/par,Sblur,UV=Ch?3:2)

    msk   = mat ? ExtractA(logo0.FadeIn0(I).FadeOut0(O)).PointResize(mw*2,mh*2).ex_inpand(2,mode="plus").BicubicResize(mw,mh,-0.5,0.25).MatchClip(c)
    \           : ExtractA(logo0.FadeIn0(I).FadeOut0(O)).MatchClip(c)
    logo0 = logo0.MatchClip(c).AssumeFPS(fps)


    V = c.trim(start,end)
    V =
    \ (mode=="Over")     ? mt_merge(V,logo0,ex_lut(msk,Format("x {Opac} *"),UV=1), luma=ch)
    \:(mode=="Watermark")? mt_merge(V,ex_lut(V,Format("x ymin - ymax ymin - A@ / 1 {Wstr} {Opac} ^ / ^ A * ymin +"),UV=1), msk)
    \:                     mt_merge(V,ex_blend(V,logo0,mode,Opac,1,true,UV=ch?4:2), msk, luma=ch)


    start == 0 && (end == 0 || end == Fc) ? V :
    \ (start>0 ?  (end == 0 || end == Fc || end == -1 && start == Fc ? c.trim(0,start == 1 ? -1 : start-1)+V :
     \ c.trim(0,start == 1 ? -1 : start-1)+V+c.trim(end == -1 ? start+1 : end2+1,0)) :
      \ V+c.trim(end == -1 ? start+1 : end2+1,0)) }
