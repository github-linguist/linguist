# allows finding the lumScale by visually comparing a pq stream with a sdr stream.
# try different values of the lumaScaling variable such that the black-and-white picture matches as much as possible.
# needs a pq2hlg cube file. can be created using DoViLutGen.exe: DoViLutGen.exe pq2hlg.cube -s 65 -i 0 -o 0
# also need masktools2, avsresize and BetterGrayscale.avsi
# you might need to use the trim command so that the frames of the two streams match exactly

LoadPlugin("avsresize.dll")
LoadPlugin("masktools2.dll")
Import("BetterGrayscale.avsi")
LoadPlugin("DGDecodeNV.dll")


# Load PQ stream
pq=DGSource("pq.dgi")


# Load full enhancement layer, if existing
fel=NOP()
#fel=DGSource("fel.dgi")


# Load RPU file, if existing
rpuFile=""
#rpuFile="RPU.bin"


# Load SDR stream
sdr=DGSource("sdr.dgi")


# Set the desired scaling here. Can be folating point numbers, usually between 0.5-5.0.
lumaScaling=1.5


# Set the path to the pq2hlg cube file
cubePath="PQ2HLG.cube"


#Convert PQ to HLG
pq=pq.z_ConvertFormat(pq.Width()/2,pq.Height()/2)
pq=(rpuFile!="")?IsClip(fel)?DoViBaker(pq,fel,rpu=rpuFile).ConvertToYUV420(matrix="2020ncl"):DoViBaker(pq,rpu=rpuFile).ConvertToYUV420(matrix="2020ncl"):pq
pq=pq.ConvertBits(16)
sdr=sdr.z_ConvertFormat(pq.Width(),pq.Height())
sdr=sdr.ConvertBits(16)

hlt=pq.z_ConvertFormat(pixel_type="RGBP16",colorspace_op="2020ncl:st2084:2020:limited=>rgb:st2084:2020:full",chromaloc_op="top_left=>center",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hln=hlt.AVSCube(cubePath)
hln=hln.z_ConvertFormat(pixel_type="YUV420P16",colorspace_op="rgb:std-b67:2020:full=>2020ncl:std-b67:2020:limited",chromaloc_op="center=>top_left",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hlt=hlt.DoViTonemap(lumScale=lumaScaling, masterMaxNits=1000, targetMaxNits=1000, targetMinNits=0, masterMinNits=0, kneeOffset=0.5, normalizeOutput=false)
hlt=hlt.AVSCube(cubePath)
#in case HDR was mastered in the 2010s green/tiel look. The movie Bourne Identity is such a case, Terminator is another.
#hlt=hlt.ApplyGradationCurves(greenPoints="0,0,129,125,255,255")
hlg=hlt.z_ConvertFormat(pixel_type="YUV420P16",colorspace_op="rgb:std-b67:2020:full=>2020ncl:std-b67:2020:limited",chromaloc_op="center=>top_left",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
#in case HDR was wrongly mastered, and its light highlights don't go higher than for the SDR, fake the HLG. The movies HEAT, Terminator and BladeRunner 2049 are such cases.
#hlg=hlg.ColorYUV(levels="TV->PC").MT_lut("x range_half < x x 2.85134733 x * x * range_max / range_max / 2.812773138 x * range_max / - 1.693693833 + * ?").ColorYUV(levels="PC->TV")

#calculate out-of-color gamut for bt709
hls=hlt.z_ConvertFormat(colorspace_op="rgb:std-b67:2020:full=>rgb:std-b67:709:full",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hls=hls.z_ConvertFormat(colorspace_op="rgb:std-b67:709:full=>rgb:std-b67:2020:full",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hls=hls.z_ConvertFormat(pixel_type="YUV420P16",colorspace_op="rgb:std-b67:2020:full=>2020ncl:std-b67:2020:limited",chromaloc_op="center=>top_left",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
#calculate overbrightness for bt709
hls=hls.ColorYUV(levels="TV->PC").MT_lut("x range_half < x x 2.8188 x * x * range_max / range_max / 2.7718 x * range_max / - 1.6812 + * ?")
hls=hls.MT_lut("x range_half < x x 0.4438 x * x * range_max / range_max / 0.1249 x * range_max / - 0.9443 + / ?").ColorYUV(levels="PC->TV")

#calculate out-of-color gamut for dci-p3
hld=hlt.z_ConvertFormat(colorspace_op="rgb:std-b67:2020:full=>rgb:std-b67:display-p3:full",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hld=hld.z_ConvertFormat(colorspace_op="rgb:std-b67:display-p3:full=>rgb:std-b67:2020:full",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
hld=hld.z_ConvertFormat(pixel_type="YUV420P16",colorspace_op="rgb:std-b67:2020:full=>2020ncl:std-b67:2020:limited",chromaloc_op="center=>top_left",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")

#Assuming Display-Referred HLG-Conversion with 89% SDR = 75% HLG == 203nits, scale HLG signal to match the SDR signal
hlp=hlg
hlp=hlp.ColorYUV(levels="TV->PC").MT_lut("x range_half < x x 2.8188 x * x * range_max / range_max / 2.7718 x * range_max / - 1.6812 + * ?").ColorYUV(levels="PC->TV")
hlc=highContrastHlg ? hlg : hlp

#show preview in monitors colorspace
sdp=sdr.z_ConvertFormat(colorspace_op="709:709:709:limited=>709:srgb:709:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a srgb monitor")
#sdp=sdr.Subtitle(y=20,"to be viewed on a bt709 monitor @2.4 gamma")
#sdp=sdr.z_ConvertFormat(colorspace_op="709:709:709:limited=>709:709:st432-1:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a dci-p3 monitor @2.4 gamma")
#sdp=sdr.z_ConvertFormat(colorspace_op="709:709:709:limited=>709:709:2020:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a bt2020 monitor @2.4 gamma")

hlp=hlp.z_ConvertFormat(colorspace_op="709:709:2020:limited=>709:srgb:709:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a srgb monitor")
#hlp=hlp.z_ConvertFormat(colorspace_op="709:709:2020:limited=>709:709:709:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a bt709 monitor @2.4 gamma")
#hlp=hlp.z_ConvertFormat(colorspace_op="709:709:2020:limited=>709:709:st432-1:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a dci-p3 monitor @2.4 gamma")
#hlp=hlp.Subtitle(y=20,"to be viewed on a bt2020 monitor @2.4 gamma")

##just out of curiosity, fake HLG from the SDR by just leaving the bightness as is and correctly converting the colors
#sdf=sdr.z_ConvertFormat(colorspace_op="709:std-b67:709:limited=>2020ncl:std-b67:2020:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36")
##make a preview of how this fake HLG will look like
#sdp=sdf.z_ConvertFormat(colorspace_op="2020ncl:std-b67:2020:limited=>709:std-b67:709:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a srgb monitor")
#sdp=sdf.z_ConvertFormat(colorspace_op="2020ncl:std-b67:2020:limited=>709:std-b67:st432-1:limited",dither_type="none",resample_filter="spline36",resample_filter_uv="spline36").Subtitle(y=20,"to be viewed on a dci-p3 monitor")
#sdp=sdp.ColorYUV(levels="TV->PC").MT_lut("x range_half < x x 2.8188 x * x * range_max / range_max / 2.7718 x * range_max / - 1.6812 + * ?").ColorYUV(levels="PC->TV")

#clipping highlights in comparison and conversion to grayscale
upperClippingPoint=(1-0.5)*256
#sdc=sdr.Grayscale().ColorYUV(levels="TV->PC").ColorYUV(off_y=upperClippingPoint).ColorYUV(off_y=-upperClippingPoint).ColorYUV(levels="PC->TV")
#hlc=hlc.Grayscale().ColorYUV(levels="TV->PC").ColorYUV(off_y=upperClippingPoint).ColorYUV(off_y=-upperClippingPoint).ColorYUV(levels="PC->TV")
sdc=sdr.better709Grayscale().ColorYUV(levels="TV->PC").ColorYUV(off_y=upperClippingPoint).ColorYUV(off_y=-upperClippingPoint).ColorYUV(levels="PC->TV")
hlc=hlc.betterHlgGrayscale().ColorYUV(levels="TV->PC").ColorYUV(off_y=upperClippingPoint).ColorYUV(off_y=-upperClippingPoint).ColorYUV(levels="PC->TV")

pq=pq.Subtitle("original PQ")
hlc=Subtitle(hlc,"clipped HLG")
sdc=Subtitle(sdc,"clipped SDR")
sdr=Subtitle(sdr,"original SDR")
sdp=Subtitle(sdp,"preview SDR")
hlp=Subtitle(hlp,"preview HLG")
hln=Subtitle(hln,"HLG")
hlg=Subtitle(hlg,"final HLG")
hls=Subtitle(hls,"final HLG out of range for BT.709")
hld=Subtitle(hld,"final HLG out of range for HLG with DCI-P3 primaries")

#Comparison
StackHorizontal(Interleave(sdc,hlc),Interleave(sdp,hlp),Interleave(sdr,hlg),Interleave(pq,hln),Interleave(Subtract(hlg,hls),Subtract(hlg,hld))).SelectEvery(4,0,1).ConvertBits(10,dither=-1)
