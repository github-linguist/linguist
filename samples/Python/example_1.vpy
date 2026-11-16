
eval("{{c}}")
clip = vs.get_output()
clip_yuv16 = core.resize.Bicubic(clip, format=vs.YUV444P16)
x_plane_clip = core.std.ShufflePlanes(clip_yuv16, planes={{x}}, colorfamily=vs.GRAY)
x_plane_clip.set_output()
