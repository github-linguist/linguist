ConvertBits(12)
ConvertToPlanarRGB(matrix="Rec709",interlaced=false)
ConvertToYUV444(matrix="PC.709", interlaced=false,chromaresample="lanczos4")

last = AntiRingLR2(last, "luma", weightL=0.23,offsetL=1,weightR=0.3,offsetR=1,knee=0.1,pr2=0.3,pr3=0.25)

ConvertBits(8, dither=1)
ConvertToYV12(chromaresample="lanczos4", matrix="Rec709", interlaced=false) # test

Prefetch(4)
