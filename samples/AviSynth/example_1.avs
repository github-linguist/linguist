LSMASHVideoSource(%src%)
Colorbars.ShowFrameNumber

ConvertToYV24

V0 = Last
V1 = FlipVertical
V2 = FlipHorizontal
V3 = TurnRight.BilinearResize(Width,Height)
V4 = TurnLeft.BilinearResize(Width,Height)
V5 = Invert

NickNames ="""
    ButlinsBytheSea_76       = 0
    PontinsRainedOut_77      = 1
    BultinsBognorRegis_78    = 2
    BultinsSkegness_79       = 3
    PontinsCamberSands_80    = 4
    PontinsLowestoft_81      = 5
"""

SCMD="""
    ButlinsBytheSea_76       500,600
    PontinsRainedOut_77      900,1000
    BultinsBognorRegis_78    1400,1600
    BultinsSkegness_79       2000,2400
    PontinsCamberSands_80    2640,3000
    PontinsLowestoft_81      4400,4800
    PontinsRainedOut_77      5000,5400
    5                        6000,6500
    1                        6501,7000
"""

SHOW=True

Prune(V0,V1,V2,V3,V4,V5,scmd=SCMD,nickname=NickNames,show=SHOW,FadeIn=True,FadeSplice=True,FadeOut=True)
ConvertToRGB32
