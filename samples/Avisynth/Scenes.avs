input = LSMASHVideoSource("episode.mov").AssumeTFF().Crop(0, 6, 0, -2)

DI = input.QTGMC(preset="draft").TDecimate(mode=2, rate=23.976)

DI

# source as 'last' (your QTGMC + TDecimate output)
global super = last.MSuper(pel=2, sharp=2)
global vec = MAnalyse(super, isb=false, blksize=16, delta=1)
global sc = MSCDetection(last, vec, thSCD1=400, thSCD2=130)

# sc is a clip where bright = scene change
ScriptClip(last, """
    score = AverageLuma(sc)
    score > 30 \
        ? Subtitle("Change " + String(score), size=48, text_color=$00FFFF00) \
        : Subtitle(String(score), size=24)
""")
