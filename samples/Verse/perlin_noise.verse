using. /Verse.org/Simulation
using. /UnrealEngine.com/Temporary/SpatialMath

using. Goo
using. Vertex

Vec2DotProduct<public>(Dist:vector2, Gradient:vector2)<computes>:float= Dist.X * Gradient.X + Dist.Y * Gradient.Y
SmoothStep<public>(Value:float)<computes>:float= Value * Value * (3.0 - 2.0 * Value)
Hash<public>(Seed:int, X:int, Y:int)<computes>:float=
    Max32Bit := 4294967296
    NormalizeFactor := 4294967295
    A := 1664525
    B := 1013904223
    C := 69069

    First := Mod[(Seed * A + X * C + B), Max32Bit] or 1
    Second := Mod[(First * A + Y * C + B), Max32Bit] or 1
    Value := Mod[(Second * A + B), Max32Bit] or 1
    return (Value*1.0) / (NormalizeFactor*1.0)
RandomUnitVector<public>(Seed:float)<computes><reads>:vector2=
    Theta := Seed * 2.0 * PiFloat
    V2(Cos(Theta), Sin(Theta))
PerlinNoise<public>(Seed:int, Scale:float, Coord:vector2)<computes><reads>:float=
    Scaled := Coord / Scale
    X0 := Floor[Scaled.X] or 0
    Y0 := Floor[Scaled.Y] or 0
    X1 := X0 + 1
    Y1 := Y0 + 1

    # Get Gradients at Four Corners
    G00 := RandomUnitVector(Hash(Seed, X0, Y0))
    G10 := RandomUnitVector(Hash(Seed, X1, Y0))
    G01 := RandomUnitVector(Hash(Seed, X0, Y1))
    G11 := RandomUnitVector(Hash(Seed, X1, Y1))

    # Get Fractional Parts
    FX := Scaled.X - X0*1.0
    FY := Scaled.Y - Y0*1.0

    # Compute Dot Products
    D00 := Vec2DotProduct(V2(FX, FY), G00)
    D10 := Vec2DotProduct(V2(FX-1.0, FY), G10)
    D01 := Vec2DotProduct(V2(FX, FY-1.0), G01)
    D11 := Vec2DotProduct(V2(FX-1.0, FY-1.0), G11)

    # Interp along X
    WX := SmoothStep(FX)
    N0 := Lerp(D00, D10, WX)
    N1 := Lerp(D01, D11, WX)

    # Interp along Y
    WY := SmoothStep(FY)

    return Lerp(N0, N1, WY)

Float_Noise<public><constructor>(Focus:string, Tweak:string, Braid:string)<transacts> := prop_float_noise{ Focus := Path(Focus), Tweak := Path(Tweak), Braid := Path(Braid) }
prop_float_noise<public> := class<concrete>(prop_float):
    @editable FN<override>:string = "PerlinNoise"
    ValueFor<override>(L_:scoped_state):float_goo=
        FloatGoo of Entwine(L_.ToInt(Focus), L_.ToFloat(Tweak), L_.ToVec2(Braid), PerlinNoise)
    # Compute<override>(L_:context):float_goo=
    #     FloatGoo of Map3(PerlinNoise, (L_.Int of Focus, L_.Float of Tweak, L_.Vec2 of Braid))