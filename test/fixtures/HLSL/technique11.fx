#include "shadow.fxh"

technique11 DrawShadow
{
    pass Depth
    {
        SetVertexShader(CompileShader(vs_5_0, ShadowVertex()));
        SetPixelShader(NULL);
    }
}
