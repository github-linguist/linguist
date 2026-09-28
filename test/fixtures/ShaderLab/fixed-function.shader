Shader "Hidden/DepthOnly" // No programmable shader stage.
{
    subshader
    {
        Pass
        {
            ZWrite On
            ColorMask 0
        }
    }
}
