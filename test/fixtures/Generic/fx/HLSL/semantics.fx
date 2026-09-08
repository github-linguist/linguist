float4x4 WorldViewProjection;

struct VertexOutput
{
    float4 clipPosition : SV_Position;
};

VertexOutput TransformVertex(float4 position : POSITION)
{
    VertexOutput result;
    result.clipPosition = mul(position, WorldViewProjection);
    return result;
}
