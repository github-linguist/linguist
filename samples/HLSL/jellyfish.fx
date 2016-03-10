float4x4 matWorldViewProjection : WORLDVIEWPROJECTION;
float4x4 matWorldView : WORLDVIEW;
float4x4 matWorld : WORLD;
float4x4 matView : VIEW;

uniform float4 vViewPosition;

struct VS_INPUT 
{
	float3 Pos:      POSITION;
	float3 Normal:   NORMAL;
	float3 Tangent:  TANGENT;
	float3 Binormal: BINORMAL;
};

struct VS_OUTPUT 
{
	float4 Pos        : POSITION;
	float3 reflection : TEXCOORD1; 
	float3 refraction : TEXCOORD2; 
	float  fresnel    : TEXCOORD3;
};

uniform float3 amt;
uniform float3 scale;
uniform float3 phase;

float3 deform(float3 p)
{
	float s = 3;
	float3 p2 = p * scale + phase;
	s += sin(p2.x) * amt.x;
	s += sin(p2.y) * amt.y;
	s += sin(p2.z) * amt.z;
	return p * s / 3;
}

VS_OUTPUT vs_main( VS_INPUT In )
{
	VS_OUTPUT Out;

	float3 pos = In.Pos;
	float3 norm = In.Normal;

	float3 p1 = pos + In.Tangent * 0.05;
	float3 p2 = pos + In.Binormal * 0.05;
	pos = deform(pos);
	p1  = deform(p1);
	p2  = deform(p2);

	p1 -= pos;
	p2 -= pos;
	norm = normalize(cross(p1, p2));

	float3 view = normalize(pos - vViewPosition.xyz);

	Out.Pos           = mul(float4(pos,  1.0), matWorldViewProjection);
	Out.reflection    = reflect(view, norm);
	Out.refraction    = reflect(view, norm * 0.4f); /* fake, but who cares? */
	Out.fresnel       = dot(view, norm);
	norm = mul(float4(norm, 0.0), matWorldViewProjection);

	return Out;
}

#define PS_INPUT VS_OUTPUT

#if 0
textureCUBE reflectionMap;
samplerCUBE reflectionMapSampler = sampler_state
{
	Texture = (reflectionMap);
	MipFilter = LINEAR;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
};
#else
// textures
texture reflectionMap 
< 
    string type = "CUBE";
    string name = "test_cube.dds";
>;

samplerCUBE reflectionMapSampler = sampler_state
{
	Texture = (reflectionMap);
	MipFilter = LINEAR;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
};
#endif

struct PS_OUTPUT 
{
   float4 color    : COLOR0;
};

PS_OUTPUT ps_main( PS_INPUT In )
{
	PS_OUTPUT Out;
	
	float4 reflection = texCUBE(reflectionMapSampler, normalize(In.reflection)) * 1.5;
	float4 refraction = texCUBE(reflectionMapSampler, normalize(In.refraction));
	float fresnel = In.fresnel;
//	float fresnel = abs(normalize(In.normal).z);
	Out.color = lerp(reflection, refraction, fresnel) *  pow(1.0 - fresnel * 0.75, 1.0);

	return Out;
}

technique blur_ps_vs_2_0
{
	pass P0
	{
		VertexShader = compile vs_2_0 vs_main();
		PixelShader  = compile ps_2_0 ps_main();
	}
}
