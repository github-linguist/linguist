float4x4 matWorldView : WORLDVIEW;
float4x4 matWorldViewProjection : WORLDVIEWPROJECTION;

struct VS_INPUT {
	float4 Position : POSITION0;
	float3 Normal : NORMAL;
	float3 Tangent : TANGENT;
	float3 Binormal : BINORMAL;
	float2 TexCoord0 : TEXCOORD0;
	float2 TexCoord1 : TEXCOORD1;
};

struct VS_OUTPUT {
	float4 Position : POSITION0;
	float2 TexCoord0 : TEXCOORD0;
	float2 TexCoord1 : TEXCOORD1;
	float3x3 TangentToView : TEXCOORD2;
};

VS_OUTPUT vs_main(VS_INPUT input)
{
	VS_OUTPUT output;
	output.Position = mul(input.Position, matWorldViewProjection);
	output.TexCoord0 = input.TexCoord0 * 5;
	output.TexCoord1 = input.TexCoord1;
	output.TangentToView[0] = mul(float4(input.Tangent, 0), matWorldView).xyz;
	output.TangentToView[1] = mul(float4(input.Binormal, 0), matWorldView).xyz;
	output.TangentToView[2] = mul(float4(input.Normal, 0), matWorldView).xyz;
	return output;
}

struct PS_OUTPUT {
	float4 gbuffer0 : COLOR0;
	float4 gbuffer1 : COLOR1;
};

texture albedo_tex;
sampler albedo_samp = sampler_state {
	Texture = (albedo_tex);
	MipFilter = Linear;
	MinFilter = Linear;
	MagFilter = Linear;
	AddressU = Wrap;
	AddressV = Wrap;
	sRGBTexture = True;
};

texture normal_tex;
sampler normal_samp = sampler_state {
	Texture = (normal_tex);
	MipFilter = Linear;
	MinFilter = Linear;
	MagFilter = Linear;
	AddressU = Wrap;
	AddressV = Wrap;
	sRGBTexture = False;
};

texture specular_tex;
sampler specular_samp = sampler_state {
	Texture = (specular_tex);
	MipFilter = Linear;
	MinFilter = Linear;
	MagFilter = Linear;
	AddressU = Wrap;
	AddressV = Wrap;
	sRGBTexture = True;
};

texture ao_tex;
sampler ao_samp = sampler_state {
	Texture = (ao_tex);
	MipFilter = Linear;
	MinFilter = Linear;
	MagFilter = Linear;
	AddressU = Wrap;
	AddressV = Wrap;
	sRGBTexture = True;
};

PS_OUTPUT ps_main(VS_OUTPUT Input)
{
	PS_OUTPUT o;

	float3 tangentNormal = normalize(tex2D(normal_samp, Input.TexCoord0).xyz * 2 - 1);
	float3 eyeNormal = normalize(mul(tangentNormal, Input.TangentToView));

	float3 albedo = tex2D(albedo_samp, Input.TexCoord0).rgb;
	float ao = tex2D(ao_samp, Input.TexCoord1).r * 0.75;
	float spec = tex2D(specular_samp, Input.TexCoord0).r;

	o.gbuffer0 = float4(eyeNormal, spec * ao);
	o.gbuffer1 = float4(albedo, 1 - ao);
	return o;
}

technique mesh {
	pass Geometry {
		VertexShader = compile vs_3_0 vs_main();
		PixelShader  = compile ps_3_0 ps_main();

		AlphaBlendEnable = False;
		ZWriteEnable = True;
	}
}
