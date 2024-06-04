#ifndef UNITY_ACT_COMMON_SURFACE_INCLUDED
#define UNITY_ACT_COMMON_SURFACE_INCLUDED
#include "LocalPackages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
#include "LocalPackages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
#include "Assets/CGIncludes/ActCommonSurfaceInput.hlsl"
struct appdata {
	float4 vertex : POSITION;
	float3 normal : NORMAL;
	float4 tangent : TANGENT;
	float4 texcoord0 : TEXCOORD0;
	float4 texcoord1 : TEXCOORD1;
	float4 texcoord2 : TEXCOORD2;
	/*ase_vdata:p=p;uv0=tc0.xy;uv1=tc1.xy;uv2=tc2.xy;n=n;t=t*/	
	UNITY_VERTEX_INPUT_INSTANCE_ID
};

struct v2f
{
	float4 pos     : SV_POSITION;
	float4 uv      : TEXCOORD0;
	float4 tSpace0 : TEXCOORD1;
	float4 tSpace1 : TEXCOORD2;
	float4 tSpace2 : TEXCOORD3;
	DECLARE_LIGHTMAP_OR_SH(lightmapUV, vertexSH, 4);
	#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
	float4 shadowCoord              : TEXCOORD5;
	#endif
	float4 wPos    : TEXCOORD6;
	float4 uv2      : TEXCOORD7;
	float4 ase_texcoord8 : TEXCOORD8;
	/*ase_interp(8,):sp=sp.xyzw;uv0=tc0.xy;uv1=tc0.zw;wn=tc3.xyz;wt=tc1.xyz;wbt=tc2.xyz;wp=tc6.xyz*/			
	
	UNITY_VERTEX_INPUT_INSTANCE_ID
	UNITY_VERTEX_OUTPUT_STEREO
};
half D_GGX(half roughness, half HdN, half LdH2)
{
	half a = roughness * roughness;
	half a2 = a * a;
	half normalizationTerm = a * 4.0 + 2.0;
	half d = (HdN * a2 - HdN) * HdN + 1;
	return a2 / (d*d*max(0.1, LdH2)*normalizationTerm);
}

inline void lightingAdv_GI(BRDFData brdfData, SurfaceOutputAdv s, inout aseData ase, Light mainLight)
{
	MixRealtimeAndBakedGI(mainLight, s.pixelNormal, s.bakedGI);
	half3 normalWS = s.pixelNormal;
	half3 viewDirectionWS = s.worldView;
	half3 reflectVector = reflect(-s.worldView, normalWS);
    half NoV = saturate(dot(normalWS, viewDirectionWS));
    half fresnelTerm = Pow4(1.0 - NoV);

    ase.indirectDiffuse = s.bakedGI * ase.occlusion;
    ase.indirectSpecular = GlossyEnvironmentReflection(reflectVector, brdfData.perceptualRoughness, ase.occlusion);

	// return EnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

inline half4 lightingAdv(Light light, SurfaceOutputAdv s, aseData ase)
{
	//half roughness = 1.0 - ase.smoothness;
	half roughness = 0.9 - 0.8 * ase.smoothness;
	half3 DielectricSpecular = kDielectricSpec.rgb * ase.specular * 2.0;
	half oneMinusReflectivity = OneMinusReflectivityMetallic(ase.metallic);
	half grazingTerm = saturate(ase.smoothness + (1 - oneMinusReflectivity));
	half perceptualRoughness = PerceptualSmoothnessToPerceptualRoughness(ase.smoothness);
	
	half3 diffColor = ase.baseColor * oneMinusReflectivity;
	half3 specColor = (DielectricSpecular - DielectricSpecular * ase.metallic) + ase.baseColor * ase.metallic;
	
	half invVdN = 1 - s.VdN;
	half sLdN = saturate(s.LdN);
	
	half difLit = smoothstep( -1 , 1 , s.LdN);

	// half speLit = D_GGX(roughness, s.HdN, sLdN, s.VdN);
	half speLit = D_GGX(roughness, s.HdN, s.VdH*s.VdH);
	// speLit = clamp(speLit, 0.0, 100.0);
	speLit *= sLdN;
	half Vc = invVdN * invVdN;
	half fresnel = (1.0 - 0.75*roughness)*(Vc * 12.5) + 1;
	
	half transLevel = 1 - ase.opacity;

	half transMask = ase.opacity;

	half3 tmpDir = reflect(-s.pixelNormal, s.worldView);

	half refraRoughness = PerceptualSmoothnessToPerceptualRoughness(ase.refraGlossy);
	half eta = 1.0 / ase.ior;
	half rMip = PerceptualRoughnessToMipmapLevel(refraRoughness);
	float3 refraDir = refract(-s.worldView, s.pixelNormal, eta);
	half3 refraEnv = DecodeHDREnvironment(SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0, samplerunity_SpecCube0, refraDir, rMip), unity_SpecCube0_HDR).rgb;
	half3 filterColor = ase.filterColor.rgb;
	half overRefra = saturate(dot(-refraDir, s.pixelNormal));
	overRefra *= overRefra;
	overRefra *= overRefra;

	half surfaceReduction;
	#ifdef UNITY_COLORSPACE_GAMMA
		surfaceReduction = 1.0 - 0.28*roughness*perceptualRoughness;      // 1-0.28*x^3 as approximation for (1/(x^4+1))^(1/2.2) on the domain [0;1]
	#else
		surfaceReduction = 1.0 / (roughness*roughness + 1.0);           // fade \in [0.5;1]
	#endif
	
	half3 LightColor = light.color.rgb * s.atten;

	half3 color = difLit * diffColor;
	color *= LightColor;
	
	half innerFalloff = saturate(s.VdN);
	innerFalloff *= innerFalloff;
	innerFalloff *= innerFalloff;
	// #if defined(UNITY_PASS_FORWARDBASE)
		half3 refraColor = refraEnv.rgb * filterColor;
	// #else
	// 	half3 refraColor = half3(0, 0, 0);
	// #endif

	half dyOcc = ase.occlusion * 0.786 + 0.214;

	color += ase.indirectDiffuse * diffColor;
	color = lerp(refraColor * overRefra, color, transMask) * dyOcc;
	color += saturate(speLit * specColor * fresnel * LightColor * dyOcc);
	// color += surfaceReduction * gi.indirect.specular * specColor;
	half fresnelTerm = Pow4(1.0 - s.VdN);
	color += surfaceReduction * ase.indirectSpecular * lerp (specColor, grazingTerm, fresnelTerm);
	color.rgb = clamp(color.rgb, 0.0, 10.0);
	return half4(color, 1.0);
}
void InitializeInputData(v2f input, SurfaceOutputAdv o, out InputData inputData)
{
    inputData = (InputData)0;

    inputData.positionWS = o.worldPos;

    inputData.normalWS = o.worldNormal;

    inputData.normalWS = NormalizeNormalPerPixel(o.pixelNormal);
    inputData.viewDirectionWS = o.worldView;

#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
    inputData.shadowCoord = input.shadowCoord;
#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
    inputData.shadowCoord = TransformWorldToShadowCoord(inputData.positionWS);
#else
    inputData.shadowCoord = float4(0, 0, 0, 0);
#endif

    inputData.bakedGI = SAMPLE_GI(input.lightmapUV, input.vertexSH, inputData.normalWS);
    inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(input.pos);
    inputData.shadowMask = SAMPLE_SHADOWMASK(input.lightmapUV);
}
			
#endif // UNITY_ACT_COMMON_SURFACE_INCLUDED