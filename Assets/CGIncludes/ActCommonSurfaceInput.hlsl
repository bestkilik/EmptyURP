#ifndef UNITY_ACT_COMMON_SURFACE_INPUT_INCLUDED
#define UNITY_ACT_COMMON_SURFACE_INPUT_INCLUDED

struct SurfaceOutputAdv
{
	half3 worldTangnet;
	half3 worldBinormal;
	half3 worldNormal;
	half3 pixelNormal;
	half3 worldView;
	half3 worldPos;
	half3 localPos;
	float atten;
	half3 bakedGI;
	//////////////////////
	float3 LightDir;
	float3 HalfVec;
	float LdN;
	float HdN;
	float VdH;
	float VdN;
};

struct aseData
{
	half3 baseColor;
	half metallic;
	half smoothness;
	half specular;
	half3 emissive;
	half opacity;
	half opacityMask;
	half3 normal;
	half refraGlossy; 
	half4 filterColor;
	half ior;
	half4 innerColor;
	half occlusion;
	half3 indirectDiffuse;
    half3 indirectSpecular;
};
	
#endif // UNITY_ACT_COMMON_SURFACE_INPUT_INCLUDED