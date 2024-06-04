#ifndef REX_CG_INCLUDED
#define REX_CG_INCLUDED
#include "RexFog.cginc"
uniform float UseAdvFog;
uniform float Use2ndFog;
uniform float UseHeightFog;
uniform float StartPos2ndFog;
uniform float FogLength2ndFog;
uniform float4 FogColor2ndFog;
// uniform float4 HeightFogParms; //x:height y:FdC z:FdC<=0 w:FogDensity
uniform float HeightFogHeight;
uniform float HeightFogDensity;
uniform float4 HeightFogColor;

uniform float DistFadeNear = 0.1;
uniform float DistFadeFar = 0.3;

inline float DitherBayer(float2 xy)
{
	int x = fmod(xy.x, 8);
	int y = fmod(xy.y, 8);
	
	const float dither[ 64 ] = {
		 1, 49, 13, 61,  4, 52, 16, 64,
		33, 17, 45, 29, 36, 20, 48, 32,
		 9, 57,  5, 53, 12, 60,  8, 56,
		41, 25, 37, 21, 44, 28, 40, 24,
		 3, 51, 15, 63,  2, 50, 14, 62,
		35, 19, 47, 31, 34, 18, 46, 30,
		11, 59,  7, 55, 10, 58,  6, 54,
		43, 27, 39, 23, 42, 26, 38, 22
	};
	int r = y * 8 + x;
	return dither[r] / 64;
}

/*#if defined(UNITY_REVERSED_Z)
	#if UNITY_REVERSED_Z == 1
		//D3d with reversed Z => z clip range is [near, 0] -> remapping to [0, far]
		//max is required to protect ourselves from near plane not being correct/meaningfull in case of oblique matrices.
		#define UNITY_Z_0_FAR_FROM_CLIPSPACE(coord) max(((1.0-(coord)/_ProjectionParams.y)*_ProjectionParams.z),0)
	#else
		//GL with reversed z => z clip range is [near, -far] -> should remap in theory but dont do it in practice to save some perf (range is close enough)
		#define UNITY_Z_0_FAR_FROM_CLIPSPACE(coord) max(-(coord), 0)
	#endif
#elif UNITY_UV_STARTS_AT_TOP
	//D3d without reversed z => z clip range is [0, far] -> nothing to do
	#define UNITY_Z_0_FAR_FROM_CLIPSPACE(coord) (coord)
#else
	//Opengl => z clip range is [-near, far] -> should remap in theory but dont do it in practice to save some perf (range is close enough)
	#define UNITY_Z_0_FAR_FROM_CLIPSPACE(coord) (coord)
#endif

float3 ComputeAdvFog(float3 col,float3 fogCol, float coord, float3 wPos, float3 viewPos, float3 vDir) {
	float dist = UNITY_Z_0_FAR_FROM_CLIPSPACE((coord.x)); 
	float3 cbVec = wPos.xyz - viewPos; 
	float fogHeightMask = 1.0 - (cbVec.y) * 0.003; 
	float fogMask = 1.0 - saturate(dist * unity_FogParams.z + unity_FogParams.w); 
	float r = saturate(-fogHeightMask); 
	r = 1.0 - r / (r + 1.0); 
	float fogMask2 = saturate((dist - StartPos2ndFog) / FogLength2ndFog); 
	fogMask2 = 1.0 - PositivePow(1.0 - fogMask2, FogColor2ndFog.w * 10.0 + 1e-3); 
	float uFogFactor = saturate((r * fogMask) * (saturate(fogHeightMask) + 1.0) * 0.5); 
	float secondfogFac = saturate((r * fogMask2) * (saturate(fogHeightMask) + 1.0) * 0.5); 
	float VOffset_Cam = HeightFogHeight - viewPos.y; 
	float VOffset_Pos = HeightFogHeight - wPos.y; 
	float UpPart = -(min(0, VOffset_Cam) + min(0, VOffset_Pos)); 
	float UnderPoart = (max(0, VOffset_Cam) + max(0, VOffset_Pos)); 
	float rH = UnderPoart * UnderPoart / (2.0 * (abs(VOffset_Cam) + abs(VOffset_Pos))) * dist * HeightFogDensity; 
	rH = 1.0 - saturate(rH * unity_FogParams.z + unity_FogParams.w); 
	rH = smoothstep(0,1,rH); 
	float heightFogFac = saturate(rH * HeightFogColor.a * (1.0 - uFogFactor)) * UseHeightFog * UseAdvFog; 
#ifdef UNITY_PASS_FORWARDADD
	col.rgb = lerp((col).rgb, float3(0, 0, 0), heightFogFac);
	col.rgb = lerp((col).rgb, float3(0, 0, 0), secondfogFac * Use2ndFog * UseAdvFog);
#else
	col.rgb = lerp((col).rgb, HeightFogColor.rgb, heightFogFac);
	col.rgb = lerp((col).rgb, FogColor2ndFog.rgb, secondfogFac * Use2ndFog * UseAdvFog);
#endif
	col.rgb = lerp((col).rgb, (fogCol).rgb, uFogFactor);
	return col;
}

#ifdef UNITY_PASS_FORWARDBASE
	#define ADV_FOG_COLOR(col, fogCol, coord, wPos, viewPos, vDir) col.rgb=ComputeAdvFog(col.rgb, fogCol, coord, wPos, viewPos, vDir)
		// float dist = UNITY_Z_0_FAR_FROM_CLIPSPACE((coord.x)); \
		// float3 cbVec = wPos.xyz - viewPos; \
		// float fogHeightMask = 1.0 - (cbVec.y) * 0.003; \
		// float fogMask = 1.0 - saturate(dist * unity_FogParams.z + unity_FogParams.w); \
		// float r = saturate(-fogHeightMask); \
		// r = 1.0 - r / (r + 1.0); \
		// float fogMask2 = saturate((dist - StartPos2ndFog) / FogLength2ndFog); \
		// fogMask2 = 1.0 - pow(1.0 - fogMask2, FogColor2ndFog.w * 10.0 + 1e-3); \
		// float uFogFactor = saturate((r * fogMask) * (saturate(fogHeightMask) + 1.0) * 0.5); \
		// float secondfogFac = saturate((r * fogMask2) * (saturate(fogHeightMask) + 1.0) * 0.5); \
		// float VOffset_Cam = HeightFogHeight - viewPos.y; \
		// float VOffset_Pos = HeightFogHeight - wPos.y; \
		// float UpPart = -(min(0, VOffset_Cam) + min(0, VOffset_Pos)); \
		// float UnderPoart = (max(0, VOffset_Cam) + max(0, VOffset_Pos)); \
		// float rH = UnderPoart * UnderPoart / (2.0 * (abs(VOffset_Cam) + abs(VOffset_Pos))) * dist * HeightFogDensity; \
		// rH = 1.0 - saturate(rH * unity_FogParams.z + unity_FogParams.w); \
		// rH = smoothstep(0,1,rH); \
		// float heightFogFac = saturate(rH * HeightFogColor.a * uFogFactor) * UseHeightFog * UseAdvFog; \
		// col.rgb = wPos.xyz;
		// col.rgb = lerp((col).rgb, HeightFogColor.rgb, heightFogFac); \
		// col.rgb = lerp((col).rgb, FogColor2ndFog.rgb, secondfogFac * Use2ndFog * UseAdvFog); \
		// col.rgb = lerp((col).rgb, (fogCol).rgb, uFogFactor)
#else
	#define ADV_FOG_COLOR(col, fogCol, coord, wPos, viewPos, vDir) \
		float dist = UNITY_Z_0_FAR_FROM_CLIPSPACE((coord.x)); \
		float uFogFactor = dist * unity_FogParams.z + unity_FogParams.w; \
		col.rgb = lerp((fogCol).rgb, (col).rgb, saturate(uFogFactor))
#endif

#if defined(FOG_LINEAR) || defined(FOG_EXP) || defined(FOG_EXP2)
	#ifdef UNITY_PASS_FORWARDADD
		#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir) ADV_FOG_COLOR(col,fixed4(0,0,0,0),coord,(wPos).xyz,viewPos,vDir)
	#else
		#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir) ADV_FOG_COLOR(col,unity_FogColor,coord,(wPos).xyz,viewPos,vDir)
	#endif
#else
	#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir)
#endif*/

#if defined(FOG_LINEAR) || defined(FOG_EXP) || defined(FOG_EXP2)
	#ifdef UNITY_PASS_FORWARDADD
		#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir)
	#else
		#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir) col=ApplyHeightFog(col,wPos)
	#endif
#else
	#define APPLY_ADV_FOG(coord,col,wPos,viewPos,vDir)
#endif

uniform int _ZtestMode;

#endif // REX_CG_INCLUDED