//effect
uniform fixed _propertyF0_1;
uniform fixed _propertyF0_0;
uniform fixed4 _propertyC0;
uniform fixed _propertyT0_0;
uniform fixed _propertyT1_0;
uniform fixed _propertyF1_0;
uniform fixed4 _propertyC1;
uniform fixed _propertyF1_1;
uniform fixed _propertyT2_0;
uniform fixed _propertyF2_0;
uniform fixed4 _propertyC2;
uniform fixed _propertyT3_0;
uniform fixed4 _propertyC3;
uniform fixed _propertyF3_0;

//HairCore
#ifndef HAIRCORE_INCLUDED
#define HAIRCORE_INCLUDED

#include "UnityCG.cginc"
#include "AutoLight.cginc"

//base prop
sampler2D _MainTex;
#ifdef NORMALTEX
	sampler2D _BumpMap;
	half _BumpScale;
#endif

half _Smoothness,_Fresnel, _Occ;
half4 _LightColor0;
half4 _Color;
half _Cutoff;

#ifdef HIGHLIGHTS
	half4 _highlightsColor;
#endif

#if defined(SCATTER)
	half4 _ScatterColor;
	half _ScatterDistortion, _ScatteringScale, _ScatteringSPower;
#endif



#if defined (HAIRTANGENTMAP)
	sampler2D _HairTangentMap;
#elif defined(HAIRTILINGFLOW)
	sampler2D _HairTilingFlowmap;
	half _HairTiling;
#endif

//sampler2D _HairAOTex;

half4 _SpecColor1;
half _SpecularAmount1;
half _SpecShift1;
half _SpecPower1;

#ifdef DOUBLELOBE
	half4 _SpecColor2;
	half  _SpecularAmount2;
	half _SpecShift2;
	half _SpecPower2;
#endif

struct VertexInputBase
{
	float4 vertex   : POSITION;
	float3 normal    : NORMAL;
	float2 uv0      : TEXCOORD0;
	float2 uv1      : TEXCOORD1;
	float4 tangent   : TANGENT;
	#ifdef HIGHLIGHTS
		float4 color : COLOR;
	#endif
};

struct VertexOutputBase
{
	float4 pos                            : SV_POSITION;
	float4 eyeVec                         : TEXCOORD1;    // eyeVec.xyz | gradationColor
	float4 tangentToWorldAndPackedData[3] : TEXCOORD2;    // [3x3:tangentToWorld | 1x3:viewDirForParallax or worldPos]
	#ifdef HIGHLIGHTS
		float4 vertexColor				 : TEXCOORD9;	
	#endif
	UNITY_LIGHTING_COORDS(5, 6)
	#if defined (GI)
		float4 ambientOrLightmapUV             : TEXCOORD7;    // SH or Lightmap UV
	#endif
	float4 tex                            : TEXCOORD0;
	float4 screenUV						: TEXCOORD8;

};


//https://www.shadertoy.com/view/4dKcWK
const half EPSILON = 1e-10;
half3 HUEtoRGB(in half hue)
{
	// Hue [0..1] to RGB [0..1]
	// See http://www.chilliant.com/rgb2hsv.html
	half3 rgb = abs(hue * 6. - half3(3, 2, 4)) * half3(1, -1, -1) + half3(-1, 2, 2);
	return clamp(rgb, 0., 1.);
}
half3 RGBtoHCV(in half3 rgb)
{
	// RGB [0..1] to Hue-Chroma-Value [0..1]
	// Based on work by Sam Hocevar and Emil Persson
	half4 p = (rgb.g < rgb.b) ? half4(rgb.bg, -1., 2. / 3.) : half4(rgb.gb, 0., -1. / 3.);
	half4 q = (rgb.r < p.x) ? half4(p.xyw, rgb.r) : half4(rgb.r, p.yzx);
	half c = q.x - min(q.w, q.y);
	half h = abs((q.w - q.y) / (6. * c + EPSILON) + q.z);
	return half3(h, c, q.x);
}

half3 HSVtoRGB(in half3 hsv)
{
	// Hue-Saturation-Value [0..1] to RGB [0..1]
	half3 rgb = HUEtoRGB(hsv.x);
	return ((rgb - 1.) * hsv.y + 1.) * hsv.z;
}

half3 RGBtoHSV(in half3 rgb)
{
	// RGB [0..1] to Hue-Saturation-Value [0..1]
	half3 hcv = RGBtoHCV(rgb);
	half s = hcv.y / (hcv.z + EPSILON);
	return half3(hcv.x, s, hcv.z);
}

half KajiyaSpecular(half3 T, half3 V, half3 L,half exponent)
{
	half3 H = normalize(L + V);
	half dotTH = dot(T, H);
	half sinTH = sqrt(1 - dotTH * dotTH);
	half dirAtten = smoothstep(-1.0, 0.0, dot(T, H));
	return dirAtten * pow(sinTH, exponent);
}

half3 ShiftTangent(half3 T, half3 N, half shift)
{
	half3 shiftedT = T + (shift * N); // shift = shift value + shift texture
	return normalize(shiftedT);
}

fixed4 EffectColor(half3 nv)
{
	fixed4 finalColor;
	float dotResult18 = nv;
	float temp_output_46_0 = ( ( 1.0 - max( 0.0 , dotResult18 ) ) + 0.1 );
	float4 blendOpSrc48 = ( saturate( pow( temp_output_46_0 , _propertyF0_1 ) ) * _propertyF0_0 * _propertyC1 );
	float4 blendOpDest48 = ( float4( 0,0,0,0 ) * _propertyF1_1 * _propertyC2 * saturate( pow( temp_output_46_0 , _propertyF1_1 ) ) );		
	finalColor = ( ( saturate( ( 1.0 - ( 1.0 - blendOpSrc48 ) * ( 1.0 - blendOpDest48 ) ) )) + ( (( _propertyT2_0 )?( 0.0 ):( 1.0 )) * _propertyF2_0 * _propertyC3 ) + ( float4( 0,0,0,0 ) * _propertyC3 * _propertyF3_0) );

	return finalColor;
}
// half ScreenDitherToAlpha(half x, half y, half c0)
// {
// 	const half dither[64] = {
// 		0, 32, 8, 40, 2, 34, 10, 42,
// 		48, 16, 56, 24, 50, 18, 58, 26 ,
// 		12, 44, 4, 36, 14, 46, 6, 38 ,
// 		60, 28, 52, 20, 62, 30, 54, 22,
// 		3, 35, 11, 43, 1, 33, 9, 41,
// 		51, 19, 59, 27, 49, 17, 57, 25,
// 		15, 47, 7, 39, 13, 45, 5, 37,
// 		63, 31, 55, 23, 61, 29, 53, 21 };

// 	int xMat = int(x) & 7;
// 	int yMat = int(y) & 7;

// 	half limit = (dither[yMat * 8 + xMat] + 11.0) / 64.0;
// 	return lerp(limit*c0, 1.0, c0);
// }


// half3 UnPackNormalTex(half4 normalTex)
// {
// 	normalTex.x *= normalTex.w;
// 	normalTex.xy = (normalTex.xy * 2 - 1);
// 	normalTex.z = sqrt(1.0 - saturate(dot(normalTex.xy, normalTex.xy)));
// 	return normalTex;
// }

// half GetSpecTerm(half roughness2,half nl,half nv,half nh)
// {
// 	half lambdaV = nl * (nv * (1 - roughness2) + roughness2);
// 	half lambdaL = nv * (nl * (1 - roughness2) + roughness2);
// 	half V = 0.5f / (lambdaV + lambdaL + 1e-5f);
// 	half a2 = roughness2 * roughness2;
// 	half d = (nh * a2 - nh) * nh + 1.0f;
// 	half D = 0.31830988618f * a2 / (d * d + 1e-7f);

// 	half specularTerm = V * D * 3.14159265359f;
// 	specularTerm = max(0, specularTerm * nl);	
// 	return specularTerm;
// }

VertexOutputBase vertBase (VertexInputBase v)
{
	VertexOutputBase o;
	UNITY_INITIALIZE_OUTPUT(VertexOutputBase, o);

	float4 posWorld = mul(unity_ObjectToWorld, v.vertex);
	o.tangentToWorldAndPackedData[0].w = posWorld.x;
	o.tangentToWorldAndPackedData[1].w = posWorld.y;
	o.tangentToWorldAndPackedData[2].w = posWorld.z;

	o.pos = UnityObjectToClipPos(v.vertex);
	o.tex.xy = v.uv0;
	o.eyeVec.xyz = normalize(posWorld.xyz - _WorldSpaceCameraPos);
	//o.eyeVec.w = dot((v.vertex.z*_Gradation.x + _Gradation.y),(v.vertex.y*_Gradation.z + _Gradation.w));
	float3 normalWorld = UnityObjectToWorldNormal(v.normal);
	float4 tangentWorld = float4(UnityObjectToWorldDir(v.tangent.xyz), v.tangent.w);
	float sign = tangentWorld.w * unity_WorldTransformParams.w;
	float3 binormal = cross(normalWorld, tangentWorld.xyz) * sign;
	float3x3 tangentToWorld = float3x3(tangentWorld.xyz, binormal, normalWorld);
	o.tangentToWorldAndPackedData[0].xyz = tangentToWorld[0];
	o.tangentToWorldAndPackedData[1].xyz = tangentToWorld[1];
	o.tangentToWorldAndPackedData[2].xyz = tangentToWorld[2];

	UNITY_TRANSFER_LIGHTING(o, v.uv1);
	#if defined (GI)
		o.ambientOrLightmapUV.rgb = max(float3(0,0,0), ShadeSH9 (float4(normalWorld, 1.0)));
	#endif
	o.tex.zw = v.uv1;
	o.screenUV = ComputeScreenPos(o.pos);
	
	#ifdef HIGHLIGHTS
		o.vertexColor = v.color;
	#endif
	
	return o;
}

half4 fragBase(VertexOutputBase i  , half facing : VFACE) : SV_Target
{
	// Input
	half3 eyeVec = normalize(i.eyeVec.xyz);
	half3 posWorld = half3(i.tangentToWorldAndPackedData[0].w, i.tangentToWorldAndPackedData[1].w, i.tangentToWorldAndPackedData[2].w);
	half3 tangent = i.tangentToWorldAndPackedData[0].xyz;
	half3 binormal = i.tangentToWorldAndPackedData[1].xyz;
	half3 normal = i.tangentToWorldAndPackedData[2].xyz;
	normal =  facing > 0 ? normal : -normal;
	half3 viewDir = -eyeVec.xyz;

	half2 texcoord = i.tex.xy;
	half4 albedoColor = tex2D(_MainTex, texcoord);
	
	#ifdef HIGHLIGHTS
		half4 albedo = lerp ( (albedoColor * _Color), (albedoColor * _highlightsColor), i.vertexColor.r);
	#else
		half4 albedo = albedoColor * _Color;
	#endif

	half metallic =  0;
	half smoothness = _Smoothness;   
	half occ = 1;




	half roughness = 1- smoothness;
	half3 specColor = lerp(unity_ColorSpaceDielectricSpec.rgb, albedo.rgb, metallic);
	half oneMinusReflectivity = unity_ColorSpaceDielectricSpec.a - metallic * unity_ColorSpaceDielectricSpec.a;
	half3 diffColor = albedo.rgb  * oneMinusReflectivity;


	// Input End

#if defined (HAIRTANGENTMAP)
	half4 hairTangentmap = tex2D(_HairTangentMap, texcoord);
	half HairAOTex = hairTangentmap.a;
	occ = lerp(1, HairAOTex, _Occ);

	half3 hairTangent = hairTangentmap.rgb * 2 - 1;
	#ifdef _INVERTTANGENT_ON
		hairTangent.y  = - hairTangent.y;
	#endif
	hairTangent = (tangent*hairTangent.x) + (binormal*hairTangent.y) + (normal*hairTangent.z);

	//hairTangent = hairTangent - normal*dot( hairTangent, normal );
	hairTangent = normalize(hairTangent);

#endif

	// Light
	UNITY_LIGHT_ATTENUATION(atten, i, posWorld);
	half3 mainLight = _LightColor0.rgb*saturate(atten);

	#ifndef USING_DIRECTIONAL_LIGHT
		half3 lightDir = normalize(UnityWorldSpaceLightDir(posWorld.xyz));
	#else

		half3 lightDir = _WorldSpaceLightPos0.xyz;
	#endif

	// Bump
#if defined (NORMALTEX)
	half3 normalTex = UnpackNormalWithScale(tex2D(_BumpMap, texcoord), _BumpScale);
	normal = normalize(tangent * normalTex.x + binormal * normalTex.y + normal * normalTex.z);
#endif
	// Bump End

	// Dir
	half3 halfDir = normalize(half3(lightDir.xyz) + viewDir);
	half nv = abs(dot(normal, viewDir));
	half nl = saturate(dot(normal, lightDir.xyz));
	half nh = saturate(dot(normal, halfDir));
	half lh = saturate(dot(lightDir.xyz, halfDir));
	// Dir End

	// GI
	half3 GIdiffuse = 0;
	half3 GIspecular = 1;
#if defined (GI)
	half3 reflUVW = reflect(eyeVec.xyz, normal);
	half GIperceptualRoughness = roughness * (1.7 - 0.7*roughness);
	half mip = GIperceptualRoughness * 6;
	GIspecular = UNITY_SAMPLE_TEXCUBE_LOD(unity_SpecCube0,reflUVW.xyz, mip).rgb * occ;
	GIspecular = DecodeHDR(GIspecular.rgbr, unity_SpecCube0_HDR);
	GIdiffuse = occ * i.ambientOrLightmapUV.rgb;

	half grazingTerm = saturate(smoothness + (1 - oneMinusReflectivity));
	half3 fresnelLerp = lerp (specColor, grazingTerm,  pow (1 - nv,5)*_Fresnel);
#endif
	// GI End
  

	// Diffuse term
	half3 diffuseTerm = 0;

	diffuseTerm =lerp(0.25,1,nl) ;

	half fd90 = 0.5 + 2 * lh * lh * roughness;
	half lightScatter   = (1 + (fd90 - 1) * pow(1 - nl,5));
	half viewScatter    = (1 + (fd90 - 1) * pow(1 - nv,5));
	 diffuseTerm = lightScatter * viewScatter * nl;

	// Diffuse term End
	
	// Specular
	half roughness2 = roughness * roughness;
	roughness2 = max(roughness2, 0.002);
	half surfaceReduction = 1.0 / (roughness2*roughness2 + 1.0);


	half specularTerm = 0;
	half3 fresnelTerm = 0; 

	fresnelTerm = specColor + (1-specColor) * pow ( 1-lh,5);

	specularTerm *= any(specColor) ? 1.0 : 0.0;
	
	half3 specularColor = specularTerm * fresnelTerm ;
	// Specular End
	
	// Hair

	half3 t1 = 0;
	half3 t2 = 0;

	#ifdef HAIRTANGENTMAP
		half3 worldTangent = hairTangent;
		t1 = ShiftTangent(worldTangent, normal, _SpecShift1 );
		#ifdef DOUBLELOBE
			t2 = ShiftTangent(worldTangent, normal, _SpecShift2 );
		#endif

	#elif defined (HAIRTILINGFLOW)
		half3 worldBinormal = normalize(binormal);
		half shiftTex = tex2D(_HairTilingFlowmap, half2(texcoord.x * _HairTiling, texcoord.y));
		t1 = ShiftTangent(worldBinormal, normal,  _SpecShift1 + shiftTex );
		t2 = ShiftTangent(worldBinormal, normal, _SpecShift2  + shiftTex );
	#endif

	#ifdef AUTOSPEC
		half3 specHSVcolor1 = RGBtoHSV(_Color.rgb);
		specHSVcolor1.x = saturate(specHSVcolor1 > 0.5? specHSVcolor1.x + 0.2 : specHSVcolor1.x - 0.2 );
		specHSVcolor1.y = saturate(specHSVcolor1.y - 0.4);
		specHSVcolor1.z = saturate(specHSVcolor1.z + 0.1);
		_SpecColor1.rgb = HSVtoRGB( specHSVcolor1);
		#ifdef DOUBLELOBE
			half3 specHSVcolor2 = RGBtoHSV(_Color.rgb);
			specHSVcolor2.x = saturate(specHSVcolor2 > 0.5? specHSVcolor2.x - 0.5 : specHSVcolor2.x + 0.5 );
			specHSVcolor2.y = saturate(specHSVcolor2.y - 0.2);
			specHSVcolor2.z = saturate(specHSVcolor2.z + 0.1);
			_SpecColor2.rgb = HSVtoRGB( specHSVcolor2);
		#endif
	#endif

	half3 spec1 =KajiyaSpecular(t1, viewDir, lightDir.xyz, _SpecPower1 * 128.0)*_SpecColor1 * _SpecularAmount1; 
	specularColor += saturate(spec1);
	#ifdef DOUBLELOBE
		half3 spec2 =KajiyaSpecular(t2, viewDir, lightDir.xyz, _SpecPower2 * 128.0)*_SpecColor2 * _SpecularAmount2;// to do: spec2 need spec mask
		specularColor += saturate(spec2);
	#endif


	// Hair End

	// Scatter
#if defined (SCATTER)
	half scatterAmount = 0;

		half scatterFresnel = pow(saturate(1-  abs(dot(i.tangentToWorldAndPackedData[2].xyz, viewDir))), _ScatteringSPower);
		half3 scatterDir = lightDir + normal * _ScatterDistortion;
		half scatterLight = pow(saturate(dot(viewDir, -scatterDir)), _ScatteringSPower) * (1 - nv) * (1 - nl);
		scatterAmount = scatterFresnel + _ScatteringScale * scatterLight;

	diffuseTerm += scatterAmount * _ScatterColor.rgb;
#endif
	// Scatter End
	
	// Final Color
	half3 color = diffColor * (GIdiffuse + mainLight * diffuseTerm);
	color += specularColor * mainLight;

#if defined (GI)
	color += surfaceReduction * GIspecular * fresnelLerp;
#endif

	//effect
	color += EffectColor(nv);
	
	half alpha = albedo.a;

	half2 screenPixel = (i.screenUV.xy/i.screenUV.w)* _ScreenParams.xy;
	#ifdef _DITHERMODE_DITHER64
		albedo.a = ScreenDitherToAlpha(screenPixel.x, screenPixel.y,  albedo.a);
	#endif
	alpha = albedo.a;

#if defined (CLIP)
	clip(alpha - _Cutoff);
#endif
	
	color = clamp(color,0,10);
	return half4(color.rgb, alpha);
	
}

#endif