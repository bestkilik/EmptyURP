#ifndef UNIVERSAL_PIPELINE_REXCG_INCLUDED
#define UNIVERSAL_PIPELINE_REXCG_INCLUDED

#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

// uniform float GlobalFog_ON;
// // uniform float UseSecondLayerFog;
// uniform float UseHeightFog;
// uniform float SecondFogStartDistance;
// uniform float SecondFogLength;
// uniform float HeightFogHeight;
// uniform float HorizonElevationAngle;
// uniform float4 SecondFogColor;
// uniform float4 HeightFogColor;

struct AdvBRDFData
{
	half3 diffuse;
	half3 specular;
	half metallic;
	half perceptualRoughness;
	half roughness;
	half roughness2;
	half grazingTerm;

	// We save some light invariant BRDF terms so we don't have to recompute
	// them in the light loop. Take a look at DirectBRDF function for detailed explaination.
	half normalizationTerm;     // roughness * 4.0 + 2.0
	half roughness2MinusOne;    // roughness^2 - 1.0
};

struct SSSInputData
{
	float3  positionWS;
	half3   normalWS;
	half3   viewDirectionWS;
	float4  shadowCoord;
	half    fogCoord;
	half3   vertexLighting;
	half3   bakedGI;
	float2  normalizedScreenSpaceUV;
    half4   shadowMask;
	half3   blurNormalWS;
};

struct HairInputData
{
	float3  positionWS;
	half3   normalWS;
	half3   viewDirectionWS;
	float4  shadowCoord;
	half    fogCoord;
	half3   vertexLighting;
	half3   bakedGI;
	float2  normalizedScreenSpaceUV;
    half4   shadowMask;
	half3   tangentWS;
};

struct EyeInputData
{
	float3  positionWS;
	half3   normalWS;
	half3   viewDirectionWS;
	float4  shadowCoord;
	half    fogCoord;
	half3   vertexLighting;
	half3   bakedGI;
	float2  normalizedScreenSpaceUV;
    half4   shadowMask;
	half3   irisNormalWS;
};

struct LeavesInputData
{
	float3  positionWS;
	half3   normalWS;
	half3   viewDirectionWS;
	float4  shadowCoord;
	half    fogCoord;
	half3   vertexLighting;
	half3   bakedGI;
	float2  normalizedScreenSpaceUV;
    half4   shadowMask;
	half3   ComputeNormalWS;
};

float ComputeHalfSpace(half Height, half fDensity, float3 positionWS, float3 cameraPosWS, float3 viewDirectionWS)
{
	float3 aV = fDensity * viewDirectionWS;
	float VOffset_Cam = cameraPosWS.y - Height;
	float VOffset_Pos = positionWS.y - Height;
	float FdotV = viewDirectionWS.y;
	float HeightMask = VOffset_Cam <= 0.0 ? 1.0 : 0.0;
	float c1 = HeightMask * (VOffset_Pos + VOffset_Cam);
	float c2 = (1 - 2*HeightMask) * VOffset_Pos;
	float g = min(c2, 0.0);
	g = -length(aV) * (c1 - g * g / abs(FdotV + 1.0e-5f));
	return g;
}

// half4 ComputeAdvFogFactors(real coord, float3 positionWS, float3 cameraPosWS, float3 viewDirectionWS){
// 	float dist = UNITY_Z_0_FAR_FROM_CLIPSPACE((coord));
// 	float3 cbVec = positionWS - cameraPosWS;
// 	float fogHeightMask = 1.0 - cbVec.y * 0.003;
// 	float fogMask = 1.0 - saturate(dist * unity_FogParams.z + unity_FogParams.w);
// 	float r = max(0,-fogHeightMask);//saturate(-fogHeightMask);
// 	r = 1.0 - r / (r + 1.0);
// 	float fogMask2 = saturate((dist - SecondFogStartDistance) / SecondFogLength);
// 	fogMask2 = 1.0 - pow(1.0 - fogMask2, SecondFogColor.w * 10.0 + 1e-3);
// 	float FacScale = (saturate(fogHeightMask) + 1.0) * 0.5 * r;
// 	float uFogFactor = FacScale * fogMask;//(r * fogMask) * (saturate(fogHeightMask) + 1.0) * 0.5;
// 	float secondfogFac = FacScale * fogMask2 * UseSecondLayerFog * GlobalFog_ON;//(r * fogMask2) * (saturate(fogHeightMask) + 1.0) * 0.5;
	
// 	float VOffset_Cam = HeightFogHeight - cameraPosWS.y;///Height;
// 	float VOffset_Pos = HeightFogHeight - positionWS.y;//Height;
// 	float UpPart = -(min(0, VOffset_Cam) + min(0, VOffset_Pos));
// 	float UnderPoart = (/* max(0, VOffset_Cam) +  */max(0, VOffset_Pos));
// 	float rH =  UpPart > 0.0 ? (UnderPoart / UpPart) : 1.0;
// 	float d = 1 - saturate(dist / 100);
// 	d = 1.0 - 0.5 * d * d;
// 	float heightFogFac = UnderPoart * 0.5 * HeightFogColor.a * d;
// 	heightFogFac *= (1.0 - saturate(uFogFactor)) * (1.0 - saturate(secondfogFac)) * UseHeightFog;

// 	half4 outVal = half4(uFogFactor, secondfogFac, heightFogFac, 0.0);
// 	return saturate(outVal);
// }

// half3 AdvFog(real3 fragColor, real coord, float3 positionWS, float3 cameraPosWS, float3 viewDirectionWS){
// half3 AdvFog(half3 fragColor, half4 coord){
// 	#if defined(FOG_LINEAR) || defined(FOG_EXP) || defined(FOG_EXP2)
// 		// float dist = UNITY_Z_0_FAR_FROM_CLIPSPACE((coord));
// 		// float3 cbVec = positionWS - cameraPosWS;
// 		// float fogHeightMask = 1.0 - (cbVec.y) * 0.003;
// 		// float fogMask = 1.0 - saturate(dist * unity_FogParams.z + unity_FogParams.w);
// 		// float r = max(0,-fogHeightMask);//saturate(-fogHeightMask);
// 		// r = 1.0 - r / (r + 1.0);
// 		// float fogMask2 = saturate((dist - SecondFogStartDistance) / SecondFogLength);
// 		// fogMask2 = 1.0 - pow(1.0 - fogMask2, SecondFogColor.w * 10.0 + 1e-3);
// 		// float FacScale = (saturate(fogHeightMask) + 1.0) * 0.5 * r;
// 		// float uFogFactor = FacScale * fogMask;//(r * fogMask) * (saturate(fogHeightMask) + 1.0) * 0.5;
// 		// float secondfogFac = FacScale * fogMask2 * UseSecondLayerFog * GlobalFog_ON;//(r * fogMask2) * (saturate(fogHeightMask) + 1.0) * 0.5;
		
// 		// float VOffset_Cam = HeightFogHeight - cameraPosWS.y;///Height;
// 		// float VOffset_Pos = HeightFogHeight - positionWS.y;//Height;
// 		// float UpPart = -(min(0, VOffset_Cam) + min(0, VOffset_Pos));
// 		// float UnderPoart = (/* max(0, VOffset_Cam) +  */max(0, VOffset_Pos));
// 		// float rH =  UpPart > 0.0 ? (UnderPoart / UpPart) : 1.0;
// 		// float d = 1 - saturate(dist / 100);
// 		// d = 1.0 - 0.5 * d * d;
// 		// float heightFogFac = UnderPoart * 0.5 * HeightFogColor.a * d;
// 		// heightFogFac *= (1.0 - saturate(uFogFactor)) * (1.0 - saturate(secondfogFac)) * UseHeightFog;
		
// 		// fragColor = lerp(fragColor, HeightFogColor.rgb, saturate(heightFogFac));
// 		// fragColor = lerp(fragColor, SecondFogColor.rgb, saturate(secondfogFac));
// 		// fragColor = lerp(fragColor, unity_FogColor.rgb, saturate(uFogFactor));
// 		fragColor = lerp(fragColor, HeightFogColor.rgb, coord.z);
// 		fragColor = lerp(fragColor, SecondFogColor.rgb, coord.y);
// 		fragColor = lerp(fragColor, unity_FogColor.rgb, coord.x);

// 		return fragColor;
// 	#endif
// 	return fragColor;
// }

half D_GGX(half roughness, half HdN, half LdH2)
{
	half a = roughness * roughness;
	half a2 = a * a;
	half normalizationTerm = a * 4.0h + 2.0h;
	float d = HdN * HdN * (a2 - 1.0f) + 1.00001f;
	return a2 / ((d * d) * max(0.1h, LdH2) * normalizationTerm);
}

half U_D_GGX(AdvBRDFData brdfData, half HdN, half LdH2)
{
	half a = brdfData.roughness;
	half a2 = brdfData.roughness2;
	float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;
	return a2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);
}

// Adv BRDF
inline void AdvInitializeBRDFData(half3 albedo, half metallic, half specular, half smoothness, half alpha, out AdvBRDFData outBRDFData)
{
	half oneMinusReflectivity = OneMinusReflectivityMetallic(metallic);
	half reflectivity = 1.0 - oneMinusReflectivity;
	// half baseVal = dot(half3(0.3, 0.59, 0.11), albedo);
	// half wBase = pow(baseVal, 0.454545) / baseVal;
	half speculerLevel = specular * 2.0;

	outBRDFData.diffuse = albedo * oneMinusReflectivity;
	outBRDFData.specular = lerp(kDieletricSpec.rgb, albedo, metallic) * speculerLevel;
	outBRDFData.metallic = metallic;
	outBRDFData.grazingTerm = saturate(smoothness + reflectivity) * speculerLevel;
	outBRDFData.perceptualRoughness = PerceptualSmoothnessToPerceptualRoughness(smoothness);
	outBRDFData.roughness = max(PerceptualRoughnessToRoughness(outBRDFData.perceptualRoughness), HALF_MIN);
	outBRDFData.roughness2 = outBRDFData.roughness * outBRDFData.roughness;

	outBRDFData.normalizationTerm = outBRDFData.roughness * 4.0h + 2.0h;
	outBRDFData.roughness2MinusOne = outBRDFData.roughness2 - 1.0h;

	#ifdef _ALPHAPREMULTIPLY_ON
		outBRDFData.diffuse *= alpha;
		alpha = alpha * oneMinusReflectivity + reflectivity;
	#endif
}

half3 AdvEnvironmentBRDF(AdvBRDFData brdfData, half3 indirectDiffuse, half3 indirectSpecular, half fresnelTerm)
{
	half3 c = indirectDiffuse * brdfData.diffuse;
	float surfaceReduction = 1.0 / (brdfData.roughness2 + 1.0);
	c += surfaceReduction * indirectSpecular * lerp(brdfData.specular, brdfData.grazingTerm, fresnelTerm);
	return c;
}


half3 AdvGlobalIllumination(AdvBRDFData brdfData, half3 bakedGI, half occlusion, half3 positionWS, half3 normalWS, half3 viewDirectionWS)
{
	half3 reflectVector = reflect(-viewDirectionWS, normalWS);
	half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

	half3 indirectDiffuse = bakedGI * occlusion;
	half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, positionWS, brdfData.perceptualRoughness, occlusion);

	return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

half3 AdvGlobalIllumination(AdvBRDFData brdfData, half3 bakedGI, half occlusion, half3 normalWS, half3 viewDirectionWS)
{
	half3 reflectVector = reflect(-viewDirectionWS, normalWS);
	half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

	half3 indirectDiffuse = bakedGI * occlusion;
	half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, brdfData.perceptualRoughness, occlusion);

	return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

half3 GrassGlobalIllumination(AdvBRDFData brdfData, half3 bakedGI, half occlusion, half3 positionWS, half3 normalWS, half3 viewDirectionWS)
{
	half3 reflectVector = reflect(-viewDirectionWS, normalWS);
	half fresnelTerm = 1.0;//Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

	half3 indirectDiffuse = bakedGI;
	half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, positionWS, brdfData.perceptualRoughness, occlusion);

	return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

half3 HairGlobalIllumination(AdvBRDFData brdfData, half3 bakedGI, half hairMask, half occlusion, half3 positionWS, half3 normalWS, half3 tangentWS, half3 viewDirectionWS)
{
	half3 fixNrm = normalize(lerp(normalWS, tangentWS, hairMask));
	half3 reflectVector = reflect(-viewDirectionWS, fixNrm);
	reflectVector = normalize(lerp(reflectVector, -reflectVector, hairMask));
	half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

	half3 indirectDiffuse = bakedGI * occlusion;
	half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, positionWS, brdfData.perceptualRoughness, lerp(occlusion, 0.1*occlusion, hairMask));

	return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

half3 AdvDirectBDRF(AdvBRDFData brdfData, half3 normalWS, half3 lightDirectionWS, half3 viewDirectionWS)
{
	#ifndef _SPECULARHIGHLIGHTS_OFF
		float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));

		float NoH = saturate(dot(normalWS, halfDir));
		half LoH = saturate(dot(lightDirectionWS, halfDir));

		// GGX Distribution multiplied by combined approximation of Visibility and Fresnel
		// BRDFspec = (D * V * F) / 4.0
		// D = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2
		// V * F = 1.0 / ( LoH^2 * (roughness + 0.5) )
		// See "Optimizing PBR for Mobile" from Siggraph 2015 moving mobile graphics course
		// https://community.arm.com/events/1155

		// Final BRDFspec = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2 * (LoH^2 * (roughness + 0.5) * 4.0)
		// We further optimize a few light invariant terms
		// brdfData.normalizationTerm = (roughness + 0.5) * 4.0 rewritten as roughness * 4.0 + 2.0 to a fit a MAD.
		float d = NoH * NoH * brdfData.roughness2MinusOne + 1.00001f;

		half LoH2 = LoH * LoH;
		half specularTerm = brdfData.roughness2 / ((d * d) * max(0.1h, LoH2) * brdfData.normalizationTerm);

		// On platforms where half actually means something, the denominator has a risk of overflow
		// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
		// sees that specularTerm have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	specularTerm = specularTerm - HALF_MIN;
		// 	specularTerm = clamp(specularTerm, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		specularTerm = clamp(specularTerm, 0.0, 25.0);

		half3 color = specularTerm * brdfData.specular + brdfData.diffuse;
		return color;
	#else
		return brdfData.diffuse;
	#endif
}

half3 AdvLightingPhysicallyBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half3 normalWS, half3 viewDirectionWS)
{
	half NdotL = saturate(dot(normalWS, lightDirectionWS));
	half3 radiance = lightColor * (lightAttenuation * NdotL);
	return AdvDirectBDRF(brdfData, normalWS, lightDirectionWS, viewDirectionWS) * radiance;
}

half3 AdvLightingPhysicallyBased(AdvBRDFData brdfData, Light light, half3 normalWS, half3 viewDirectionWS)
{
	return AdvLightingPhysicallyBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, normalWS, viewDirectionWS);
}

//// RefractGlass
half3 RefractGlassBDRF(AdvBRDFData brdfData, half3 normalWS, half3 lightDirectionWS, half3 viewDirectionWS, half alpha, half4 innerColor)
{
	#ifndef _SPECULARHIGHLIGHTS_OFF
		float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));
		half LdN = saturate(dot(normalWS, lightDirectionWS));
		float NoH = saturate(dot(normalWS, halfDir));
		half LoH = saturate(dot(lightDirectionWS, halfDir));

		// GGX Distribution multiplied by combined approximation of Visibility and Fresnel
		// BRDFspec = (D * V * F) / 4.0
		// D = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2
		// V * F = 1.0 / ( LoH^2 * (roughness + 0.5) )
		// See "Optimizing PBR for Mobile" from Siggraph 2015 moving mobile graphics course
		// https://community.arm.com/events/1155

		// Final BRDFspec = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2 * (LoH^2 * (roughness + 0.5) * 4.0)
		// We further optimize a few light invariant terms
		// brdfData.normalizationTerm = (roughness + 0.5) * 4.0 rewritten as roughness * 4.0 + 2.0 to a fit a MAD.
		float d = NoH * NoH * brdfData.roughness2MinusOne + 1.00001f;

		half LoH2 = LoH * LoH;
		half specularTerm = brdfData.roughness2 / ((d * d) * max(0.1h, LoH2) * brdfData.normalizationTerm);

		// On platforms where half actually means something, the denominator has a risk of overflow
		// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
		// sees that specularTerm have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	specularTerm = specularTerm - HALF_MIN;
		// 	specularTerm = clamp(specularTerm, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		specularTerm = clamp(specularTerm, 0.0, 25.0);

		half3 color = specularTerm * brdfData.specular + brdfData.diffuse;
		
		return (color * alpha + (innerColor.rgb * innerColor.a)) * LdN;
	#else
		return brdfData.diffuse * alpha;
	#endif
}

half3 LightingRefractGlassBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half3 normalWS, half3 viewDirectionWS, half alpha, half4 innerColor)
{
	// half NdotL = saturate(dot(normalWS, lightDirectionWS));
	// half3 radiance = lightColor * (lightAttenuation * NdotL);
	return RefractGlassBDRF(brdfData, normalWS, lightDirectionWS, viewDirectionWS, alpha, innerColor) * lightColor * lightAttenuation;
}

half3 LightingRefractGlassBased(AdvBRDFData brdfData, Light light, half3 normalWS, half3 viewDirectionWS, half alpha, half4 innerColor)
{
	return LightingRefractGlassBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, normalWS, viewDirectionWS, alpha, innerColor);
}

half3 GrassLighting(AdvBRDFData brdfData, Light light, half occlusion, half3 normalWS, half3 viewDirectionWS){
	half3 L = light.direction;
	half3 V = viewDirectionWS;
	half3 H = normalize(L + V);
	half3 N = normalWS;
	half3 upN = half3(0.0,1.0,0.0);
	half atten = light.distanceAttenuation * light.shadowAttenuation;

	half LdN = saturate(dot(L, N));
	half fLdN = saturate(dot(L, upN));
	float HdN = saturate(dot(H, N));
	half LdH = saturate(dot(L, H));

	float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;
	half LdH2 = LdH * LdH;
	half specularTerm = brdfData.roughness2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);

	// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
	// 	specularTerm = specularTerm - HALF_MIN;
	// 	specularTerm = clamp(specularTerm, 0.0, 100.0); // Prevent FP16 overflow on mobiles
	// #endif
	specularTerm = clamp(specularTerm, 0.0, 25.0);

	// half3 color = specularTerm * brdfData.specular + brdfData.diffuse;
	// return color;
	half3 color = fLdN * brdfData.diffuse;
	color += specularTerm * brdfData.specular * LdN * occlusion;
	color *= light.color * atten;

	return color;
}
//// Hair Lit
half3 SimpleHairBDRF(AdvBRDFData brdfData, half3 normalWS, half3 tangentWS, half3 lightDirectionWS, half lightAttenuation, half hairMask, half occlusion, half3 viewDirectionWS)
{
	half LdN = dot(lightDirectionWS, normalWS);
	half sLdN = saturate(LdN);

	#ifndef _SPECULARHIGHLIGHTS_OFF
		// brdfData.specular = lerp(brdfData.specular, dot(brdfData.specular, half3(0.3,0.59,0.11)),hairMask);
		half diffOcc = occlusion * 0.5 + 0.5;

		half KajiyaDiffuse = saturate(1 - abs( dot(tangentWS,lightDirectionWS) ));
		float3 FakeNormal = normalize( viewDirectionWS - tangentWS * dot(viewDirectionWS,tangentWS) );
		half NoL = saturate( ( dot(FakeNormal, lightDirectionWS) + 1 ) / 4.0 );
		half DiffuseScatter = lerp( NoL, KajiyaDiffuse, 0.33 );
		half scatter = 1.0 - brdfData.metallic;
		half t2Scl = 2.0 - 1.5 * scatter;
		
		float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));
		
		float HdN = /* saturate */(dot(normalWS, halfDir));
		half LdH = saturate(dot(lightDirectionWS, halfDir));
		
		float aHdN = dot(halfDir, tangentWS);
		aHdN *= aHdN;
		// float aHdN = HdN * HdN;
		aHdN = sqrt(1.0 - aHdN);

		HdN = lerp(saturate(HdN), aHdN, hairMask);

		half LdH2 = LdH * LdH;
		half speLit = U_D_GGX(brdfData, HdN, LdH2) * lerp(1.0, 0.1, hairMask);
		// speLit = saturate(speLit);
		speLit *= sLdN;
		half T2   = D_GGX(saturate(max(0.463*brdfData.perceptualRoughness + 0.432, brdfData.perceptualRoughness * 1.2)), HdN, LdH2);
		
		half rScl = 1.0 - brdfData.perceptualRoughness;
		rScl = 1.0 - rScl * rScl;
		// DiffuseScatter += T2 * rScl * t2Scl * diffOcc;
		// DiffuseScatter *=/*  0.3183099 * */scatter;
		DiffuseScatter = (DiffuseScatter * scatter + (T2 * t2Scl * rScl * (scatter*0.5 + 0.5) * 0.2));

		// On platforms where half actually means something, the denominator has a risk of overflow
		// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
		// sees that speLit have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	speLit = speLit - HALF_MIN;
		// 	speLit = clamp(speLit, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		speLit = clamp(speLit, 0.0, 25.0);

		half3 difLit = lerp(sLdN, DiffuseScatter, hairMask) * brdfData.diffuse;
		// sLdN = lerp(sLdN, NoL * 2.0, hairMask);

		// half3 color = brdfData.diffuse * difLit * diffOcc;
		// color += speLit * brdfData.specular * occlusion/*  * NoL * 2 */;
		// color *= sLdN;
		half3 color = difLit + speLit * brdfData.specular;
		color *= occlusion;
		//color = speLit;
		//color = speLit;
		return color;
	#else
		return brdfData.diffuse * sLdN;
	#endif
}

half3 LightingSimpleHairBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half hairMask, half occlusion, half3 normalWS, half3 tangentWS, half3 viewDirectionWS)
{
	return SimpleHairBDRF(brdfData, normalWS, tangentWS, lightDirectionWS, lightAttenuation, hairMask, occlusion, viewDirectionWS) * lightColor * lightAttenuation;
}

half3 LightingSimpleHairBased(AdvBRDFData brdfData, Light light, half hairMask, half occlusion, half3 normalWS, half3 tangentWS, half3 viewDirectionWS)
{
	return LightingSimpleHairBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, hairMask, occlusion, normalWS, tangentWS, viewDirectionWS);
}

//// EyeBall Lit
half3 EyeBallBDRF(AdvBRDFData brdfData, half occlusion, half3 normalWS, half3 IrisNormalWS, half3 lightDirectionWS, half3 viewDirectionWS, half3 fwDirectionWS)
{
	half LdN = dot(lightDirectionWS, IrisNormalWS);
	half sLdN = saturate(LdN);

	#ifndef _SPECULARHIGHLIGHTS_OFF
		half diffOcc = occlusion * 0.5 + 0.5;

		float3 fakeLightDir = normalize(float3(1.5,0.3,1.5)*float3(lightDirectionWS) + float3(fwDirectionWS));
		half fLdN = saturate(dot(fakeLightDir, IrisNormalWS));
		float3 halfDir = SafeNormalize(fakeLightDir + float3(viewDirectionWS));

		half HdN = saturate(dot(normalWS, halfDir));
		half HdN_Iris = dot(halfDir, IrisNormalWS);
		half LdH = saturate(dot(lightDirectionWS, halfDir));

		// GGX Distribution multiplied by combined approximation of Visibility and Fresnel
		// BRDFspec = (D * V * F) / 4.0
		// D = roughness^2 / ( HdN^2 * (roughness^2 - 1) + 1 )^2
		// V * F = 1.0 / ( LdH^2 * (roughness + 0.5) )
		// See "Optimizing PBR for Mobile" from Siggraph 2015 moving mobile graphics course
		// https://community.arm.com/events/1155

		// Final BRDFspec = roughness^2 / ( HdN^2 * (roughness^2 - 1) + 1 )^2 * (LdH^2 * (roughness + 0.5) * 4.0)
		// We further optimize a few light invariant terms
		// brdfData.normalizationTerm = (roughness + 0.5) * 4.0 rewritten as roughness * 4.0 + 2.0 to a fit a MAD.
		//float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;

		half LdH2 = LdH * LdH;
		//half specularTerm = brdfData.roughness2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);
		half speLit = U_D_GGX(brdfData, HdN, LdH2);
		half speLit2 = clamp(D_GGX(0.4, HdN_Iris, LdH2), 0.0, 10.0);

		// On platforms where half actually means something, the denominator has a risk of overflow
		// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
		// sees that speLit have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	speLit = speLit - HALF_MIN;
		// 	speLit = clamp(speLit, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		speLit = clamp(speLit, 0.0, 25.0);

		half3 color = (speLit + speLit2 * brdfData.diffuse) * brdfData.specular * fLdN * occlusion + brdfData.diffuse * sLdN * diffOcc;
		return color;
	#else
		return brdfData.diffuse * sLdN;
	#endif
}

half3 LightingEyeBallBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half occlusion, half3 normalWS, half3 IrisNormalWS, half3 viewDirectionWS, half3 fwDirectionWS)
{
	// half NdotL = saturate(dot(IrisNormalWS, lightDirectionWS));
	// half3 radiance = lightColor * (lightAttenuation * NdotL);
	return EyeBallBDRF(brdfData, occlusion, normalWS, IrisNormalWS, lightDirectionWS, viewDirectionWS, viewDirectionWS) * lightColor * lightAttenuation;
}

half3 LightingEyeBallBased(AdvBRDFData brdfData, Light light, half occlusion, half3 normalWS, half3 IrisNormalWS, half3 viewDirectionWS, half3 fwDirectionWS)
{
	return LightingEyeBallBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, occlusion, normalWS, IrisNormalWS, viewDirectionWS, fwDirectionWS);
}

//// SSS Lit
half3 GlobalIllumination3S(AdvBRDFData brdfData, half3 bakedGI, half occlusion, half3 positionWS, half3 normalWS, half3 viewDirectionWS, half4 sssInfo, half4 indirInfo)
{
	half3 reflectVector = reflect(-viewDirectionWS, normalWS);
	half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));
	half3 indirSSS = (sssInfo.rgb + indirInfo.rgb) * 0.5;
	half3 indirectDiffuse = saturate(dot(half3(0.3,0.59,0.11), bakedGI)*16.0)*saturate(0.068 - 0.136*bakedGI)*(indirSSS * sssInfo.a) + bakedGI;
	indirectDiffuse *= indirInfo.a;
	indirectDiffuse *= occlusion;
	half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, positionWS, brdfData.perceptualRoughness, occlusion);

	return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
}

half3 Simple3SBDRF(AdvBRDFData brdfData, half3 normalWS, half3 blurNormalWS, half3 lightDirectionWS, half lightAttenuation, half occlusion, half litOcclusion, half3 viewDirectionWS, half4 sssInfo)
{
	half LdN = dot(lightDirectionWS, normalWS);
	half sLdN = saturate(LdN);

	#ifndef _SPECULARHIGHLIGHTS_OFF
		float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));
		
		float HdN = saturate(dot(normalWS, halfDir));
		half LdH = saturate(dot(lightDirectionWS, halfDir));
		
		// half diffOcc = occlusion * 0.5 + 0.5;
		// half dAtten = lightAttenuation * diffOcc;
		// half sl = sssInfo.a * 0.1;
		// half sAtten = dAtten * (1.0 - sl) + sl;
		// sAtten = pow(saturate(sAtten), 0.2);
		// half sOff = 0.15 + 0.625*sssInfo.a;
		// half sm = (LdN + 1.0) * sAtten - 1.0;
		// half sV = smoothstep(-sOff, 1.0 + sOff, sm);
		// half sBase = sLdN * dAtten;

		half sLevel = 1.0 - sssInfo.a;
		sLevel = 1.0 - sLevel*sLevel*sLevel;
		// half3 difLit = saturate(sV - sBase)*sssInfo.rgb*sLevel + sLdN * dAtten;
		float3 sssLevel = sssInfo.rgb * sLevel;
		half3 sOff = 0.15 + 0.625*sssLevel;
		float3 wNLowR = lerp(normalWS, blurNormalWS, sssLevel.r);
		float3 wNLowG = lerp(normalWS, blurNormalWS, sssLevel.g);
		float3 wNLowB = lerp(normalWS, blurNormalWS, sssLevel.b);
		float3 sssLdn = float3(dot(wNLowR, lightDirectionWS), dot(wNLowG, lightDirectionWS), dot(wNLowB, lightDirectionWS));
		half3 difLit = half3(
			smoothstep(-sOff.r, 1.0 + sOff.r, sssLdn.r),
			smoothstep(-sOff.g, 1.0 + sOff.g, sssLdn.g),
			smoothstep(-sOff.b, 1.0 + sOff.b, sssLdn.b)
		) * lightAttenuation;
		
		//half3 difLit = sLdN * dAtten;

		half LdH2 = LdH * LdH;
		half speLit = U_D_GGX(brdfData, HdN, LdH2);
		//half speLit2 = D_GGX(min(0.278, 0.5*brdfData.perceptualRoughness), HdN, LdH2) * 0.01;
		

		// On platforms where half actually means something, the denominator has a risk of overflow
		// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
		// sees that speLit have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	speLit = speLit - HALF_MIN;
		// 	speLit = clamp(speLit, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		speLit = clamp(speLit, 0.0, 25.0);
		
		half3 color = difLit * brdfData.diffuse; 
		color += (speLit/* + speLit2*/) * brdfData.specular * sLdN * lightAttenuation * litOcclusion;
		return color;
	#else
		return brdfData.diffuse * sLdN;
	#endif
}

half3 LightingSimple3SBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half occlusion, half litOcclusion, half3 normalWS, half3 blurNormalWS, half3 viewDirectionWS, half4 sssInfo)
{
	return Simple3SBDRF(brdfData, normalWS, blurNormalWS, lightDirectionWS, lightAttenuation, occlusion, litOcclusion, viewDirectionWS, sssInfo) * lightColor;
}

half3 LightingSimple3SBased(AdvBRDFData brdfData, Light light, half occlusion, half litOcclusion, half3 normalWS, half3 blurNormalWS, half3 viewDirectionWS, half4 sssInfo)
{
	return LightingSimple3SBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, occlusion, litOcclusion, normalWS, blurNormalWS, viewDirectionWS, sssInfo);
}

//// Leaves Lit
half3 LeavesBDRF(AdvBRDFData brdfData, half3 normalWS, half3 ComputeNormalWS, half3 lightDirectionWS, half lightAttenuation, half occlusion, half3 viewDirectionWS, half4 sssInfo)
{
	half LdN = dot(lightDirectionWS, ComputeNormalWS);
	half sLdN = saturate(LdN);
	half atten = lightAttenuation;//(lightAttenuation * 0.5 + 0.5);
	
	#ifndef _SPECULARHIGHLIGHTS_OFF
		half diffOcc = occlusion * 0.5 + 0.5;
		float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));
		
		float HdN = saturate(dot(normalWS, halfDir));
		half LdH = saturate(dot(lightDirectionWS, halfDir));
		
		half halfTransDepth = 0.5 * sssInfo.a;
		half difLit = saturate(LdN * (1 - halfTransDepth) + halfTransDepth);
		half3 backLit = saturate(pow(1 - LdH, 16) * sssInfo.rgb * sssInfo.a * 2 * difLit) * (brdfData.diffuse + 1.0) * 0.5;
		difLit *= difLit;
		
		half LdH2 = LdH * LdH;
		half speLit = U_D_GGX(brdfData, HdN, LdH2);

		// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
		// 	speLit = speLit - HALF_MIN;
		// 	speLit = clamp(speLit, 0.0, 100.0); // Prevent FP16 overflow on mobiles
		// #endif
		speLit = clamp(speLit, 0.0, 25.0);

		half3 color = brdfData.diffuse * difLit * diffOcc;
		color += speLit * brdfData.specular * sLdN * occlusion;
		color *= atten;
		color += backLit * (atten * 0.5 + 0.5);
		return color;
	#else
		return brdfData.diffuse * sLdN * atten;
	#endif
}

half3 LightingSimpleLeavesBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half occlusion, half3 normalWS, half3 ComputeNormalWS, half3 viewDirectionWS, half4 sssInfo)
{
	return LeavesBDRF(brdfData, normalWS, ComputeNormalWS, lightDirectionWS, lightAttenuation, occlusion, viewDirectionWS, sssInfo) * lightColor;
}

half3 LightingSimpleLeavesBased(AdvBRDFData brdfData, Light light, half occlusion, half3 normalWS, half3 ComputeNormalWS, half3 viewDirectionWS, half4 sssInfo)
{
	return LightingSimpleLeavesBased(brdfData, light.color, light.direction, light.distanceAttenuation * (light.shadowAttenuation * 0.5 + 0.5), occlusion, normalWS, ComputeNormalWS, viewDirectionWS, sssInfo);
}

/////////////
half4 UniversalAdvPBR(InputData inputData, half3 albedo, half metallic, half specular, half smoothness, half occlusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 color = AdvGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
	color += AdvLightingPhysicallyBased(brdfData, mainLight, inputData.normalWS, inputData.viewDirectionWS);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += AdvLightingPhysicallyBased(brdfData, light, inputData.normalWS, inputData.viewDirectionWS);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	return half4(color, alpha);
}

half4 UniversalSSSLit(SSSInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half4 sssInfo, half4 indirInfo, half occlusion, half litOcclusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 color = GlobalIllumination3S(brdfData, inputData.bakedGI, occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS, sssInfo, indirInfo);
	color += LightingSimple3SBased(brdfData, mainLight, occlusion, litOcclusion, inputData.normalWS, inputData.blurNormalWS, inputData.viewDirectionWS, sssInfo);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += LightingSimple3SBased(brdfData, light, occlusion, litOcclusion, inputData.normalWS, inputData.blurNormalWS, inputData.viewDirectionWS, sssInfo);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	return half4(color, alpha);
}

half4 UniversalLeavesLit(LeavesInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half4 sssInfo, half occlusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif

	half3 oriNrmWS = inputData.normalWS;
	#ifdef _COMPUTENORMAL_ON
		float3 wObjPos = float3(UNITY_MATRIX_M[0][3], UNITY_MATRIX_M[1][3], UNITY_MATRIX_M[2][3]);
		float3 fakeNrm = normalize(inputData.positionWS - wObjPos);
		fakeNrm = normalize((float3(0, 1, 0) + mainLight.direction)*0.5 * sssInfo.a + fakeNrm);
	#else
		float3 fakeNrm = normalize((float3(0, 1, 0) + mainLight.direction)*0.5 * sssInfo.a + inputData.normalWS);
	#endif
	inputData.ComputeNormalWS = fakeNrm;

	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 color = AdvGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.normalWS, inputData.viewDirectionWS);
	color += LightingSimpleLeavesBased(brdfData, mainLight, occlusion, inputData.normalWS, inputData.ComputeNormalWS, inputData.viewDirectionWS, sssInfo);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			#ifdef _COMPUTENORMAL_ON
				fakeNrm = normalize((float3(0, 1, 0) + light.direction)*0.5 * sssInfo.a + fakeNrm);
			#else
				fakeNrm = normalize((float3(0, 1, 0) + light.direction)*0.5 * sssInfo.a + oriNrmWS);
			#endif
			inputData.normalWS = fakeNrm;
			color += LightingSimpleLeavesBased(brdfData, light, occlusion, inputData.normalWS, inputData.ComputeNormalWS, inputData.viewDirectionWS, sssInfo);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	return half4(color, alpha);
}

half4 UniversalGrassLit(InputData inputData, half3 albedo, half smoothness, half occlusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	half spec = dot(albedo, half3(0.2126729, 0.7151522, 0.0721750));
	AdvInitializeBRDFData(albedo, 0, spec, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	mainLight.shadowAttenuation = 0.786 * mainLight.shadowAttenuation + 0.214;
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 color = GrassGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
	color += GrassLighting(brdfData, mainLight, occlusion, inputData.normalWS, inputData.viewDirectionWS);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += GrassLighting(brdfData, light, occlusion, inputData.normalWS, inputData.viewDirectionWS);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	return half4(color, alpha);
}

half4 UniversalEyeLit(EyeInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half occlusion, half3 emission, half alpha, half3 fwDir)
{
	AdvBRDFData brdfData;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 fwDirectionWS = normalize(mul((float3x3)UNITY_MATRIX_M, fwDir));

	half3 color = AdvGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.normalWS, inputData.viewDirectionWS);
	color += LightingEyeBallBased(brdfData, mainLight, occlusion, inputData.normalWS, inputData.irisNormalWS, inputData.viewDirectionWS, fwDirectionWS);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += LightingEyeBallBased(brdfData, light, occlusion, inputData.normalWS, inputData.irisNormalWS, inputData.viewDirectionWS, fwDirectionWS);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	return half4(color, alpha);
}

half4 UniversalHairLit(HairInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half hairMask, half occlusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	smoothness = smoothness * 0.825 + 0.15;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));
	
	half3 color = HairGlobalIllumination(brdfData, inputData.bakedGI, hairMask, occlusion, inputData.positionWS, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS);
	color += LightingSimpleHairBased(brdfData, mainLight, hairMask, occlusion, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += LightingSimpleHairBased(brdfData, light, hairMask, occlusion, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS);
		}
	#endif

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	// color = brdfData.diffuse;
	return half4(color, alpha);
}

half4 UniversalRefractGlass(InputData inputData, half3 albedo, half metallic, half specular, half smoothness, half4 RefractionColor, half RefractionGlossy, half IOR, half4 InnerColor, half occlusion, half3 emission, half alpha)
{
	AdvBRDFData brdfData;
	AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

	#if defined(SHADOWS_SHADOWMASK) && defined(LIGHTMAP_ON)
		half4 shadowMask = inputData.shadowMask;
	#elif !defined (LIGHTMAP_ON)
		half4 shadowMask = unity_ProbesOcclusion;
	#else
		half4 shadowMask = half4(1, 1, 1, 1);
	#endif

	Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, shadowMask);
	#if defined(_SCREEN_SPACE_OCCLUSION)
		AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(inputData.normalizedScreenSpaceUV);
		mainLight.color *= aoFactor.directAmbientOcclusion;
		occlusion = min(occlusion, aoFactor.indirectAmbientOcclusion);
	#endif
	MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, half4(0, 0, 0, 0));

	half3 color = AdvGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.normalWS, inputData.viewDirectionWS);
	
	
	color += LightingRefractGlassBased(brdfData, mainLight, inputData.normalWS, inputData.viewDirectionWS, alpha, InnerColor);

	#ifdef _ADDITIONAL_LIGHTS
		uint pixelLightCount = GetAdditionalLightsCount();
		for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
		{
			Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
			color += LightingRefractGlassBased(brdfData, light, inputData.normalWS, inputData.viewDirectionWS, alpha, InnerColor);
		}
	#endif

	half VdN = saturate(dot(inputData.viewDirectionWS, inputData.normalWS));
	half invVdN = 1.0 - VdN;
	half Vc = invVdN * invVdN;
	half innerFalloff = VdN * VdN;
	innerFalloff *= innerFalloff;

	half transLevel = 1 - alpha;
	half transMask = saturate(pow(invVdN*0.999, 1.0/(1.0 - smoothness)));
	transMask = lerp(1, transMask, transLevel);

	half refraRoughness = 1.0 - (smoothness * RefractionGlossy);
	half eta = 1.0 / IOR;
	half rMip = (refraRoughness * (1.7 - 0.7 * refraRoughness)) * 6;
	float3 refraDir = refract(-inputData.viewDirectionWS, inputData.normalWS, eta);
	half4 refraEnv = SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0, samplerunity_SpecCube0, refraDir, rMip);
	half3 refraColor = lerp(1.0,RefractionColor.rgb, transLevel * RefractionColor.a * (1 - Vc) - transLevel + 1.0) * refraEnv.rgb;
	
	// color = lerp(refraColor, color, transMask);
	color += refraColor - transMask * refraColor;

	#ifdef _ADDITIONAL_LIGHTS_VERTEX
		color += inputData.vertexLighting * brdfData.diffuse;
	#endif

	color += emission;
	//color = filterColor;
	return half4(color, alpha);
}

#endif