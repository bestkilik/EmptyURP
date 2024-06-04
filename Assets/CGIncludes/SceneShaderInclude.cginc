#ifndef  SCENE_SHADER_INCLUDE_H
#define SCENE_SHADER_INCLUDE_H

#define UNITY_BRDF_PBS BRDF2_Unity_PBS
#define LIGHTMAP_SHADOW_MIXING 1

#include "UnityCG.cginc"
#include "UnityShaderVariables.cginc"            
#include "Lighting.cginc"
#include "UnityPBSLighting.cginc"
#include "AutoLight.cginc"
#include "RexCG.cginc"

#define WOBJ_POS float3(unity_ObjectToWorld[0][3], unity_ObjectToWorld[1][3], unity_ObjectToWorld[2][3])

#define NORMAL_VERT \
    float3 worldNormal = UnityObjectToWorldNormal(v.normal); \
    float3 worldTangent = UnityObjectToWorldDir(v.tangent.xyz); \
    float tangentSign = v.tangent.w * unity_WorldTransformParams.w; \
    float3 worldBinormal = cross(worldNormal, worldTangent) * tangentSign; \
    o.tSpace0 = float4(worldTangent.x, worldBinormal.x, worldNormal.x, worldPos.x); \
    o.tSpace1 = float4(worldTangent.y, worldBinormal.y, worldNormal.y, worldPos.y); \
    o.tSpace2 = float4(worldTangent.z, worldBinormal.z, worldNormal.z, worldPos.z)

#ifdef LIGHTMAP_ON
    #define LM_VERT \
    o.lmap = 0; \
    o.lmap.xy = v.texcoord1.xy * unity_LightmapST.xy + unity_LightmapST.zw
#else
    #define LM_VERT \
    o.lmap = 0; \
    o.lmap.rgb = ShadeSH9(float4(worldNormal, 1.0))
#endif
   
#define SHADOW_FOG \
    UNITY_TRANSFER_SHADOW(o, v.texcoord1.xy); \
    UNITY_TRANSFER_FOG(o,o.pos)

#define NORMAL_PIXEL \
    float3 worldN; \
    worldN.x = dot(i.tSpace0.xyz, normal); \
    worldN.y = dot(i.tSpace1.xyz, normal); \
    worldN.z = dot(i.tSpace2.xyz, normal); \
    worldN = normalize(worldN); \
    normal = worldN

#define INIT_GI \
    UnityLight light; \
    light.color = _LightColor0; \
    light.dir = lightDir; \
    UnityGIInput d; \
    d.ambient = 0; \
    d.light = light; \
    d.worldPos = worldPos; \
    d.worldViewDir = -viewDir; \
    d.atten = attenuation; \
    d.lightmapUV = i.lmap; \
    d.probeHDR[0] = unity_SpecCube0_HDR

inline float4 FastObjectToClipPos(float4 vertex, out float4 worldPos)
{
    worldPos = mul(unity_ObjectToWorld, float4(vertex.xyz, 1.0));
    return mul(UNITY_MATRIX_VP, worldPos);
}

inline UnityGI Fast_GI_Diffuse(UnityGIInput data, half occlusion)
{
    UnityGI o_gi;
    ResetUnityGI(o_gi);

#if defined(HANDLE_SHADOWS_BLENDING_IN_GI)
    half bakedAtten = UnitySampleBakedOcclusion(data.lightmapUV.xy, data.worldPos);
    float zDist = dot(_WorldSpaceCameraPos - data.worldPos, UNITY_MATRIX_V[2].xyz);
    float fadeDist = UnityComputeShadowFadeDistance(data.worldPos, zDist);
    data.atten = UnityMixRealtimeAndBakedShadows(data.atten, bakedAtten, UnityComputeShadowFade(fadeDist));
#endif


    o_gi.light = data.light;
    o_gi.light.color *= data.atten;

#ifdef LIGHTMAP_ON
    half4 bakedColorTex = UNITY_SAMPLE_TEX2D(unity_Lightmap, data.lightmapUV.xy);
    half3 bakedColor = DecodeLightmap(bakedColorTex);
#ifdef DIRLIGHTMAP_COMBINED
    fixed4 bakedDirTex = UNITY_SAMPLE_TEX2D_SAMPLER(unity_LightmapInd, unity_Lightmap, data.lightmapUV.xy);
    o_gi.indirect.diffuse = DecodeDirectionalLightmap(bakedColor, bakedDirTex, normalWorld);
#else
    o_gi.indirect.diffuse = bakedColor;
#endif

    o_gi.indirect.diffuse += data.ambient;
#else
    o_gi.indirect.diffuse = data.lightmapUV.rgb;
#endif

    o_gi.indirect.diffuse *= occlusion;
    o_gi.indirect.specular = 0;

    return o_gi;
}

inline UnityGI Fast_GI(UnityGIInput data, half occlusion, half3 normalWorld, float smooth)
{
    UnityGI o_gi;
    ResetUnityGI(o_gi);

#if defined(HANDLE_SHADOWS_BLENDING_IN_GI)
    half bakedAtten = UnitySampleBakedOcclusion(data.lightmapUV.xy, data.worldPos);
    float zDist = dot(_WorldSpaceCameraPos - data.worldPos, UNITY_MATRIX_V[2].xyz);
    float fadeDist = UnityComputeShadowFadeDistance(data.worldPos, zDist);
    data.atten = UnityMixRealtimeAndBakedShadows(data.atten, bakedAtten, UnityComputeShadowFade(fadeDist));
#endif

    o_gi.light = data.light;
    o_gi.light.color *= data.atten;

#ifdef LIGHTMAP_ON
    half4 bakedColorTex = UNITY_SAMPLE_TEX2D(unity_Lightmap, data.lightmapUV.xy);
    half3 bakedColor = DecodeLightmap(bakedColorTex);
#ifdef DIRLIGHTMAP_COMBINED
    fixed4 bakedDirTex = UNITY_SAMPLE_TEX2D_SAMPLER(unity_LightmapInd, unity_Lightmap, data.lightmapUV.xy);
    o_gi.indirect.diffuse = DecodeDirectionalLightmap(bakedColor, bakedDirTex, normalWorld);
#else
    o_gi.indirect.diffuse = bakedColor;
#endif

    o_gi.indirect.diffuse += data.ambient;
#else
    o_gi.indirect.diffuse = data.lightmapUV.rgb;
#endif

    o_gi.indirect.diffuse *= occlusion;

    Unity_GlossyEnvironmentData glossIn;
    glossIn.reflUVW = normalize(reflect(data.worldViewDir, normalWorld));
    glossIn.roughness = SmoothnessToPerceptualRoughness(smooth);

    half3 env0 = Unity_GlossyEnvironment(UNITY_PASS_TEXCUBE(unity_SpecCube0), data.probeHDR[0], glossIn);

    o_gi.indirect.specular = env0 * occlusion;

    return o_gi;
}

struct appdata_scene
{
    float4 vertex       : POSITION;
    float4 texcoord   : TEXCOORD0;
    float4 texcoord1 : TEXCOORD1;
    float4 texcoord2 : TEXCOORD2;
    float3 normal      : NORMAL;
    float4 tangent     : TANGENT;
    float4 color         : COLOR;

    UNITY_VERTEX_INPUT_INSTANCE_ID
};

#endif //SCENE_SHADER_INCLUDE_H