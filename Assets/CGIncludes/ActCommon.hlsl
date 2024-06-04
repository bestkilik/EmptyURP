#ifndef UNIVERSAL_PIPELINE_ACTCOMMON_INCLUDED
#define UNIVERSAL_PIPELINE_ACTCOMMON_INCLUDED
struct appdata_base {
    float4 vertex : POSITION;
    float3 normal : NORMAL;
    float4 texcoord : TEXCOORD0;
    UNITY_VERTEX_INPUT_INSTANCE_ID
};
// Legacy for compatibility with existing shaders
inline bool IsGammaSpace()
{
    #ifdef UNITY_COLORSPACE_GAMMA
    return true;
    #else
    return false;
    #endif
}
#ifdef UNITY_COLORSPACE_GAMMA
#define unity_ColorSpaceGrey half4(0.5, 0.5, 0.5, 0.5)
#define unity_ColorSpaceDouble half4(2.0, 2.0, 2.0, 2.0)
#define unity_ColorSpaceDielectricSpec half4(0.220916301, 0.220916301, 0.220916301, 1.0 - 0.220916301)
#define unity_ColorSpaceLuminance half4(0.22, 0.707, 0.071, 0.0) // Legacy: alpha is set to 0.0 to specify gamma mode
#else // Linear values
#define unity_ColorSpaceGrey half4(0.214041144, 0.214041144, 0.214041144, 0.5)
#define unity_ColorSpaceDouble half4(4.59479380, 4.59479380, 4.59479380, 2.0)
#define unity_ColorSpaceDielectricSpec half4(0.04, 0.04, 0.04, 1.0 - 0.04) // standard dielectric reflectivity coef at incident angle (= 4%)
#define unity_ColorSpaceLuminance half4(0.0396819152, 0.458021790, 0.00609653955, 1.0) // Legacy: alpha is set to 1.0 to specify linear mode
#endif
#endif