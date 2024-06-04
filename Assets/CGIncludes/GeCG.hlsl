#ifndef UNIVERSAL_PIPELINE_GECG_INCLUDED
#define UNIVERSAL_PIPELINE_GECG_INCLUDED

#define UNITY_DECLARE_TEX2DARRAY(tex) Texture2DArray tex; SamplerState sampler##tex
#define UNITY_SAMPLE_TEX2DARRAY(tex,coord) tex.Sample (sampler##tex,coord)

// 全域雪參數
float4 _SnowRect;
float _SnowControl;
sampler2D _SnowMask;
UNITY_DECLARE_TEX2DARRAY(_SnowAreaArray);
sampler2D _SnowInfo;
float _CoverSnowRange;
float _CoverSnowThickness;
float _SnowTiling;
int _AreaIndex;
#endif