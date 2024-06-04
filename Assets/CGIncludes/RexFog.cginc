#ifndef REX_FOG_INCLUDED
#define REX_FOG_INCLUDED

uniform float  _FogDensity, _DistanceFallOff, _ContrastTone;
uniform float _NoiseIntensity, _NoiseDistanceEnd, _NoiseScale;
uniform int _NoiseModeBlend;
uniform float3 _NoiseSpeed;
uniform float3 _LightDirection;
uniform float3 _Fog_MainLightDirection;
uniform float _DirectionalIntensity, _DirectionalFalloff;
uniform float3 _DirectionalColor;
uniform float _SkyboxFogIntensity;
uniform float _SkyboxFogHeight;
uniform float _SkyboxFogFill;
uniform float _CamearRangeMin;
uniform float _CamearRangeMax;
uniform float _FogHeightThinOff;
uniform float _FogHeightThickOff;
uniform float _FogHeightFallOff;
uniform float3 _DFogColorStart;
uniform float _DirectionalOcclusion;
uniform float _FogDistanceStart;
uniform float _FogDistanceEnd;
uniform sampler3D _FogNoise;
//float _FogColorDuo;
//float3 _DFogColorFar;

float3 mod3D289(float3 x) { return x - floor(x / 289.0) * 289.0; }
float4 mod3D289(float4 x) { return x - floor(x / 289.0) * 289.0; }
float4 permute(float4 x) { return mod3D289((x * 34.0 + 1.0) * x); }
float4 taylorInvSqrt(float4 r) { return 1.79284291400159 - r * 0.85373472095314; }
/*float snoise(float3 v)
{
	const float2 C = float2(1.0 / 6.0, 1.0 / 3.0);
	float3 i = floor(v + dot(v, C.yyy));
	float3 x0 = v - i + dot(i, C.xxx);
	float3 g = step(x0.yzx, x0.xyz);
	float3 l = 1.0 - g;
	float3 i1 = min(g.xyz, l.zxy);
	float3 i2 = max(g.xyz, l.zxy);
	float3 x1 = x0 - i1 + C.xxx;
	float3 x2 = x0 - i2 + C.yyy;
	float3 x3 = x0 - 0.5;
	i = mod3D289(i);
	float4 p = permute(permute(permute(i.z + float4(0.0, i1.z, i2.z, 1.0)) + i.y + float4(0.0, i1.y, i2.y, 1.0)) + i.x + float4(0.0, i1.x, i2.x, 1.0));
	float4 j = p - 49.0 * floor(p / 49.0);  // mod(p,7*7)
	float4 x_ = floor(j / 7.0);
	float4 y_ = floor(j - 7.0 * x_);  // mod(j,N)
	float4 x = (x_ * 2.0 + 0.5) / 7.0 - 1.0;
	float4 y = (y_ * 2.0 + 0.5) / 7.0 - 1.0;
	float4 h = 1.0 - abs(x) - abs(y);
	float4 b0 = float4(x.xy, y.xy);
	float4 b1 = float4(x.zw, y.zw);
	float4 s0 = floor(b0) * 2.0 + 1.0;
	float4 s1 = floor(b1) * 2.0 + 1.0;
	float4 sh = -step(h, 0.0);
	float4 a0 = b0.xzyw + s0.xzyw * sh.xxyy;
	float4 a1 = b1.xzyw + s1.xzyw * sh.zzww;
	float3 g0 = float3(a0.xy, h.x);
	float3 g1 = float3(a0.zw, h.y);
	float3 g2 = float3(a1.xy, h.z);
	float3 g3 = float3(a1.zw, h.w);
	float4 norm = taylorInvSqrt(float4(dot(g0, g0), dot(g1, g1), dot(g2, g2), dot(g3, g3)));
	g0 *= norm.x;
	g1 *= norm.y;
	g2 *= norm.z;
	g3 *= norm.w;
	float4 m = max(0.6 - float4(dot(x0, x0), dot(x1, x1), dot(x2, x2), dot(x3, x3)), 0.0);
	m = m * m;
	m = m * m;
	float4 px = float4(dot(x0, g0), dot(x1, g1), dot(x2, g2), dot(x3, g3));
	return 42.0 * dot(m, px);
}*/

half snoise(float3 v)
{
	return tex3D(_FogNoise, v).r;
}

float Remap(float from, float fromMin, float fromMax, float toMin, float toMax)
{
	float fromAbs = from - fromMin;
	float fromMaxAbs = fromMax - fromMin;

	float normal = fromAbs / fromMaxAbs;

	float toMaxAbs = toMax - toMin;
	float toAbs = toMaxAbs * normal;

	float to = toAbs + toMin;

	return to;
}

half4 FogAttenuation(float3 worldPos, float linearDepth01, float linearEyeDepth)
{
	#ifdef FOG_SKYBOX
	float3 normalize_WorldPosition = normalize(float3(worldPos.x, worldPos.y + _SkyboxFogHeight, worldPos.z));
	float saferPower309_g1045 = max(saturate(1 - normalize_WorldPosition.y), 0.0001);
	float lerpResult179_g1045 = lerp(pow(saferPower309_g1045, 4.0), 1.0, _SkyboxFogFill);
	float FogFactor = saturate(lerpResult179_g1045 * _SkyboxFogIntensity)+saturate(-normalize_WorldPosition.y);
	float3 normalizeResult318_ViewDist = normalize(worldPos);
	#else
	float HeightLerp = saturate((worldPos.y - _FogHeightThinOff) / (_FogHeightThickOff - _FogHeightThinOff));
	float HeightDensity = pow(HeightLerp, _FogHeightFallOff);
	float UE4FogDensity = exp(-_FogHeightFallOff * HeightLerp);
	float FogFactor = lerp(saturate((1 - exp2(-UE4FogDensity)) / UE4FogDensity) * HeightDensity ,HeightDensity, _FogHeightFallOff*0.1);
	float3 normalizeResult318_ViewDist = normalize(worldPos - _WorldSpaceCameraPos);
	#endif
	
	
	float dotResult145_LV = dot(normalizeResult318_ViewDist, _Fog_MainLightDirection.xyz);
	float appendResult233_g8762 = _Fog_MainLightDirection.y;
	float nolinearDepth01 = pow(linearDepth01, _DirectionalOcclusion*4.0);
	appendResult233_g8762 = smoothstep(-0.1, 0.05, appendResult233_g8762)* nolinearDepth01;
	float DirectionalMask30_g1045 = saturate(pow(abs(((dotResult145_LV* 0.5 + 0.5) * _DirectionalIntensity)), _DirectionalFalloff))* appendResult233_g8762;
	float3 _DFogColor = lerp(_DFogColorStart, _DirectionalColor, DirectionalMask30_g1045);

	float factor = (linearEyeDepth - _FogDistanceStart) / (_FogDistanceEnd - _FogDistanceStart);
	factor = saturate(factor);
	float heightRegulator = (1.0-saturate((_WorldSpaceCameraPos.y - _CamearRangeMin) / (_CamearRangeMax - _CamearRangeMin)));
	factor = smoothstep(0, _DistanceFallOff,factor) + heightRegulator * factor;

	UNITY_BRANCH
	if(_NoiseModeBlend > 0.5)
	{
		// BOXOPHOBIC
		float distanceView = distance(worldPos, _WorldSpaceCameraPos);
		float simplePerlin3D193_g1045 = snoise(((worldPos * (1.0 / _NoiseScale)) + (-_NoiseSpeed * _Time.y)));
		float NoiseDistanceMask7_g1045 = saturate(((distanceView - _NoiseDistanceEnd) / (0.0 - _NoiseDistanceEnd)));
		float NoiseSimplex3D24_g1045 = lerp(1.0, (simplePerlin3D193_g1045 * 0.5 + 0.5), (NoiseDistanceMask7_g1045 * _NoiseIntensity));
	
		// Custom
		/*float distanceView = distance(worldPos, _WorldSpaceCameraPos);
		float3 viewDir = normalize(worldPos - _WorldSpaceCameraPos);
		float simplePerlin3D193_g1045 = snoise(((viewDir + _WorldSpaceCameraPos.xyz * abs(_NoiseSpeed)) / _NoiseScale) + (-_NoiseSpeed * _Time.y));
		float NoiseDistanceMask7_g1045 = saturate(((distanceView - _NoiseDistanceEnd) / (0.0 - _NoiseDistanceEnd)));
		float3 noiseDir = normalize(_NoiseSpeed.xyz);
		float noiseDirFade = 1 - abs(dot(viewDir, noiseDir));
		float NoiseSimplex3D24_g1045 = lerp(1.0, (simplePerlin3D193_g1045 * 0.5 + 0.5), (noiseDirFade * NoiseDistanceMask7_g1045 * _NoiseIntensity));*/

		#ifdef FOG_SKYBOX
			factor = max(factor, NoiseSimplex3D24_g1045);
		#else
			factor *= NoiseSimplex3D24_g1045;
		#endif
	}

	//if (linear01Depth < _DepthluminanceSky * 0.01)
	//	col.rgb += col.rgb * linear01Depth * 1.5;
	//else
	//	col.rgb += col.rgb * Remap(linear01Depth, _DepthluminanceSky * 0.01, 1.0, _DepthluminanceSky * 0.01,0) * 1.5;
	
	float4 col = float4(_DFogColor.rgb * saturate(_FogDensity), saturate(FogFactor * factor * _FogDensity));
	#ifdef FOG_SKYBOX
	col.rgb = lerp(col.rgb, _DFogColor.rgb, FogFactor);
	col.a = saturate(max(col.a, FogFactor));
	#endif //FOG_SKYBOX
	return lerp(float4(_DFogColor.rgb * col.a, col.a),col, _ContrastTone);
}

half4 FogAttenuation(float3 worldPos)
{
	#ifdef FOG_SKYBOX
	float linearEyeDepth = _ProjectionParams.z;
	float linearDepth01 = 1;
	#else
	float near = _ProjectionParams.y;
	float far  = _ProjectionParams.z;
	float3 viewPos = mul(UNITY_MATRIX_V, float4(worldPos, 1)).xyz;
	float linearEyeDepth = max(dot(viewPos, float3(0,0,-1)), 0);
	float linearDepth01 = saturate((linearEyeDepth - near) / (far - near));
	#endif
	return FogAttenuation(worldPos, linearDepth01, linearEyeDepth);
}

half4 ApplyHeightFog(half4 col, float3 worldPos)
{
	half4 fog = FogAttenuation(worldPos);
	col.rgb = lerp(col.rgb, fog.rgb, fog.a);
	col.rgb = clamp(col.rgb, 0.0, 10.0);//这里必须限定下，不然值域会过大，加了Bloom后，会出现白团黑块的异常现象。
	return col;
}

#endif // REX_FOG_INCLUDED