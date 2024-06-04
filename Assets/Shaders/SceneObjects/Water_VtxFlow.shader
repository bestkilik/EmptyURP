Shader "GE/Scene/Water_VtxFlow"
{
	Properties
	{
		[Space(15)][Header(Flow)]
		[Toggle]_invFlow ("Invert Flow", int) = 0
		[Enum(World,0,UV,1)]_FlowSpace("Flow Space", int) = 0
		_FlowSpeed ("Flow Speed", float) = 0.3

		[Space(15)][Header(Base)]
		_Smoothness ("Smoothness", Range(0, 1)) = 0.92
		_Specular ("Specular", Range(0, 1)) = 0.5
		_BumpScale ("Wave Normal Scale", Range(0, 2)) = 1
		_RefractStrength ("Refract Strength", float) = 0.3
		_WaveSpeed ("Wave Speed", float) = 5
		_WaveSize ("Wave Size", float) = 0.5
		[NoScaleOffset] _BumpMap ("Wave Normal", 2D) = "bump" {}
		_ShoreDepth ("邊緣範圍", Range(0, 2)) = 0.15
		_ShallowRange ("淺水區範圍", float) = 1.5
		_DeepRange("深水區範圍", float) = 4.5
		_VisibleDepth ("可視水深", float) = 2
		_VisibleDepthPow ("可視水深Power", Range(0, 3)) = 1
		_ColorShallow ("顏色-淺水區", Color) = (0.94, 1.0, 0.97, 1)
		_ColorWater ("顏色-中間", Color) = (0.73, 0.92, 1.0, 1)
		_ColorDeep ("顏色-深水區", Color) = (0.34, 0.76, 0.8, 1)
		_UnderWaterValue ("水下顏色明度", Range(0, 1)) = 0.75

		[Space(15)][Header(Water Sets)]
		[NoScaleOffset] _WaterSetsTex("水參數 R:Foam G:ShoreNoise", 2D) = "black" {}
		_FoamSize ("Foam Size", float) = 0.12
		_FoamSpeedScl("Foam Speed Scale", float) = 0.75
		_FoamColor ("Foam Color", color) = (1,1,1,1)
		_ShoreNoiseSize("Shore Noise Size", float) = 0.5
		_ShoreNoiseIntensity("Shore Noise Intensity", Range(0,1)) = 0.25
		[Toggle]_UseVerticleDepth("Use Verticle Depth", float) = 1

		[Space(20)][Header(Ice)]
		[NoScaleOffset]_IceMask("Ice Mask", 2D) = "white" {}
		[NoScaleOffset]_IceNormal("Ice Normal", 2D) = "bump" {}
		_IceFilterColor("Ice Filter Color", Color) = (0.0, 0.396, 0.44, 1)
		_IceSize ("Ice Size", float) = 0.4

		[Space(20)][Header(Depth)]
		_OffsetFactor ("Offset Factor", float) = 0
		_OffsetUnit ("Offset Unit", float) = 0

		[Space(20)][Header(Stencil Buffer)]
		[IntRange]_StencilRef("StencilRef", Range( 0 , 255)) = 0
		[IntRange]_StencilReadMask("Stencil ReadMask", Range( 0 , 255)) = 255
		[IntRange]_StencilWriteMask("Stencil WriteMask", Range( 0 , 255)) = 255
		[Enum(CompareFunction)]_StencilComp("Stencil Comp", Range( 0 , 8)) = 8
		[Enum(UnityEngine.Rendering.StencilOp)]_StencilPass("Stencil Pass", Float) = 0
		[Enum(UnityEngine.Rendering.StencilOp)]_StencilFail("Stencil Fail", Float) = 0
		[Enum(UnityEngine.Rendering.StencilOp)]_StencilZFail("Stencil ZFail", Float) = 0
	}

	SubShader
	{
		Tags { "RenderPipeline"="UniversalPipeline" "IgnoreProjector" = "True" "Queue" = "Transparent" "RenderType" = "Transparent" }
		LOD 200
		ZWrite Off

		Stencil
		{
			Ref [_StencilRef]
			ReadMask [_StencilReadMask]
			WriteMask [_StencilWriteMask]
			Comp [_StencilComp]
			Pass [_StencilPass]
			Fail [_StencilFail]
			ZFail [_StencilZFail]
		}

		pass
		{
			Name "Starndard"
			Tags { "LightMode" = "UniversalForward" }
			Offset [_OffsetFactor],[_OffsetUnit]
			Blend SrcAlpha OneMinusSrcAlpha

			HLSLPROGRAM
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1
			#pragma multi_compile _ FOG_LINEAR
			#pragma multi_compile _ _COVERSNOW

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Assets/CGIncludes/GeCG.hlsl"

			#pragma vertex vert
			#pragma fragment frag

			#define UNITY_PI 3.1415926535

			half D_GGX(half roughness, half HdN, half LdN, half VdN)
			{
				half a = roughness * roughness;
				half a2 = a * a;
				half Vis_SmithV = LdN * (VdN * (1 - a) + a);
				half Vis_SmithL = VdN * (LdN * (1 - a) + a);
				half Vis = 0.5 * rcp(Vis_SmithV + Vis_SmithL);
				float d = (HdN * a2 - HdN) * HdN + 1.0;
				return Vis * a2 / (UNITY_PI * d * d);
			}

			struct appdata
			{
				float4 vertex    : POSITION;
				float4 color     : COLOR;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 texcoord0 : TEXCOORD0;
			};

			struct v2f
			{
				float4 pos         : SV_POSITION;
				float4 color       : COLOR;
				float2 uv          : TEXCOORD0;
				float4 tSpace0     : TEXCOORD1;
				float4 tSpace1     : TEXCOORD2;
				float4 tSpace2     : TEXCOORD3;
				float4 screenCoord : TEXCOORD4;
				float4 fogCoord    : TEXCOORD5;
			};

			half3 DecodeHDR(half4 data, half4 decodeInstructions)
			{
				// Take into account texture alpha if decodeInstructions.w is true(the alpha value affects the RGB channels)
				half alpha = decodeInstructions.w * (data.a - 1.0) + 1.0;
				
				// If Linear mode is not supported we can skip exponent part
				#if defined(UNITY_COLORSPACE_GAMMA)
					return(decodeInstructions.x * alpha) * data.rgb;
				#else
					#if defined(UNITY_USE_NATIVE_HDR)
						return decodeInstructions.x * data.rgb; // Multiplier for future HDRI relative to absolute conversion.
					#else
						return(decodeInstructions.x * pow(abs(alpha), decodeInstructions.y)) * data.rgb;
					#endif
				#endif
			}

			sampler2D _BumpMap;
			sampler2D _WaterSetsTex;
			float4 _CameraDepthTexture_TexelSize;
			float4 _FixTime;

			CBUFFER_START( UnityPerMaterial )
			sampler2D _IceMask;
			sampler2D _IceNormal;
			float _IceSize;
			float _FlowSpeed;
			float _ShallowRange;
			float _DeepRange;
			float _WaveSize;
			float _Smoothness;
			float _Specular;
			float _BumpScale;
			float _RefractStrength;
			float _WaveSpeed;
			float _ShoreDepth;
			float _FoamSize;
			float _UnderWaterValue;
			float _VisibleDepth;
			float _VisibleDepthPow;
			float3 _ColorShallow;
			float3 _ColorDeep;
			float3 _ColorWater;
			float _VerticalDepth;
			float4 _FoamColor;
			float _ShoreNoiseSize;
			float _ShoreNoiseIntensity;
			int _invFlow;
			int _FlowSpace;
			float _FoamSpeedScl;
			half3 _IceFilterColor;
			float _UseVerticleDepth;
			CBUFFER_END

			v2f vert(appdata v)
			{
				v2f o = (v2f)0;
				
				o.pos = TransformObjectToHClip(v.vertex.xyz);
				
				float3 wPos = mul(GetObjectToWorldMatrix(), v.vertex).xyz;
				float3 wN = float3(0, 1, 0);
				float3 wT = float3(1, 0, 0);
				float3 wB = float3(0, 0, 1);
				
				o.tSpace0 = float4(wT.x, wB.x, wN.x, wPos.x);
				o.tSpace1 = float4(wT.y, wB.y, wN.y, wPos.y);
				o.tSpace2 = float4(wT.z, wB.z, wN.z, wPos.z);
				
				o.uv = v.texcoord0.xy;
				
				o.screenCoord = ComputeScreenPos(o.pos);
				// o.screenCoord.w = o.pos.w;
				o.color = v.color;
				o.fogCoord = o.pos.z;

				return o;
			}

			half4 frag(v2f i): SV_Target
			{
				float3 worldPos = float3(i.tSpace0.w, i.tSpace1.w, i.tSpace2.w);
				float3 T = normalize(float3(i.tSpace0.x, i.tSpace1.x, i.tSpace2.x));
				float3 B = normalize(float3(i.tSpace0.y, i.tSpace1.y, i.tSpace2.y));
				float3 N = normalize(float3(i.tSpace0.z, i.tSpace1.z, i.tSpace2.z));
				float3 V = normalize(_WorldSpaceCameraPos - worldPos);
				float3 L = _MainLightPosition.xyz;

				float2 flowUV = _FlowSpace == 0 ? worldPos.xz * 0.01 : i.uv;
				float2 uv = (flowUV / _WaveSize);
				float2 uvForm = (flowUV / _FoamSize);
				float2 uvSNoise = (flowUV / _ShoreNoiseSize);
				
				float2 flowVector = (i.color.rg * 2.0 - 1.0) * _FlowSpeed;
				if(_invFlow){
					flowVector = _invFlow ? -flowVector : flowVector;
				}
				
				float2 sceneUV = i.screenCoord.xy * rcp(i.screenCoord.w);

				#ifdef _COVERSNOW
					float2 iceUV = (flowUV / _IceSize);

					float snowProc = 1.0-_SnowControl;
					float snowTile = 0.001 * _SnowTiling;
					float smoothRange = lerp(1.0, 16.0, saturate(_SnowControl*4));
					float srScl = smoothRange / (smoothRange - 1.0);
					float2 snowUV = worldPos.xz * snowTile;
					float2 snowMaskUV = (worldPos.xz - _SnowRect.xy) / _SnowRect.zw;
					float4 snowInfo = tex2D(_SnowInfo, snowUV);
					float4 snowArea = UNITY_SAMPLE_TEX2DARRAY(_SnowAreaArray, float3(snowMaskUV, _AreaIndex));
					float snowMask = snowArea.g;
					float coverMask = 1-0.75*saturate((snowMask - snowProc)*16);

					snowMask = saturate( ((snowMask - srScl*snowProc)*smoothRange));
					snowMask = lerp(snowMask, 1.0, snowArea.r);

					half iceMask = tex2D(_IceMask, iceUV).r;
					float3 iceNormal = UnpackNormal(tex2D(_IceNormal, iceUV));
				#endif

				float t0 = frac(_Time.y * 0.1 - 0.25);
				float t1 = frac(_Time.y * 0.1 + 0.25);
				float flowMask = abs(2.0 * t0 - 1.0);
				float2 flowUV0 = t0 * flowVector;
				float2 flowUV1 = t1 * flowVector;
				#ifdef _COVERSNOW
					flowUV0 = flowUV0 - flowUV0*snowMask;
					flowUV1 = flowUV1 - flowUV1*snowMask;
				#endif

				half texForm =  lerp(tex2D(_WaterSetsTex, flowUV0*_FoamSpeedScl + uvForm).r, tex2D(_WaterSetsTex, flowUV1*_FoamSpeedScl + uvForm + float2(0.15,0.15)).r, flowMask);
				half formMask = texForm * (1 - i.color.b);

				float4 tNrm = lerp(tex2D(_BumpMap, flowUV0 + uv), tex2D(_BumpMap, flowUV1 + uv + float2(0.15,0.15)), flowMask);
				float3 tN = UnpackNormalScale(tNrm, _BumpScale);

				#ifdef _COVERSNOW
					tN = lerp(tN, iceNormal, snowMask);
				#endif

				float sceneZ = max(0, LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH(sceneUV), _ZBufferParams)) ;
				float partZ = max(0, i.screenCoord.w);
				
				float camHigh = (_WorldSpaceCameraPos.y - worldPos.y);
				float WaterDepth = ((sceneZ / partZ) * camHigh) - camHigh;
				WaterDepth = _UseVerticleDepth ? WaterDepth : saturate(sceneZ - partZ);
				float sDepth = _ShoreDepth;
				float sWaterDep = WaterDepth;
				float fixDepth = saturate(pow(abs(WaterDepth / _VisibleDepth), _VisibleDepthPow));
				float shoreBorder = saturate((sWaterDep / sDepth) * 10.0);
				float shallowDepth = saturate((sWaterDep / _ShallowRange) * 10.0);
				float deepDepth = saturate((sWaterDep / _DeepRange) * 10.0);

				float dScl = smoothstep(0.0, 0.25, WaterDepth);
				float2 distortionUV = sceneUV + dScl * dScl * 0.25 * shoreBorder * _RefractStrength * tN.xy;
				float4 projDistoUV = float4(distortionUV * i.screenCoord.w, 1.0, i.screenCoord.w);
				float distoZ = max(0.0, LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH(projDistoUV.xy * rcp(projDistoUV.w)), _ZBufferParams));
				float3 virDistoZ = ((distoZ / partZ) * (worldPos - GetCameraPositionWS().xyz)) + (GetCameraPositionWS().xyz - float3(0, worldPos.y, 0));
				virDistoZ *= -1;
				half3 sceneColor = virDistoZ.y < 0 ? SHADERGRAPH_SAMPLE_SCENE_COLOR(sceneUV).rgb: SHADERGRAPH_SAMPLE_SCENE_COLOR(distortionUV).rgb;

				half shoreNoise = tex2D(_WaterSetsTex, uvSNoise).g * _ShoreNoiseIntensity;
				shoreNoise *= (1.0 - shallowDepth);
				// shoreBorder -= shoreNoise;

				half3 waterColor = lerp(_ColorWater, _ColorDeep ,deepDepth);
				waterColor = lerp(_ColorShallow, waterColor, shallowDepth);
				sceneColor *= _UnderWaterValue*waterColor;

				float roughness = clamp(1 - _Smoothness, 0.1, 0.95);
				half specularLevel = _Specular * 2;
				half specularColor = 0.04;

				float3 wN = (N * tN.z) + (B * tN.y) + (T * tN.x);
				wN = normalize(wN);

				float3 L2 = float3(0.182913,0.487769,0.853595);
				float3 H = normalize(L2 + V);
				float3 H2 = V;
				half LdN = saturate(dot(L, wN));
				half HdN = dot(H, wN);
				half VdH = dot(V, H);
				half VdN = saturate(dot(V, wN));
				half invVdN = 1 - VdN;
				invVdN = pow(invVdN, 2);
				
				half fresnel = VdN;

				half3 diffColor = lerp(waterColor * 0.125h, 0.0h, invVdN);

				half3 gi_diff = SampleSHVertex(wN);
				half3 difLit = (LdN * _MainLightColor.rgb + gi_diff);
				half speLit = D_GGX(roughness, HdN, LdN, VdN) * LdN * specularLevel * 0.2h;
				speLit *= specularColor;
				speLit = clamp(speLit, 0, 2.0);

				half3 FresnelSchlickWithRoughness = invVdN * 0.96h + 0.04h;
				float3 worldRefl = reflect(-V, wN);
				half mip = roughness * (1.7 - 0.7 * roughness) * 6;
				half4 IBLSpec = SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0, samplerunity_SpecCube0, worldRefl, mip);
				IBLSpec.rgb = DecodeHDR(IBLSpec, unity_SpecCube0_HDR);
				IBLSpec.rgb *= FresnelSchlickWithRoughness * specularLevel;

				half4 outColor = 1;
				outColor.rgb = lerp(sceneColor, difLit * diffColor, fixDepth);
				

				#ifdef _COVERSNOW
					half3 innColor = sceneColor * _IceFilterColor.rgb;
					half iceSnowMask = ((1-saturate(sWaterDep * 0.03))+snowInfo.z)*0.5;
					half3 iceColor = lerp(innColor, difLit * half3(0.896269,0.896269,0.896269), saturate(iceMask + iceSnowMask));
					outColor.rgb = lerp(outColor.rgb, iceColor, snowMask);
					formMask *= coverMask;
				#endif
				outColor.rgb = lerp(outColor.rgb, difLit * _FoamColor.rgb, formMask);

				outColor.rgb += _MainLightColor.rgb * speLit + IBLSpec.rgb;
				outColor.rgb = MixFog(outColor.rgb, ComputeFogFactor(i.fogCoord.x));
				outColor.rgb = clamp(outColor.rgb, 0.0, 20.0);
				
				shoreBorder = lerp(shoreBorder, 1.0, saturate(partZ*0.2 - 20.0) );
				// outColor.rgb = i.color.a;
				outColor.a = shoreBorder * i.color.a;
				#if UNITY_REVERSED_Z
					outColor.rgb = half3(1,0,0);
				#else
					outColor.rgb = half3(0,1,0);
				#endif
				outColor.a = 1;

				return outColor;
			}
			ENDHLSL
		}
	}
}
