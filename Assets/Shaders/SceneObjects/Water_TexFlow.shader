Shader "GE/Scene/Water_TexFlow"
{
	Properties
	{
		[Space(15)][Header(Flow)]
		[NoScaleOffset] _FlowMap("Flow Map", 2D) = "black" {}
		[Toggle]_invFlow ("Invert Flow", int) = 0
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

		[Space(15)][Header(Foam)]
		[NoScaleOffset] _FoamTex("泡沫貼圖", 2D) = "black" {}
		_FoamSize ("Foam Size", float) = 0.12
		_FoamColor ("Foam Color", color) = (1,1,1,1)

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
			#pragma vertex vert
			#pragma fragment frag
			
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define UNITY_PI 3.1415926535

			half D_GGX(half roughness, half HdN, half LdN, half VdN)
			{
				half a = roughness * roughness;
				half a2 = a * a;
				half Vis_SmithV = LdN * (VdN * (1 - a) + a);
				half Vis_SmithL = VdN * (LdN * (1 - a) + a);
				half Vis = 0.5 * rcp(Vis_SmithV + Vis_SmithL);
				half d = (HdN * a2 - HdN) * HdN + 1;
				return Vis * a2 / (UNITY_PI * d * d);
			}

			struct appdata
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 texcoord0 : TEXCOORD0;
			};

			struct v2f
			{
				float4 pos         : SV_POSITION;
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
			sampler2D _FoamTex;
			sampler2D _FlowMap;
			float4 _CameraDepthTexture_TexelSize;
			float4 _FixTime;
			
			CBUFFER_START( UnityPerMaterial )
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
			int _invFlow;
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

				float2 uv = (worldPos.xz / _WaveSize) * 0.01;
				float2 uvForm = (worldPos.xz / _FoamSize) * 0.01;
				
				float4 waterInfo = tex2D(_FlowMap, i.uv.xy);

				float2 flowVector = (waterInfo.rg * 2.0 - 1.0) * _FlowSpeed;
				if(_invFlow){
					flowVector = _invFlow ? -flowVector : flowVector;
				}
				float t0 = frac(_FixTime.y * 0.1 - 0.25);
				float t1 = frac(_FixTime.y * 0.1 + 0.25);
				float flowMask = abs(2.0 * t0 - 1.0);
				float2 flowUV0 = t0 * flowVector;
				float2 flowUV1 = t1 * flowVector;

				float4 texForm =  lerp(tex2D(_FoamTex, flowUV0*0.5 + uvForm), tex2D(_FoamTex, flowUV1*0.5 + uvForm + float2(0.15,0.15)), flowMask);
				float formMask = texForm.a * (1 - waterInfo.a);

				float4 tNrm = lerp(tex2D(_BumpMap, flowUV0 + uv), tex2D(_BumpMap, flowUV1 + uv + float2(0.15,0.15)), flowMask);
				float3 tN = UnpackNormalScale(tNrm, _BumpScale);

				float3 wN = (N * tN.z) + (B * tN.y) + (T * tN.x);
				wN = normalize(wN);

				float2 sceneUV = i.screenCoord.xy * rcp(i.screenCoord.w);

				float sceneZ = max(0, LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH(sceneUV), _ZBufferParams)) ;
				float partZ = max(0, i.screenCoord.w);
				
				float camHigh = (_WorldSpaceCameraPos.y - worldPos.y);
				float WaterDepth = ((sceneZ / partZ) * camHigh) - camHigh;
				float sDepth = _ShoreDepth;
				float sWaterDep = WaterDepth * 10;
				float fixDepth = saturate(pow(abs(WaterDepth / _VisibleDepth), _VisibleDepthPow));
				float shoreBorder = saturate(sWaterDep / sDepth);
				float shallowDepth = saturate(sWaterDep / _ShallowRange);
				float deepDepth = saturate(sWaterDep / _DeepRange);

				float dScl = smoothstep(0.0, 0.25, WaterDepth);
				float2 distortionUV = sceneUV + dScl * dScl * 0.25 * shoreBorder * _RefractStrength * tN.xy;
				float4 projDistoUV = float4(distortionUV * i.screenCoord.w, 1.0, i.screenCoord.w);
				float distoZ = max(0.0, LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH(projDistoUV.xy * rcp(projDistoUV.w)), _ZBufferParams));
				float3 virDistoZ = ((distoZ / partZ) * (worldPos - GetCameraPositionWS().xyz)) + (GetCameraPositionWS().xyz - float3(0, worldPos.y, 0));
				virDistoZ *= -1;
				half3 sceneColor = virDistoZ.y < 0 ? SHADERGRAPH_SAMPLE_SCENE_COLOR(sceneUV).rgb: SHADERGRAPH_SAMPLE_SCENE_COLOR(distortionUV).rgb;
				sceneColor *= _UnderWaterValue;

				half3 waterColor = lerp(_ColorWater, _ColorDeep ,deepDepth);
				waterColor = lerp(_ColorShallow, waterColor, shallowDepth);

				float roughness = clamp(1 - _Smoothness, 0.05, 0.95);
				half specularLevel = _Specular * 2;
				half specularColor = 0.04;

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

				half3 FresnelSchlickWithRoughness = invVdN * 0.96h + 0.04h;
				float3 worldRefl = reflect(-V, wN);
				half mip = roughness * (1.7 - 0.7 * roughness) * 6;
				half4 IBLSpec = SAMPLE_TEXTURECUBE_LOD(unity_SpecCube0, samplerunity_SpecCube0, worldRefl, mip);
				IBLSpec.rgb = DecodeHDR(IBLSpec, unity_SpecCube0_HDR);
				IBLSpec.rgb *= FresnelSchlickWithRoughness * specularLevel;

				half4 outColor;// = 1;
				outColor.rgb = lerp(sceneColor*waterColor, difLit * diffColor, fixDepth);
				outColor.rgb = lerp(outColor.rgb, difLit * _FoamColor.rgb, formMask);
				outColor.rgb += _MainLightColor.rgb * speLit + IBLSpec.rgb;
				outColor.rgb = MixFog(outColor.rgb, ComputeFogFactor(i.fogCoord.x));
				outColor.rgb = clamp(outColor.rgb, 0.0, 100.0);
				outColor.a = shoreBorder;

				return outColor;
			}
			ENDHLSL
		}
	}
}
