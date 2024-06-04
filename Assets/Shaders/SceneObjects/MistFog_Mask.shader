Shader "GE/Level/MistFog_Mask"
{
	Properties
	{
		_Color("Color", Color) = (0.5,0.5,0.5,1.0)
		_Intensity("Intensity", Float) = 1
		
		[NoScaleOffset]_MainTex("MainTex", 2D) = "white" {}
		_FogDensity("Fog Density", Range(0,1)) = 0.5
		_NoiseSize("Noise Size",Float) = 120
		_SlideVector("Slide", Vector) = (0.02, 0, -0.01, 0.005)
		_Blend("Top Blend", float) = 0.15

		[Space(20)][Header(Fog Mask)]
		_RangeRect ("RangeRect", Vector) = (0,0,1080,1080)
		_BorderRange("BolderRange", Float) = 90
		_HeightFogDensity("Addition Fog Density", Range(0,1)) = 1.0
		[Toggle(_SNOWSTOME)] _SnowStome("with Snow Stome", Float) = 0
		[Toggle] _MaskOnStome("Mask On Stome", Float) = 1
		[NoScaleOffset]_MaskTex("mask", 2D) = "white" {}
		[NoScaleOffset]_MaskSelectTex("maskSelect", 2D) = "white" { }
		[NoScaleOffset]_BorderMask("Border Mask", 2D) = "black" {}
		_SelectColor("selecting color", Color) = (0.0, 0.0, 0.6, 1.0)

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
		Tags { "RenderPipeline"="UniversalPipeline" "IgnoreProjector" = "True" "Queue" = "Transparent+500" "RenderType" = "Transparent" }
		LOD 200

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

		HLSLINCLUDE
		
		ENDHLSL

		Pass
		{
			
			Name "Starndard"
			Tags { "LightMode"="UniversalForward" }

			Blend SrcAlpha OneMinusSrcAlpha
			ZWrite Off
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			
			HLSLPROGRAM
			#define REQUIRE_DEPTH_TEXTURE 1
			#define REQUIRE_OPAQUE_TEXTURE 1
			#pragma vertex vert
			#pragma fragment frag
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"

			#pragma shader_feature_local _SNOWSTOME

			struct appdata
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
			};

			struct v2f
			{
				float4 pos           : SV_POSITION;
				float3 worldPos      : TEXCOORD0;
				float4 worldSpaceDir : TEXCOORD1;
				float4 screenCoord   : TEXCOORD5;
			};

			float4 Pow2(float4 v){
				return v*v;
			}
			
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _MainTex;
			sampler2D _MaskTex;
			sampler2D _MaskSelectTex;
			sampler2D _BorderMask;
			float4 _SlideVector;
			float4 _FixTime;
			float4 _RangeRect;
			float _FogDensity;
			float _Intensity;
			float _NoiseSize;
			float _Blend;
			float _BorderRange;
			float _MaskOnStome;
			float4 _Color;
			float4 _SelectColor;
			float _HeightFogDensity;
			int _EnableSelectColor;

			float4 _TroopPosInfo0;
			float4 _TroopPosInfo1;
			float4 _TroopPosInfo2;
			float4 _TroopPosInfo3;
			float4 _TroopPosInfo4;

			v2f vert ( appdata v )
			{
				v2f o = (v2f)0;

				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.normal = v.normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				o.worldPos = positionWS;
				o.pos = positionCS;
				o.screenCoord = ComputeScreenPos(o.pos);

				// World space vector from camera to the vertex position
				o.worldSpaceDir.xyz = _WorldSpaceCameraPos.xyz - positionWS;

				// Z value of the vector in view space
				o.worldSpaceDir.w = mul(UNITY_MATRIX_V, float4(o.worldSpaceDir.xyz, 0.0)).z;

				return o;
			}

			half4 frag ( v2f i  ) : SV_Target
			{
				float3 worldPos = i.worldPos;
				float3 V = normalize(i.worldSpaceDir.xyz);
				float3 N = float3(0,1,0);
				float3 T = float3(1,0,0);
				float3 B = float3(0,0,1);
				float VdN = saturate(dot(V, N));
				float iVdN = 1.0 / VdN;

				float VdT = dot(V, T);
				float VdB = dot(V, B);
				float offT = ( VdT * iVdN ) * worldPos.y;
				float offB = ( VdB * iVdN ) * worldPos.y;

				float2 sceneUV = i.screenCoord.xy * rcp(i.screenCoord.w);

				float sceneZ = max(0, LinearEyeDepth(SHADERGRAPH_SAMPLE_SCENE_DEPTH(sceneUV), _ZBufferParams)) ;
				float partZ = max(0, i.screenCoord.w);
				float camHigh = (_WorldSpaceCameraPos.y - worldPos.y);
				float vDepth = ((sceneZ / partZ) * camHigh) - camHigh;
				half heightMask = saturate(vDepth * _Blend);

				i.worldSpaceDir *= -sceneZ / i.worldSpaceDir.w;
				float3 scnWorldPos = _WorldSpaceCameraPos.xyz + i.worldSpaceDir.xyz;
				// float sCamHeight = _WorldSpaceCameraPos.y;
				// float sCamDist = length(_WorldSpaceCameraPos.xyz - scnWorldPos);
				float2 parallaxPos = worldPos.xz - float2(offT,offB);
				float2 fogUV = ((scnWorldPos.xz - _RangeRect.xy) / _RangeRect.zw);
				float2 fogUV_p =  ((parallaxPos - _RangeRect.xy) / _RangeRect.zw);
				float2 borderUV = (parallaxPos - _RangeRect.xy + _BorderRange.xx) / (_RangeRect.zw + 2.0*_BorderRange.xx);
				half mask  = tex2Dlod(_MaskTex, float4(fogUV,0,2)).r;
				      mask += tex2Dlod(_MaskTex, float4(fogUV,0,3)).r;
				      mask += tex2Dlod(_MaskTex, float4(fogUV,0,4)).r;
				      mask *= 0.3333;

				half pMask  = tex2Dlod(_MaskTex, float4(fogUV_p,0,3)).r;
				      pMask += tex2Dlod(_MaskTex, float4(fogUV_p,0,3)).r;
				      pMask += tex2Dlod(_MaskTex, float4(fogUV_p,0,4)).r;
				      pMask *= 0.3333;
				
				float a = 1 - saturate(mask - pMask);
				mask = lerp(pMask, mask, a*a);
				
				half2 edge = saturate(abs(borderUV - 0.5)*2);
				half borderMask  = tex2Dlod(_BorderMask, float4(borderUV,0,0)).r;
				      borderMask += tex2Dlod(_BorderMask, float4(borderUV,0,2)).r;
				      borderMask *= 0.5;
				      borderMask  = ((edge.x>=1.0||edge.y>=1.0)?1.0:borderMask);
				    //   borderMask  = (edge.y>1.0?1.0:borderMask);

				half3 selectColor  = tex2D(_MaskSelectTex, fogUV).r;
				selectColor = saturate(_SelectColor.rgb * (sin(_FixTime.w) * 0.5 + 0.5) * selectColor * _EnableSelectColor);
				half sv = 1.0-dot(selectColor, half3(0.2126729, 0.7151522, 0.0721750));

				half base0  = tex2D(_MainTex, (worldPos.xz + float2(0.0,0.0))/ _NoiseSize      + _FixTime.y*_SlideVector.xy).r;
				      base0 *= tex2D(_MainTex, (parallaxPos + float2(0.2,0.3))/(_NoiseSize*1.5) + _FixTime.y*_SlideVector.zw).r;
				      base0  = sqrt(base0);
				      base0  = saturate(base0);

				#ifdef _SNOWSTOME
				half windFog = tex2D(_MainTex, worldPos.xz * 0.00625 + _FixTime.y * float2(-0.48, 0.3)).g;
				half flySnow = tex2D(_MainTex, worldPos.xz * 0.01250 + _FixTime.y * float2(-0.72, 0.3)).b;
				#endif

				half unitMask  = _TroopPosInfo0.w>0.5?saturate((distance(scnWorldPos.xz, float2(_TroopPosInfo0.xz)))*0.1-1.4):1.0;
				unitMask       *= _TroopPosInfo1.w>0.5?saturate((distance(scnWorldPos.xz, float2(_TroopPosInfo1.xz)))*0.1-1.4):1.0;
				unitMask       *= _TroopPosInfo2.w>0.5?saturate((distance(scnWorldPos.xz, float2(_TroopPosInfo2.xz)))*0.1-1.4):1.0;
				unitMask       *= _TroopPosInfo3.w>0.5?saturate((distance(scnWorldPos.xz, float2(_TroopPosInfo3.xz)))*0.1-1.4):1.0;
				unitMask       *= _TroopPosInfo4.w>0.5?saturate((distance(scnWorldPos.xz, float2(_TroopPosInfo4.xz)))*0.1-1.4):1.0;
				unitMask = saturate(unitMask);

				half3 Color =  pow(saturate((1-_FogDensity)*base0 + _FogDensity), 1.0/(_Color.rgb*0.99999+0.00001)) * _Intensity;
				#ifdef _SNOWSTOME
				Color = lerp(Color, 0.67220175.xxx, saturate(windFog + flySnow));
				#endif
				half3 fogLit = _MainLightColor.rgb*0.5 + SampleSHVertex(N)*0.5;
				Color = Color * sv;
				
				half Alpha = saturate(mask * unitMask);
				Alpha -= (1-_FogDensity)*0.5*saturate(1-base0);
				#ifdef _SNOWSTOME
				Alpha = lerp(Alpha, 1.0, saturate(windFog + flySnow)) * (_MaskOnStome?mask:1.0);
				#endif
				Alpha = lerp(Alpha, 1.0, borderMask);
				Alpha *= heightMask;
				Alpha = smoothstep(0,1,Alpha);
				
				half x = vDepth / worldPos.y;
				half y = saturate(2.5*sceneUV.y - 1.5);
				y*=y;
				x = x * y * base0;

				Color = lerp(unity_FogColor.rgb, Color, Alpha);
				Color = Color*fogLit + selectColor;
				Alpha = lerp(x * _HeightFogDensity, 1, Alpha);

				return half4( Color, Alpha );
			}
			ENDHLSL
		}
	}
}