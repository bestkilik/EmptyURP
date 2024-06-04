Shader "GE/Level/MistFog_Circle"
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
		[Toggle(_SNOWSTOME)] _SnowStome("with Snow Stome", Float) = 0
		[NoScaleOffset]_BorderMask("Border Mask", 2D) = "black" {}

		[Space(20)][Header(Circle Info)]
		_CenterPos("Center Position", Vector) = (0,0,0,0)
		_Radius("Radius", Float) = 0.5

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
			sampler2D _BorderMask;
			float4 _SlideVector;
			float4 _FixTime;
			float4 _RangeRect;
			float _FogDensity;
			float _Intensity;
			float _NoiseSize;
			float _Blend;
			float _BorderRange;
			float4 _Color;

			float4 _CenterPos;
			float _Radius;

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
				vDepth = saturate(vDepth * _Blend);

				i.worldSpaceDir *= -sceneZ / i.worldSpaceDir.w;
				float3 scnWorldPos = _WorldSpaceCameraPos.xyz + i.worldSpaceDir.xyz;
				float2 parallaxPos = worldPos.xz - float2(offT,offB);

				float2 borderUV = (parallaxPos - _RangeRect.xy + _BorderRange.xx) / (_RangeRect.zw + 2.0*_BorderRange.xx);
				float2 edge = saturate(abs(borderUV - 0.5)*2);
				float borderMask  = tex2Dlod(_BorderMask, float4(borderUV,0,0)).r;
				      borderMask += tex2Dlod(_BorderMask, float4(borderUV,0,2)).r;
				      borderMask *= 0.5;
				      borderMask  = ((edge.x>=1.0||edge.y>=1.0)?1.0:borderMask);

				float m = distance( scnWorldPos.xz, _CenterPos.xz);
				float m0 = distance( parallaxPos, _CenterPos.xz);

				float4 base0  = tex2D(_MainTex, (worldPos.xz + float2(0.0,0.0))/ _NoiseSize      + _FixTime.y*_SlideVector.xy);
					   base0 *= tex2D(_MainTex, (parallaxPos + float2(0.2,0.3))/(_NoiseSize*1.5) + _FixTime.y*_SlideVector.zw);
					   base0  = sqrt(base0);
					   base0  = saturate(base0);

				#ifdef _SNOWSTOME
				float windFog = tex2D(_MainTex, worldPos.xz * 0.00625 + _FixTime.y * float2(-0.8, 0.5)).g;
				float flySnow = tex2D(_MainTex, worldPos.xz * 0.01250 + _FixTime.y * float2(-1.2, 0.5)).b;
				#endif

				float mi   = smoothstep(0,1,(m - _Radius)*0.1 + 0.75);
				float mi_0 = smoothstep(0,1,(m0 - _Radius)*0.1 + 0.75);

				float a = 1-saturate(mi - mi_0);
				mi = lerp(mi_0, mi, a*a);

				float3 Color =  pow(saturate((1-_FogDensity)*base0.r + _FogDensity), 1.0/(_Color.rgb*0.99999+0.00001)) * _Intensity;
				#ifdef _SNOWSTOME
				Color = lerp(Color, 0.896269.xxx, saturate(windFog + flySnow));
				#endif
				Color *= _MainLightColor.rgb*0.5 + SampleSHVertex(N)*0.5;

				float Alpha = saturate(mi);
				Alpha -= (1-_FogDensity)*saturate(1-base0.r);
				#ifdef _SNOWSTOME
				Alpha = lerp(Alpha, 1.0, saturate(windFog + flySnow)) * mi;
				#endif
				Alpha = lerp(Alpha, 1.0, borderMask);
				Alpha *= vDepth;
				Alpha = smoothstep(0,1,Alpha);
				return half4( Color, Alpha );
			}
			ENDHLSL
		}
	}
}