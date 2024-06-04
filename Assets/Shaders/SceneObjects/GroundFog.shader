Shader "GE/Scene/GroundFog"
{
	Properties
	{
		_Color("Color", Color) = (0.5,0.5,0.5,1.0)
		[NoScaleOffset]_MainTex("Fog Tex", 2D) = "white" {}
		[NoScaleOffset]_MaskTex("Mask Tex", 2D) = "white" {}
		_FogDensity("Fog Density", Range(0,1)) = 0.65
		_FogStrength("Fog Strength", Range(0,1)) = 0.25
		_NoiseSize("Noise Size",Float) = 60
		_SlideVector("Slide", Vector) = (-6, -0.1, 2, -2)

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
		Tags { "RenderPipeline"="UniversalPipeline" "IgnoreProjector" = "True" "Queue" = "Transparent+60" "RenderType" = "Transparent" }
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
			#pragma vertex vert
			#pragma fragment frag
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"

			struct appdata
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 texcoord0 : TEXCOORD0;
			};

			struct v2f
			{
				float4 pos           : SV_POSITION;
				float3 worldPos      : TEXCOORD0;
				float4 worldSpaceDir : TEXCOORD1;
				float4 uv0           : TEXCOORD2;
				float4 screenCoord   : TEXCOORD3;
			};

			float4 Pow2(float4 v){
				return v*v;
			}
			
			float4 _FixTime;
			CBUFFER_START( UnityPerMaterial )
			uniform float4 _CameraDepthTexture_TexelSize;
			sampler2D _MainTex;
			sampler2D _MaskTex;
			float4 _SlideVector;
			float4 _RangeRect;
			float _FogDensity;
			float _NoiseSize;
			float4 _Color;
			float _FogStrength;
			CBUFFER_END

			v2f vert ( appdata v )
			{
				v2f o = (v2f)0;

				o.uv0.xy = v.texcoord0.xy;
				o.uv0.zw = 0;

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

			half4 frag ( v2f i ) : SV_Target
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

				float2 parallaxPos = worldPos.xz - float2(offT,offB);

				half mask  = tex2D(_MaskTex, i.uv0.xy).r;
				float4 slideVector = _SlideVector * 0.01;
				half base0  = tex2D(_MainTex, (worldPos.xz + float2(0.0,0.0))/ _NoiseSize      + _Time.y*slideVector.xy).r;
				base0 *= tex2D(_MainTex, (parallaxPos + float2(0.2,0.3))/(_NoiseSize*1.5) + _Time.y*slideVector.zw).r;
				base0  = sqrt(base0);
				base0  = saturate(base0 - (1 - _FogDensity));

				half3 fogLit = _MainLightColor.rgb*0.5 + SampleSHVertex(N)*0.5;

				half hMask = saturate(vDepth / worldPos.y);

				half3 Color = _Color.rgb;
				Color = Color*fogLit;
				half Alpha = hMask * base0 * _FogStrength * mask;

				return half4( Color, Alpha );
			}
			ENDHLSL
		}
	}
}