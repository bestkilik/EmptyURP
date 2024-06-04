Shader "GE/Character/SimpleCha_VtxAnm"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)]_Cull("Cull", Float) = 2
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[Header(Main Properties)]
		_Color("Color", Color) = (1,1,1,1)
		_MainTex("Base", 2D) = "white" {}
		_MorphTex("Morph Tex", 2D) = "black" {}
		_MorphNormal("Morph Normal", 2D)="white"{}
		_Progress ("Time Progress", Range(0,1)) = 0

		[Header(Base)]
		[Toggle(_ALPHATEST_ON)] _ALPHATEST("Alpha Test", Float) = 0
		_AlphaCutoff("Alpha Clip Threshold", Range( 0 , 1)) = 0.5

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
		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		LOD 200
		Cull [_Cull]
		
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
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

		inline half3 DecodeHDR (half4 data, half4 decodeInstructions)
		{
			// Take into account texture alpha if decodeInstructions.w is true(the alpha value affects the RGB channels)
			half alpha = decodeInstructions.w * (data.a - 1.0) + 1.0;

			// If Linear mode is not supported we can skip exponent part
			#if defined(UNITY_COLORSPACE_GAMMA)
				return (decodeInstructions.x * alpha) * data.rgb;
			#else
			#   if defined(UNITY_USE_NATIVE_HDR)
					return decodeInstructions.x * data.rgb; // Multiplier for future HDRI relative to absolute conversion.
			#   else
					return (decodeInstructions.x * pow(abs(alpha), decodeInstructions.y)) * data.rgb;
			#   endif
			#endif
		}

		half3 LambertLitBased(half3 albedo, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half3 normalWS, half3 viewDirectionWS)
		{
			half NdotL = saturate(dot(normalWS, lightDirectionWS));
			half3 radiance = lightColor * (lightAttenuation * NdotL);
			return albedo * radiance;
		}

		half3 LambertLitBased(half3 albedo, Light light, half3 normalWS, half3 viewDirectionWS)
		{
			return LambertLitBased(albedo, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, normalWS, viewDirectionWS);
		}

		half3 lambertGlobalIllumination(half3 albedo, half3 bakedGI, half occlusion)
		{
			half3 indirectDiffuse = bakedGI * occlusion;
			return indirectDiffuse * albedo;
		}

		half4 UniversalLambert(InputData inputData, half3 albedo, half occlusion, half3 emission, half alpha)
		{
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

			half3 color = lambertGlobalIllumination(albedo, inputData.bakedGI, occlusion);
			color += LambertLitBased(albedo, mainLight, inputData.normalWS, inputData.viewDirectionWS);

			#ifdef _ADDITIONAL_LIGHTS
				uint pixelLightCount = GetAdditionalLightsCount();
				for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
				{
					Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
					color += LambertLitBased(albedo, light, inputData.normalWS, inputData.viewDirectionWS);
				}
			#endif

			#ifdef _ADDITIONAL_LIGHTS_VERTEX
				color += inputData.vertexLighting * albedo;
			#endif

			color += emission;
			return half4(color, alpha);
		}
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			Blend Off
			ZWrite On
			ZTest LEqual
			
			HLSLPROGRAM
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_SRP_VERSION 999999

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _SHADOWS_SOFT
			
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma skip_variants FOG_EXP FOG_EXP2

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			
			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactor : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION) || defined(ASE_NEEDS_FRAG_SCREEN_POSITION_NORMALIZED)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _MorphTex; 
			sampler2D _MorphNormal;
			CBUFFER_START( UnityPerMaterial )
			float4 _MorphTex_TexelSize;
			float4 _MorphTex_HDR; 
			float4 _MainTex_ST;
			float _AlphaCutoff;
			CBUFFER_END

			UNITY_INSTANCING_BUFFER_START(PerInstance)
			UNITY_DEFINE_INSTANCED_PROP(float,_Progress)
			UNITY_DEFINE_INSTANCED_PROP(float4,_Color)
			UNITY_INSTANCING_BUFFER_END(PerInstance)

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;

				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	float3 defaultVertexValue = v.vertex.xyz;
				// #else
				// 	float3 defaultVertexValue = float3(0, 0, 0);
				// #endif
				// float3 vertexValue = defaultVertexValue;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	v.vertex.xyz = vertexValue;
				// #else
				// 	v.vertex.xyz += vertexValue;
				// #endif
				float halfH = _MorphTex_TexelSize.y * 0.5;
				float actFrame = floor((1-UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Progress))*_MorphTex_TexelSize.w)*_MorphTex_TexelSize.y + halfH;
				float2 vtxUV = float2(v.texcoord1.x, actFrame);
				float4 vtxOffset = tex2Dlod(_MorphTex, float4(vtxUV,0,0));
				vtxOffset.xyz = DecodeHDR(vtxOffset, _MorphTex_HDR);
				vtxOffset.xyz -= float3(2.5,0.5,2.5);
				v.vertex.xyz = vtxOffset.xyz;

				float4 vtxNrm = tex2Dlod(_MorphNormal, float4(vtxUV,0,0));
				vtxNrm.xyz = vtxNrm.xyz*2.0 - 1.0;
				v.normal = vtxNrm.xyz;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.normal, v.tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
				o.fogFactor = ComputeFogFactor( positionCS.z );
				#else
				o.fogFactor = 0;
				#endif
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION) || defined(ASE_NEEDS_FRAG_SCREEN_POSITION_NORMALIZED)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}

			half4 frag (VertexOutput IN, FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				float3 WorldNormal = normalize( IN.tSpace0.xyz );
				float3 WorldTangent = IN.tSpace1.xyz;
				float3 WorldBiTangent = IN.tSpace2.xyz;
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float camDist = distance(float3(0, 0, 0), WorldViewDirection);
				float4 ShadowCoords = float4(0, 0, 0, 0);
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION) || defined(ASE_NEEDS_FRAG_SCREEN_POSITION_NORMALIZED)
				float4 ScreenPos = IN.screenPos;
				float4 screenPosNrm = ScreenPos / ScreenPos.w;
				screenPosNrm.z = (UNITY_NEAR_CLIP_VALUE >= 0) ? screenPosNrm.z : screenPosNrm.z * 0.5 + 0.5;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord(WorldPosition);
				#endif

				WorldViewDirection = SafeNormalize(WorldViewDirection);

				float2 uv_MainTex = IN.ase_texcoord7.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float4 baseTex = tex2D( _MainTex, uv_MainTex );
				baseTex.rgb = lerp(baseTex.rgb*UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Color).rgb, baseTex.rgb, baseTex.a);
				
				float3 Albedo = baseTex.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = 0;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = _AlphaCutoff;

				#if defined(_ALPHATEST_ON)
				clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					inputData.normalWS = normalize(TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal )));
				#else
					#if !SHADER_HINT_NICE_QUALITY
						inputData.normalWS = WorldNormal;
					#else
						inputData.normalWS = normalize( WorldNormal );
					#endif
				#endif

				inputData.normalWS = ase_vface? inputData.normalWS: -inputData.normalWS;

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactor.x;
				#endif

				inputData.vertexLighting = 1.0;//IN.fogFactor.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.clipPos);
				inputData.shadowMask = SAMPLE_SHADOWMASK(IN.lightmapUVOrVertexSH.xy);

				half4 color = UniversalLambert(
					inputData, 
					Albedo, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef ASE_FOG
					color.rgb = MixFog(color.rgb, inputData.fogCoord);
				#endif

				return color;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _MorphTex; 
			CBUFFER_START( UnityPerMaterial )
			float4 _MorphTex_TexelSize;
			float4 _MorphTex_HDR; 
			float4 _MainTex_ST;
			float _AlphaCutoff;
			CBUFFER_END

			UNITY_INSTANCING_BUFFER_START(PerInstance)
			UNITY_DEFINE_INSTANCED_PROP(float,_Progress)
			UNITY_INSTANCING_BUFFER_END(PerInstance)

			float3 _LightDirection;

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord2.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	float3 defaultVertexValue = v.vertex.xyz;
				// #else
				// 	float3 defaultVertexValue = float3(0, 0, 0);
				// #endif
				// float3 vertexValue = defaultVertexValue;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	v.vertex.xyz = vertexValue;
				// #else
				// 	v.vertex.xyz += vertexValue;
				// #endif
				float halfH = _MorphTex_TexelSize.y * 0.5;
				float actFrame = floor((1-UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Progress))*_MorphTex_TexelSize.w)*_MorphTex_TexelSize.y + halfH;
				float2 vtxUV = float2(v.texcoord1.x, actFrame);
				float4 vtxOffset = tex2Dlod(_MorphTex, float4(vtxUV,0,0));
				vtxOffset.xyz = DecodeHDR(vtxOffset, _MorphTex_HDR);
				vtxOffset.xyz -= float3(2.5,0.5,2.5);
				v.vertex.xyz = vtxOffset.xyz;

				v.normal = v.normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.normal);

				float4 clipPos = TransformWorldToHClip( ApplyShadowBias( positionWS, normalWS, _LightDirection ) );

				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif


				#ifdef _ALPHATEST_ON
				float Alpha = 1.0;
				float AlphaClipThreshold = _AlphaCutoff;
				clip(Alpha - AlphaClipThreshold);
				#endif
				
				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _MorphTex; 
			CBUFFER_START( UnityPerMaterial )
			float4 _MorphTex_TexelSize;
			float4 _MorphTex_HDR; 
			float4 _MainTex_ST;
			float _AlphaCutoff;
			CBUFFER_END

			UNITY_INSTANCING_BUFFER_START(PerInstance)
			UNITY_DEFINE_INSTANCED_PROP(float,_Progress)
			UNITY_INSTANCING_BUFFER_END(PerInstance)
			
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	float3 defaultVertexValue = v.vertex.xyz;
				// #else
				// 	float3 defaultVertexValue = float3(0, 0, 0);
				// #endif
				// float3 vertexValue = defaultVertexValue;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	v.vertex.xyz = vertexValue;
				// #else
				// 	v.vertex.xyz += vertexValue;
				// #endif
				float halfH = _MorphTex_TexelSize.y * 0.5;
				float actFrame = floor((1-UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Progress))*_MorphTex_TexelSize.w)*_MorphTex_TexelSize.y + halfH;
				float2 vtxUV = float2(v.texcoord1.x, actFrame);
				float4 vtxOffset = tex2Dlod(_MorphTex, float4(vtxUV,0,0));
				vtxOffset.xyz = DecodeHDR(vtxOffset, _MorphTex_HDR);
				vtxOffset.xyz -= float3(2.5,0.5,2.5);
				v.vertex.xyz = vtxOffset.xyz;

				v.normal = v.normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				#ifdef _ALPHATEST_ON
				float Alpha = 1;
				float AlphaClipThreshold = _AlphaCutoff;
				clip(Alpha - AlphaClipThreshold);
				#endif
				
				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"

			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _MorphTex; 
			CBUFFER_START( UnityPerMaterial )
			float4 _MorphTex_TexelSize;
			float4 _MorphTex_HDR; 
			float4 _MainTex_ST;
			float _AlphaCutoff;
			CBUFFER_END

			UNITY_INSTANCING_BUFFER_START(PerInstance)
			UNITY_DEFINE_INSTANCED_PROP(float,_Progress)
			UNITY_DEFINE_INSTANCED_PROP(float4,_Color)
			UNITY_INSTANCING_BUFFER_END(PerInstance)

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	float3 defaultVertexValue = v.vertex.xyz;
				// #else
				// 	float3 defaultVertexValue = float3(0, 0, 0);
				// #endif
				// float3 vertexValue = defaultVertexValue;
				// #ifdef ASE_ABSOLUTE_VERTEX_POS
				// 	v.vertex.xyz = vertexValue;
				// #else
				// 	v.vertex.xyz += vertexValue;
				// #endif
				float halfH = _MorphTex_TexelSize.y * 0.5;
				float actFrame = floor((1-UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Progress))*_MorphTex_TexelSize.w)*_MorphTex_TexelSize.y + halfH;
				float2 vtxUV = float2(v.texcoord1.x, actFrame);
				float4 vtxOffset = tex2Dlod(_MorphTex, float4(vtxUV,0,0));
				vtxOffset.xyz = DecodeHDR(vtxOffset, _MorphTex_HDR);
				vtxOffset.xyz -= float3(2.5,0.5,2.5);
				v.vertex.xyz = vtxOffset.xyz;

				v.normal = v.normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex = IN.ase_texcoord2.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float4 baseTex = tex2D( _MainTex, uv_MainTex );
				baseTex.rgb = lerp(baseTex.rgb*UNITY_ACCESS_INSTANCED_PROP(PerInstance, _Color).rgb, baseTex.rgb, baseTex.a);

				float3 Albedo = baseTex.rgb;
				float3 Emission = 0;
				float Alpha = 1.0;
				float AlphaClipThreshold = _AlphaCutoff;

				#ifdef _ALPHATEST_ON
				clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
	}
	Fallback "Hidden/InternalErrorShader"
}