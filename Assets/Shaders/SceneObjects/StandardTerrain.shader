Shader "GE/Scene/Terrain/StandardTerrain"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)]_Cull("Cull", Float) = 2
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[Header(Main Properties)]_Color("Color", Color) = (1,1,1,1)
		_Splat("Control", 2D) = "white" {}
		_Tex0("Splat0", 2D) = "gray" {}
		_Tex1("Splat1", 2D) = "gray" {}
		_Tex2("Splat2", 2D) = "gray" {}
		[Normal]_Normal0("Normal0", 2D) = "bump" {}
		[Normal]_Normal1("Normal1", 2D) = "bump" {}
		[Normal]_Normal2("Normal2", 2D) = "bump" {}
		_SmoothnessOffset0("SmoothnessOffset0", Range( -1 , 1)) = 0
		_SmoothnessOffset1("SmoothnessOffset1", Range( -1 , 1)) = 0
		_SmoothnessOffset2("SmoothnessOffset2", Range( -1 , 1)) = 0
		_SmoothnessScale0("SmoothnessScale0", Float) = 1
		_SmoothnessScale1("SmoothnessScale1", Float) = 1
		_SmoothnessScale2("SmoothnessScale2", Float) = 1

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

		//region HLSLINCLUDE
		HLSLINCLUDE
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
		#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
		#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
		#include "Assets/CGIncludes/GeCG.hlsl"

		struct AdvBRDFData
		{
			half3 diffuse;
			half3 specular;
			half metallic;
			half perceptualRoughness;
			half roughness;
			half roughness2;
			half grazingTerm;

			// We save some light invariant BRDF terms so we don't have to recompute
			// them in the light loop. Take a look at DirectBRDF function for detailed explaination.
			half normalizationTerm;     // roughness * 4.0 + 2.0
			half roughness2MinusOne;    // roughness^2 - 1.0
		};

		half U_D_GGX(AdvBRDFData brdfData, half HdN, half LdH2)
		{
			half a = brdfData.roughness;
			half a2 = brdfData.roughness2;
			float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;
			return a2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);
		}

		inline void AdvInitializeBRDFData(half3 albedo, half metallic, half specular, half smoothness, half alpha, out AdvBRDFData outBRDFData)
		{
			half oneMinusReflectivity = OneMinusReflectivityMetallic(metallic);
			half reflectivity = 1.0 - oneMinusReflectivity;
			// half baseVal = dot(half3(0.3, 0.59, 0.11), albedo);
			// half wBase = pow(baseVal, 0.454545) / baseVal;
			half speculerLevel = specular * 2.0;

			outBRDFData.diffuse = albedo * oneMinusReflectivity;
			outBRDFData.specular = lerp(kDieletricSpec.rgb, albedo, metallic) * speculerLevel;
			outBRDFData.metallic = metallic;
			outBRDFData.grazingTerm = saturate(smoothness + reflectivity) * speculerLevel;
			outBRDFData.perceptualRoughness = PerceptualSmoothnessToPerceptualRoughness(smoothness);
			outBRDFData.roughness = max(PerceptualRoughnessToRoughness(outBRDFData.perceptualRoughness), HALF_MIN);
			outBRDFData.roughness2 = outBRDFData.roughness * outBRDFData.roughness;

			outBRDFData.normalizationTerm = outBRDFData.roughness * 4.0h + 2.0h;
			outBRDFData.roughness2MinusOne = outBRDFData.roughness2 - 1.0h;

			#ifdef _ALPHAPREMULTIPLY_ON
				outBRDFData.diffuse *= alpha;
				alpha = alpha * oneMinusReflectivity + reflectivity;
			#endif
		}

		half3 AdvDirectBDRF(AdvBRDFData brdfData, half3 normalWS, half3 lightDirectionWS, half3 viewDirectionWS)
		{
			#ifndef _SPECULARHIGHLIGHTS_OFF
				float3 halfDir = SafeNormalize(float3(lightDirectionWS) + float3(viewDirectionWS));

				float NoH = saturate(dot(normalWS, halfDir));
				half LoH = saturate(dot(lightDirectionWS, halfDir));

				// GGX Distribution multiplied by combined approximation of Visibility and Fresnel
				// BRDFspec = (D * V * F) / 4.0
				// D = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2
				// V * F = 1.0 / ( LoH^2 * (roughness + 0.5) )
				// See "Optimizing PBR for Mobile" from Siggraph 2015 moving mobile graphics course
				// https://community.arm.com/events/1155

				// Final BRDFspec = roughness^2 / ( NoH^2 * (roughness^2 - 1) + 1 )^2 * (LoH^2 * (roughness + 0.5) * 4.0)
				// We further optimize a few light invariant terms
				// brdfData.normalizationTerm = (roughness + 0.5) * 4.0 rewritten as roughness * 4.0 + 2.0 to a fit a MAD.
				float d = NoH * NoH * brdfData.roughness2MinusOne + 1.00001f;

				half LoH2 = LoH * LoH;
				half specularTerm = brdfData.roughness2 / ((d * d) * max(0.1h, LoH2) * brdfData.normalizationTerm);

				// On platforms where half actually means something, the denominator has a risk of overflow
				// clamp below was added specifically to "fix" that, but dx compiler (we convert bytecode to metal/gles)
				// sees that specularTerm have only non-negative terms, so it skips max(0,..) in clamp (leaving only min(100,...))
				// #if defined (SHADER_API_MOBILE) || defined (SHADER_API_SWITCH)
				// 	specularTerm = specularTerm - HALF_MIN;
				// 	specularTerm = clamp(specularTerm, 0.0, 100.0); // Prevent FP16 overflow on mobiles
				// #endif
				specularTerm = clamp(specularTerm, 0.0, 25.0);

				half3 color = specularTerm * brdfData.specular + brdfData.diffuse;
				return color;
			#else
				return brdfData.diffuse;
			#endif
		}

		half3 AdvLightingPhysicallyBased(AdvBRDFData brdfData, half3 lightColor, half3 lightDirectionWS, half lightAttenuation, half3 normalWS, half3 viewDirectionWS)
		{
			half NdotL = saturate(dot(normalWS, lightDirectionWS));
			half3 radiance = lightColor * (lightAttenuation * NdotL);
			return AdvDirectBDRF(brdfData, normalWS, lightDirectionWS, viewDirectionWS) * radiance;
		}

		half3 AdvLightingPhysicallyBased(AdvBRDFData brdfData, Light light, half3 normalWS, half3 viewDirectionWS)
		{
			return AdvLightingPhysicallyBased(brdfData, light.color, light.direction, light.distanceAttenuation * light.shadowAttenuation, normalWS, viewDirectionWS);
		}

		half3 AdvEnvironmentBRDF(AdvBRDFData brdfData, half3 indirectDiffuse, half3 indirectSpecular, half fresnelTerm)
		{
			half3 c = indirectDiffuse * brdfData.diffuse;
			float surfaceReduction = 1.0 / (brdfData.roughness2 + 1.0);
			c += surfaceReduction * indirectSpecular * lerp(brdfData.specular, brdfData.grazingTerm, fresnelTerm);
			return c;
		}

		half3 AdvGlobalIllumination(AdvBRDFData brdfData, half3 bakedGI, half occlusion, half3 positionWS, half3 normalWS, half3 viewDirectionWS)
		{
			half3 reflectVector = reflect(-viewDirectionWS, normalWS);
			half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

			half3 indirectDiffuse = bakedGI * occlusion;
			half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, brdfData.perceptualRoughness, occlusion);

			return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
		}

		half4 UniversalAdvPBR(InputData inputData, half3 albedo, half metallic, half specular, half smoothness, half occlusion, half3 emission, half alpha)
		{
			AdvBRDFData brdfData;
			AdvInitializeBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

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

			half3 color = AdvGlobalIllumination(brdfData, inputData.bakedGI, occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
			color += AdvLightingPhysicallyBased(brdfData, mainLight, inputData.normalWS, inputData.viewDirectionWS);

			#ifdef _ADDITIONAL_LIGHTS
				uint pixelLightCount = GetAdditionalLightsCount();
				for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
				{
					Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
					color += AdvLightingPhysicallyBased(brdfData, light, inputData.normalWS, inputData.viewDirectionWS);
				}
			#endif

			#ifdef _ADDITIONAL_LIGHTS_VERTEX
				color += inputData.vertexLighting * brdfData.diffuse;
			#endif

			color += emission;
			return half4(color, alpha);
		}
		ENDHLSL
		//endregion HLSLINCLUDE
		
		Pass
		{
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset [_OffsetFactor],[_OffsetUnit]
			ColorMask RGBA

			HLSLPROGRAM
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ FOG_LINEAR
			#pragma multi_compile _ _COVERSNOW
			#pragma multi_compile _ _SNOWBRUSHMASK

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"

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
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _Tex0;
			sampler2D _Splat;
			sampler2D _Tex1;
			sampler2D _Tex2;
			sampler2D _Normal0;
			sampler2D _Normal1;
			sampler2D _Normal2;
			#if _SNOWBRUSHMASK
			sampler2D SnowMaskTex;
			float4 maskRect;
			#endif
			CBUFFER_START( UnityPerMaterial )
			float4 _Color;
			float4 _Tex0_ST;
			float4 _Tex1_ST;
			float4 _Tex2_ST;
			float _SmoothnessScale0;
			float _SmoothnessOffset0;
			float _SmoothnessScale1;
			float _SmoothnessOffset1;
			float _SmoothnessScale2;
			float _SmoothnessOffset2;
			CBUFFER_END


						
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
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
				o.fogFactor = positionCS.z;
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
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION) || defined(_CLOSEFADE_ON)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}

			half4 frag ( VertexOutput IN  ) : SV_Target
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
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
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

				float2 uv_Tex0 = IN.ase_texcoord7.xy * _Tex0_ST.xy + _Tex0_ST.zw;
				float4 tex2DNode5 = tex2D( _Tex0, uv_Tex0 );
				float2 texCoord18 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float4 tex2DNode17 = tex2D( _Splat, texCoord18 );
				float mask019 = tex2DNode17.r;
				float2 uv_Tex1 = IN.ase_texcoord7.xy * _Tex1_ST.xy + _Tex1_ST.zw;
				float4 tex2DNode14 = tex2D( _Tex1, uv_Tex1 );
				float mask120 = tex2DNode17.g;
				float2 uv_Tex2 = IN.ase_texcoord7.xy * _Tex2_ST.xy + _Tex2_ST.zw;
				float4 tex2DNode15 = tex2D( _Tex2, uv_Tex2 );
				float mask221 = tex2DNode17.b;
				float3 albedo37 = (( _Color * ( ( tex2DNode5 * mask019 ) + ( tex2DNode14 * mask120 ) + ( tex2DNode15 * mask221 ) ) )).rgb;
				
				float3 normal42 = UnpackNormalScale( ( ( tex2D( _Normal0, uv_Tex0 ) * mask019 ) + ( tex2D( _Normal1, uv_Tex1 ) * mask120 ) + ( tex2D( _Normal2, uv_Tex2 ) * mask221 ) ), 1.0 );
				
				float G055 = tex2DNode5.g;
				float G156 = tex2DNode14.g;
				float G257 = tex2DNode15.g;
				float smoothness84 = ( ( saturate( ( ( G055 * _SmoothnessScale0 ) + _SmoothnessOffset0 ) ) * mask019 ) + ( saturate( ( ( G156 * _SmoothnessScale1 ) + _SmoothnessOffset1 ) ) * mask120 ) + ( saturate( ( ( G257 * _SmoothnessScale2 ) + _SmoothnessOffset2 ) ) * mask221 ) );
				
				float3 Albedo = albedo37;
				float3 Normal = normal42;
				float3 Emission = 0;
				float Metallic = 0;
				float Smoothness = smoothness84;
				float Specular = 0.5;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				#ifdef _COVERSNOW
				//snow
				float snowProc = 1.0-_SnowControl;
				float snowTile = 0.001 * _SnowTiling;
				float detailMask = 1-saturate(abs(snowProc-0.5)*10-4);

				float smoothRange = lerp(1.0, 16.0, saturate(_SnowControl*4)); //16.0;
				float srScl = smoothRange / (smoothRange - 1.0);
				float srBias = 2.0 / smoothRange;

				float2 snowMaskUV = (WorldPosition.xz - _SnowRect.xy) / _SnowRect.zw;
				float2 snowUV = WorldPosition.xz * snowTile;
				float2 snowUV2 = snowUV * 0.25;
				float4 snowInfo = tex2D(_SnowInfo, snowUV);
				float4 snowInfo2 = tex2D(_SnowInfo, snowUV2);
				float4 snowArea = UNITY_SAMPLE_TEX2DARRAY(_SnowAreaArray, float3(snowMaskUV, _AreaIndex));
				float snowMask = snowArea.g;
				float coverMask = 1-0.75*saturate((snowMask - snowProc)*16);
				float3 snowColor = lerp(float3(0.122139,0.245342,0.456411) ,float3(0.896269,0.896269,0.896269),snowInfo.x*snowInfo2.x);
				snowMask = saturate(((snowMask - srScl*snowProc))*smoothRange - detailMask*(1-snowInfo.g));
				snowMask = lerp(snowMask, 1.0, snowArea.r);

				#if _SNOWBRUSHMASK
				float2 brushUV =  (WorldPosition.xz - maskRect.xy) / maskRect.zw;
				float bm  = tex2Dlod(SnowMaskTex, float4(brushUV, 0, 0)).r;
				bm = saturate(bm - (1-bm)*(1-snowInfo.g));
				bm = (brushUV.x > 1 || brushUV.x < 0 || brushUV.y > 1 || brushUV.y < 0) ? 1.0 : bm;
				snowMask *= bm;
				coverMask = lerp(1.0f, coverMask, bm);
				#endif

				float3 desatAlbedo = (dot(Albedo, float3(0.2126729, 0.7151522, 0.0721750))*float3(1.0,0.8,0.7) + Albedo*0.15) * 0.85;
				Albedo = lerp(lerp(desatAlbedo * 0.75, Albedo, coverMask), snowColor, snowMask);
				Smoothness = lerp(Smoothness, 0.65, snowMask);
				#endif
				
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

				#ifdef ASE_FOG
					inputData.fogCoord = ComputeFogFactor(IN.fogFactor.x);
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

				half4 color = UniversalAdvPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START( UnityPerMaterial )
			float4 _Color;
			float4 _Tex0_ST;
			float4 _Tex1_ST;
			float4 _Tex2_ST;
			float _SmoothnessScale0;
			float _SmoothnessOffset0;
			float _SmoothnessScale1;
			float _SmoothnessOffset1;
			float _SmoothnessScale2;
			float _SmoothnessOffset2;
			CBUFFER_END

			
			
			float3 _LightDirection;

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				
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
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START( UnityPerMaterial )
			float4 _Color;
			float4 _Tex0_ST;
			float4 _Tex1_ST;
			float4 _Tex2_ST;
			float _SmoothnessScale0;
			float _SmoothnessOffset0;
			float _SmoothnessScale1;
			float _SmoothnessOffset1;
			float _SmoothnessScale2;
			float _SmoothnessOffset2;
			CBUFFER_END


			
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
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
				float AlphaClipThreshold = 0.5;
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"

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

			sampler2D _Tex0;
			sampler2D _Splat;
			sampler2D _Tex1;
			sampler2D _Tex2;
			CBUFFER_START( UnityPerMaterial )
			float4 _Color;
			float4 _Tex0_ST;
			float4 _Tex1_ST;
			float4 _Tex2_ST;
			float _SmoothnessScale0;
			float _SmoothnessOffset0;
			float _SmoothnessScale1;
			float _SmoothnessOffset1;
			float _SmoothnessScale2;
			float _SmoothnessOffset2;
			CBUFFER_END


			
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
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

				float2 uv_Tex0 = IN.ase_texcoord2.xy * _Tex0_ST.xy + _Tex0_ST.zw;
				float4 tex2DNode5 = tex2D( _Tex0, uv_Tex0 );
				float2 texCoord18 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float4 tex2DNode17 = tex2D( _Splat, texCoord18 );
				float mask019 = tex2DNode17.r;
				float2 uv_Tex1 = IN.ase_texcoord2.xy * _Tex1_ST.xy + _Tex1_ST.zw;
				float4 tex2DNode14 = tex2D( _Tex1, uv_Tex1 );
				float mask120 = tex2DNode17.g;
				float2 uv_Tex2 = IN.ase_texcoord2.xy * _Tex2_ST.xy + _Tex2_ST.zw;
				float4 tex2DNode15 = tex2D( _Tex2, uv_Tex2 );
				float mask221 = tex2DNode17.b;
				float3 albedo37 = (( _Color * ( ( tex2DNode5 * mask019 ) + ( tex2DNode14 * mask120 ) + ( tex2DNode15 * mask221 ) ) )).rgb;
				
				
				float3 Albedo = albedo37;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

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