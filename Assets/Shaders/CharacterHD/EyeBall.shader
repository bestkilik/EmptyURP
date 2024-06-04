// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "GE/CharacterHD/EyeBall"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)]_Cull("Cull", Float) = 2
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		_IrisRadius("Iris Radius", Range( 0 , 0.5)) = 0.2
		_IrisDepthScale("Iris Depth Scale", Range( 0 , 0.2)) = 0.1
		_PupilScale("Pupil Scale", Range( 0 , 2)) = 0.75
		_LimbusColorWidth("Limbus Color Width", Range( 0 , 0.1)) = 0.03
		_LimbusShadingWidth("Limbus Shading Width", Range( 0 , 0.1)) = 0.04
		_ScleraColor("Sclera Color", Color) = (1,1,1,1)
		[NoScaleOffset]_MainTex("Sclera Tex", 2D) = "white" {}
		_IrisColor("Iris Color", Color) = (1,1,1,1)
		[NoScaleOffset]_IrisMap("Iris Map(A : Depth)", 2D) = "white" {}
		[NoScaleOffset]_BumpMap("Sclera Normal", 2D) = "bump" {}
		_ScoleraNormaXOffset("Scolera Norma X-Offset", Range( -0.5 , 0.5)) = 0
		_IrisNormal("Iris Normal", 2D) = "bump" {}
		_OcclutionMap("Occlution Map", 2D) = "white" {}
		_OcclutionIntensity("Occlution Intensity", Range( 0 , 1)) = 1
		_Smoothness("Smoothness", Range( 0 , 1)) = 0.9
		_Specular("Specular", Range( 0 , 1)) = 0.5
		[ASEEnd]_FwDir("Forward Direcion", Vector) = (0,-1,0,0)

		
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

		struct EyeballInputData
		{
			float3  positionWS;
			half3   normalWS;
			half3   viewDirectionWS;
			float4  shadowCoord;
			half    fogCoord;
			half3   vertexLighting;
			half3   bakedGI;
			float2  normalizedScreenSpaceUV;
			half4   shadowMask;
			half3   irisNormalWS;
		};

		struct eyeballBRDFData
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

		half D_GGX(half roughness, half HdN, half LdH2)
		{
			half a = roughness * roughness;
			half a2 = a * a;
			half normalizationTerm = a * 4.0h + 2.0h;
			float d = HdN * HdN * (a2 - 1.0f) + 1.00001f;
			return a2 / ((d * d) * max(0.1h, LdH2) * normalizationTerm);
		}

		half U_D_GGX(eyeballBRDFData brdfData, half HdN, half LdH2)
		{
			half a = brdfData.roughness;
			half a2 = brdfData.roughness2;
			float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;
			return a2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);
		}

		inline void InitializeEyeballBRDFData(half3 albedo, half metallic, half specular, half smoothness, half alpha, out eyeballBRDFData outBRDFData)
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

		half3 LightingEyeball(eyeballBRDFData brdfData, Light light, half3 N, half3 irisN, half3 V, half occlusion, half3 fwDirWS)
		{
			half atten = light.shadowAttenuation * light.distanceAttenuation;
			half3 L = light.direction;
			half LdN = dot(L, N);
			half sLdN = saturate(LdN);
			
			half diffOcc = occlusion * 0.5 + 0.5;
			half3 fakeL = normalize(half3(1.5,0.3,1.5)*L+fwDirWS);
			half fLdN = saturate(dot(fakeL, irisN));
			half3 H = SafeNormalize(fakeL + V);

			half HdN = saturate(dot(H, N));
			half HdN_Iris = dot(H, irisN);
			half LdH = saturate(dot(L, H));
			half LdH2 = LdH * LdH;

			half speLit = U_D_GGX(brdfData, HdN, LdH2);
			speLit = clamp(speLit, 0.0, 100.0);
			half speLit2 = clamp(D_GGX(0.4, HdN_Iris, LdH2), 0.0, 10.0);

			half3 color = (speLit + speLit2 * brdfData.diffuse) * brdfData.specular * fLdN * occlusion + brdfData.diffuse * sLdN * diffOcc;
			color *= light.color * atten;
			return color;
		}

		half3 AdvEnvironmentBRDF(eyeballBRDFData brdfData, half3 indirectDiffuse, half3 indirectSpecular, half fresnelTerm)
		{
			half3 c = indirectDiffuse * brdfData.diffuse;
			float surfaceReduction = 1.0 / (brdfData.roughness2 + 1.0);
			c += surfaceReduction * indirectSpecular * lerp(brdfData.specular, brdfData.grazingTerm, fresnelTerm);
			return c;
		}

		half3 GlobalIlluminationEyeball(eyeballBRDFData brdfData, half3 bakedGI, half occlusion, half3 positionWS, half3 normalWS, half3 viewDirectionWS)
		{
			half3 reflectVector = reflect(-viewDirectionWS, normalWS);
			half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

			half3 indirectDiffuse = bakedGI * occlusion;
			half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, brdfData.perceptualRoughness, occlusion);

			return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
		}

		half4 UniversalEyeballLit(EyeballInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half occlusion, half3 emission, half alpha, half3 fwDir)
		{
			eyeballBRDFData brdfData;
			InitializeEyeballBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

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

			half3 fwDirectionWS = normalize(mul((float3x3)UNITY_MATRIX_M, fwDir));

			half3 color = GlobalIlluminationEyeball(brdfData, inputData.bakedGI, occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
			color += LightingEyeball(brdfData, mainLight, inputData.normalWS, inputData.irisNormalWS, inputData.viewDirectionWS, occlusion, fwDirectionWS);

			#ifdef _ADDITIONAL_LIGHTS
				uint pixelLightCount = q();
				for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
				{
					Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
					color += LightingEyeball(brdfData, light, inputData.normalWS, inputData.irisNormalWS, inputData.viewDirectionWS, occlusion, fwDirectionWS);
				}
			#endif

			#ifdef _ADDITIONAL_LIGHTS_VERTEX
				color += inputData.vertexLighting * brdfData.diffuse;
			#endif

			color += emission;
			return half4(color, alpha);
		}
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			
			
			HLSLPROGRAM
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
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
			
			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT


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

			sampler2D _MainTex;
			sampler2D _IrisMap;
			sampler2D _BumpMap;
			sampler2D _IrisNormal;
			sampler2D _OcclutionMap;
			CBUFFER_START( UnityPerMaterial )
			float4 _ScleraColor;
			float4 _IrisColor;
			float3 _FwDir;
			float _IrisRadius;
			float _IrisDepthScale;
			float _PupilScale;
			float _LimbusColorWidth;
			float _LimbusShadingWidth;
			float _ScoleraNormaXOffset;
			float _Smoothness;
			float _Specular;
			float _OcclutionIntensity;
			CBUFFER_END


			float2 calcIrisUV31( half2 uv, half PupilScale )
			{
				half2 UVcentered = uv - half2(0.5, 0.5);
				half UVlength = length(UVcentered);
				// UV on circle at distance 0.5 from the center, in direction of original UV
				half2 UVmax = normalize(UVcentered)*0.5;
				half2 UVscaled = lerp(UVmax, half2(0.0, 0.0), saturate((1.0 - UVlength*2.0)*PupilScale));
				return UVscaled + half2(0.5, 0.5);
			}
			
			float2 calcIrisMask36( half2 uv, half IrisRadius, half2 LimbusUVWidth )
			{
				uv = uv - half2(0.5f, 0.5f);
				half2 m, r;
				r = (length(uv) - (IrisRadius - LimbusUVWidth)) / LimbusUVWidth;
				m = saturate(1 - r);
				m = smoothstep(0, 1, m);
				return m;
			}
			
			
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

				float2 texCoord4 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv5 = texCoord4;
				float temp_output_10_0 = ( 1.0 / ( _IrisRadius * 2.0 ) );
				float dotResult19 = dot( WorldViewDirection , WorldTangent );
				float dotResult21 = dot( WorldViewDirection , WorldBiTangent );
				float2 appendResult23 = (float2(dotResult19 , dotResult21));
				float2 uv31 = ( ( temp_output_10_0 * ( ( uv5 - ( appendResult23 * tex2D( _IrisMap, ( ( temp_output_10_0 * ( uv5 - float2( 0.5,0.5 ) ) ) + float2( 0.5,0.5 ) ) ).a * _IrisDepthScale ) ) - float2( 0.5,0.5 ) ) ) + float2( 0.5,0.5 ) );
				float PupilScale31 = _PupilScale;
				float2 localcalcIrisUV31 = calcIrisUV31( uv31 , PupilScale31 );
				float2 irisUV33 = localcalcIrisUV31;
				float2 uv36 = uv5;
				float IrisRadius36 = _IrisRadius;
				float2 appendResult41 = (float2(_LimbusColorWidth , _LimbusShadingWidth));
				float2 LimbusUVWidth36 = appendResult41;
				float2 localcalcIrisMask36 = calcIrisMask36( uv36 , IrisRadius36 , LimbusUVWidth36 );
				float2 break42 = localcalcIrisMask36;
				float irisMask44 = break42.x;
				float4 lerpResult53 = lerp( ( _ScleraColor * tex2D( _MainTex, uv5 ) ) , ( _IrisColor * tex2D( _IrisMap, irisUV33 ) ) , irisMask44);
				float3 albedo56 = (lerpResult53).rgb;
				
				float3 appendResult63 = (float3(_ScoleraNormaXOffset , 0.0 , 0.0));
				float3 normal64 = ( UnpackNormalScale( tex2D( _BumpMap, uv5 ), 1.0f ) + appendResult63 );
				
				float irisMaskSmooth43 = break42.y;
				
				float3 irisNormal68 = UnpackNormalScale( tex2D( _IrisNormal, irisUV33 ), 1.0f );
				
				float3 fwDir89 = _FwDir;
				
				float occlusion78 = ( 1.0 - ( ( 1.0 - tex2D( _OcclutionMap, uv5 ).r ) * _OcclutionIntensity ) );
				
				float3 Albedo = albedo56;
				float3 Normal = normal64;
				float3 Emission = 0;
				float Metallic = 0;
				float Smoothness = _Smoothness;
				float Specular = _Specular;
				float IrisMask = irisMaskSmooth43;
				float3 IrisNrm = irisNormal68;
				float3 FwDir = fwDir89;
				float Occlusion = occlusion78;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
				
				#if defined(_ALPHATEST_ON)
				clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				EyeballInputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				inputData.normalWS = normalize(TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal )));
				inputData.irisNormalWS = normalize(TransformTangentToWorld(IrisNrm, half3x3( WorldTangent, WorldBiTangent, WorldNormal )));
				inputData.irisNormalWS = lerp(inputData.normalWS, inputData.irisNormalWS, IrisMask);

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

				half4 color = UniversalEyeballLit(
					inputData,
					Albedo,
					Metallic,
					Specular,
					Smoothness,
					Occlusion,
					Emission,
					Alpha,
					FwDir);

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
			
			#pragma multi_compile_fog
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
			float4 _ScleraColor;
			float4 _IrisColor;
			float3 _FwDir;
			float _IrisRadius;
			float _IrisDepthScale;
			float _PupilScale;
			float _LimbusColorWidth;
			float _LimbusShadingWidth;
			float _ScoleraNormaXOffset;
			float _Smoothness;
			float _Specular;
			float _OcclutionIntensity;
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
			
			#pragma multi_compile_fog
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
			float4 _ScleraColor;
			float4 _IrisColor;
			float3 _FwDir;
			float _IrisRadius;
			float _IrisDepthScale;
			float _PupilScale;
			float _LimbusColorWidth;
			float _LimbusShadingWidth;
			float _ScoleraNormaXOffset;
			float _Smoothness;
			float _Specular;
			float _OcclutionIntensity;
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
			
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"

			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_VERT_TANGENT
			#define ASE_NEEDS_VERT_NORMAL


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
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _IrisMap;
			CBUFFER_START( UnityPerMaterial )
			float4 _ScleraColor;
			float4 _IrisColor;
			float3 _FwDir;
			float _IrisRadius;
			float _IrisDepthScale;
			float _PupilScale;
			float _LimbusColorWidth;
			float _LimbusShadingWidth;
			float _ScoleraNormaXOffset;
			float _Smoothness;
			float _Specular;
			float _OcclutionIntensity;
			CBUFFER_END


			float2 calcIrisUV31( half2 uv, half PupilScale )
			{
				half2 UVcentered = uv - half2(0.5, 0.5);
				half UVlength = length(UVcentered);
				// UV on circle at distance 0.5 from the center, in direction of original UV
				half2 UVmax = normalize(UVcentered)*0.5;
				half2 UVscaled = lerp(UVmax, half2(0.0, 0.0), saturate((1.0 - UVlength*2.0)*PupilScale));
				return UVscaled + half2(0.5, 0.5);
			}
			
			float2 calcIrisMask36( half2 uv, half IrisRadius, half2 LimbusUVWidth )
			{
				uv = uv - half2(0.5f, 0.5f);
				half2 m, r;
				r = (length(uv) - (IrisRadius - LimbusUVWidth)) / LimbusUVWidth;
				m = saturate(1 - r);
				m = smoothstep(0, 1, m);
				return m;
			}
			

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldTangent = TransformObjectToWorldDir(v.tangent.xyz);
				o.ase_texcoord3.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.normal);
				float ase_vertexTangentSign = v.tangent.w * unity_WorldTransformParams.w;
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord4.xyz = ase_worldBitangent;
				
				o.ase_texcoord2.xy = v.texcoord0.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				o.ase_texcoord3.w = 0;
				o.ase_texcoord4.w = 0;
				
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

				float2 texCoord4 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv5 = texCoord4;
				float temp_output_10_0 = ( 1.0 / ( _IrisRadius * 2.0 ) );
				float3 ase_worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				ase_worldViewDir = normalize(ase_worldViewDir);
				float3 ase_worldTangent = IN.ase_texcoord3.xyz;
				float dotResult19 = dot( ase_worldViewDir , ase_worldTangent );
				float3 ase_worldBitangent = IN.ase_texcoord4.xyz;
				float dotResult21 = dot( ase_worldViewDir , ase_worldBitangent );
				float2 appendResult23 = (float2(dotResult19 , dotResult21));
				float2 uv31 = ( ( temp_output_10_0 * ( ( uv5 - ( appendResult23 * tex2D( _IrisMap, ( ( temp_output_10_0 * ( uv5 - float2( 0.5,0.5 ) ) ) + float2( 0.5,0.5 ) ) ).a * _IrisDepthScale ) ) - float2( 0.5,0.5 ) ) ) + float2( 0.5,0.5 ) );
				float PupilScale31 = _PupilScale;
				float2 localcalcIrisUV31 = calcIrisUV31( uv31 , PupilScale31 );
				float2 irisUV33 = localcalcIrisUV31;
				float2 uv36 = uv5;
				float IrisRadius36 = _IrisRadius;
				float2 appendResult41 = (float2(_LimbusColorWidth , _LimbusShadingWidth));
				float2 LimbusUVWidth36 = appendResult41;
				float2 localcalcIrisMask36 = calcIrisMask36( uv36 , IrisRadius36 , LimbusUVWidth36 );
				float2 break42 = localcalcIrisMask36;
				float irisMask44 = break42.x;
				float4 lerpResult53 = lerp( ( _ScleraColor * tex2D( _MainTex, uv5 ) ) , ( _IrisColor * tex2D( _IrisMap, irisUV33 ) ) , irisMask44);
				float3 albedo56 = (lerpResult53).rgb;
				
				
				float3 Albedo = albedo56;
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
	
}/*ASEBEGIN
Version=18933
2301;67;1532;936;1056.392;258.4252;1;True;False
Node;AmplifyShaderEditor.CommentaryNode;35;-3475.924,-1231.847;Inherit;False;501;214;Comment;2;4;5;;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;4;-3425.924,-1176.847;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;34;-3508.626,-991.5148;Inherit;False;2920.82;1032.467;Comment;34;18;17;16;36;41;37;39;38;31;33;32;30;29;28;27;26;23;25;24;21;19;8;6;12;11;7;10;9;42;43;44;69;88;89;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;5;-3198.924,-1181.847;Inherit;False;uv;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;8;-3435.718,-922.5148;Inherit;False;Property;_IrisRadius;Iris Radius;0;0;Create;True;0;0;0;False;0;False;0.2;0;0;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;9;-3033.718,-916.5148;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;6;-3085.718,-777.5148;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;10;-2880.718,-941.5148;Inherit;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;7;-2883.718,-771.5148;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;18;-2636.327,-718.674;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;11;-2701.718,-838.5148;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.VertexBinormalNode;17;-2639.327,-422.6741;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.VertexTangentNode;16;-2638.327,-569.6737;Inherit;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleAddOpNode;12;-2536.718,-837.5148;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DotProductOpNode;21;-2402.664,-529.8127;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;19;-2401.664,-644.8128;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;25;-2301.264,-394.497;Inherit;False;Property;_IrisDepthScale;Iris Depth Scale;1;0;Create;True;0;0;0;False;0;False;0.1;0;0;0.2;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;23;-2232.264,-647.0128;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;69;-2357.544,-866.24;Inherit;True;Property;_TextureSample0;Texture Sample 0;8;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Instance;50;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;26;-2013.896,-825.8622;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;24;-2000.065,-648.4973;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;27;-1790.509,-753.1945;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;28;-1611.374,-751.4937;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;29;-1437.103,-934.5914;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;39;-3438.442,-116.299;Inherit;False;Property;_LimbusShadingWidth;Limbus Shading Width;4;0;Create;True;0;0;0;False;0;False;0.04;0;0;0.1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;38;-3438.442,-205.9972;Inherit;False;Property;_LimbusColorWidth;Limbus Color Width;3;0;Create;True;0;0;0;False;0;False;0.03;0;0;0.1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;30;-1272.484,-934.8092;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0.5,0.5;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;32;-1395.213,-792.6777;Inherit;False;Property;_PupilScale;Pupil Scale;2;0;Create;True;0;0;0;False;0;False;0.75;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.CustomExpressionNode;31;-1061.528,-932.7254;Inherit;False;half2 UVcentered = uv - half2(0.5, 0.5)@$half UVlength = length(UVcentered)@$// UV on circle at distance 0.5 from the center, in direction of original UV$half2 UVmax = normalize(UVcentered)*0.5@$$half2 UVscaled = lerp(UVmax, half2(0.0, 0.0), saturate((1.0 - UVlength*2.0)*PupilScale))@$return UVscaled + half2(0.5, 0.5)@;2;Create;2;True;uv;FLOAT2;0,0;In;;Half;False;True;PupilScale;FLOAT;0;In;;Half;False;calcIrisUV;True;False;0;;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;37;-3179.448,-276.5929;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.DynamicAppendNode;41;-3149.722,-163.6104;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;33;-811.8062,-932.6156;Inherit;False;irisUV;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CustomExpressionNode;36;-2962.972,-212.2798;Inherit;False;uv = uv - half2(0.5f, 0.5f)@$half2 m, r@$r = (length(uv) - (IrisRadius - LimbusUVWidth)) / LimbusUVWidth@$m = saturate(1 - r)@$m = smoothstep(0, 1, m)@$return m@;2;Create;3;True;uv;FLOAT2;0,0;In;;Half;False;True;IrisRadius;FLOAT;0;In;;Half;False;True;LimbusUVWidth;FLOAT2;0,0;In;;Half;False;calcIrisMask;True;False;0;;False;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;57;-3497.486,107.0844;Inherit;False;1396.154;893.9156;Comment;12;46;47;45;50;51;49;52;48;53;54;55;56;;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;51;-3435.486,792.0001;Inherit;False;33;irisUV;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;47;-3447.486,384.0001;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.BreakToComponentsNode;42;-2682.642,-211.9634;Inherit;False;FLOAT2;1;0;FLOAT2;0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.ColorNode;45;-3214.488,157.0844;Inherit;False;Property;_ScleraColor;Sclera Color;5;0;Create;True;0;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;50;-3258.486,771.0001;Inherit;True;Property;_IrisMap;Iris Map(A : Depth);8;1;[NoScaleOffset];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;49;-3175.486,587.0001;Inherit;False;Property;_IrisColor;Iris Color;7;0;Create;True;0;0;0;False;0;False;1,1,1,1;0,0,0,0;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;46;-3255.486,361.0001;Inherit;True;Property;_MainTex;Sclera Tex;6;1;[NoScaleOffset];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;44;-2495.693,-242.4614;Inherit;False;irisMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;48;-2885.486,429.0001;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;52;-2883.486,591.0001;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;54;-2907.486,708.0001;Inherit;False;44;irisMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;53;-2702.486,524.0001;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ComponentMaskNode;55;-2536.332,518.0513;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;56;-2325.332,519.0513;Inherit;False;albedo;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;82;-2025.892,868.0123;Inherit;False;1319;368.9999;Comment;7;70;73;74;76;75;77;78;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;65;-2023.78,127.6771;Inherit;False;983.8687;407.488;Comment;6;61;62;63;58;60;64;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;81;-2020.918,554.5011;Inherit;False;780.0303;280;Comment;3;66;68;67;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;89;-1314.706,-260.1666;Inherit;False;fwDir;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;78;-930.8921,941.0123;Inherit;False;occlusion;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;77;-1129.892,946.0123;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;75;-1582.892,1121.012;Inherit;False;Property;_OcclutionIntensity;Occlution Intensity;13;0;Create;True;0;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;76;-1278.892,945.0123;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;70;-1776.892,918.0123;Inherit;True;Property;_OcclutionMap;Occlution Map;12;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;63;-1620.911,376.165;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;73;-1975.892,941.0123;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;67;-1970.918,627.7247;Inherit;False;33;irisUV;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;43;-2501.642,-114.9634;Inherit;False;irisMaskSmooth;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;66;-1783.458,604.5011;Inherit;True;Property;_IrisNormal;Iris Normal;11;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.OneMinusNode;74;-1450.892,945.0123;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;60;-1973.78,201.6746;Inherit;False;5;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;64;-1263.911,185.165;Inherit;False;normal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;62;-1907.911,371.165;Inherit;False;Property;_ScoleraNormaXOffset;Scolera Norma X-Offset;10;0;Create;True;0;0;0;False;0;False;0;0;-0.5;0.5;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;84;-343.7306,26.36694;Inherit;False;Property;_Specular;Specular;15;0;Create;True;0;0;0;False;0;False;0.5;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;61;-1444.911,182.165;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;91;-249.3918,330.5748;Inherit;False;78;occlusion;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;86;-240.7306,171.3669;Inherit;False;68;irisNormal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;85;-272.7306,98.36694;Inherit;False;43;irisMaskSmooth;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;88;-1538.601,-256.3534;Inherit;False;Property;_FwDir;Forward Direcion;16;0;Create;False;0;0;0;False;0;False;0,-1,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;79;-257.6821,-210.2216;Inherit;False;56;albedo;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;80;-258.1635,-131.1835;Inherit;False;64;normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;83;-342.5693,-56.3374;Inherit;False;Property;_Smoothness;Smoothness;14;0;Create;True;0;0;0;False;0;False;0.9;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;68;-1464.887,604.9502;Inherit;False;irisNormal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;90;-247.3918,246.5748;Inherit;False;89;fwDir;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;58;-1794.706,177.6771;Inherit;True;Property;_BumpMap;Sclera Normal;9;1;[NoScaleOffset];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;True;-1;2;;200;11;GE/CharacterHD/EyeBall;b92c3807db6b12847a3b8a35f34f3fac;True;Forward;0;0;Forward;14;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;8;Alpha Test;0;0;Cast Shadows;1;0;Receive Shadows;1;0;GPU Instancing;0;638302781750722565;LOD CrossFade;0;638302781738963823;DOTS Instancing;0;0;Vertex Position,InvertActionOnDeselection;1;0;Built-in Fog;1;0;0;4;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;b92c3807db6b12847a3b8a35f34f3fac;True;Meta;0;3;Meta;1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;b92c3807db6b12847a3b8a35f34f3fac;True;ShadowCaster;0;1;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;b92c3807db6b12847a3b8a35f34f3fac;True;DepthOnly;0;2;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;5;0;4;0
WireConnection;9;0;8;0
WireConnection;10;1;9;0
WireConnection;7;0;6;0
WireConnection;11;0;10;0
WireConnection;11;1;7;0
WireConnection;12;0;11;0
WireConnection;21;0;18;0
WireConnection;21;1;17;0
WireConnection;19;0;18;0
WireConnection;19;1;16;0
WireConnection;23;0;19;0
WireConnection;23;1;21;0
WireConnection;69;1;12;0
WireConnection;24;0;23;0
WireConnection;24;1;69;4
WireConnection;24;2;25;0
WireConnection;27;0;26;0
WireConnection;27;1;24;0
WireConnection;28;0;27;0
WireConnection;29;0;10;0
WireConnection;29;1;28;0
WireConnection;30;0;29;0
WireConnection;31;0;30;0
WireConnection;31;1;32;0
WireConnection;41;0;38;0
WireConnection;41;1;39;0
WireConnection;33;0;31;0
WireConnection;36;0;37;0
WireConnection;36;1;8;0
WireConnection;36;2;41;0
WireConnection;42;0;36;0
WireConnection;50;1;51;0
WireConnection;46;1;47;0
WireConnection;44;0;42;0
WireConnection;48;0;45;0
WireConnection;48;1;46;0
WireConnection;52;0;49;0
WireConnection;52;1;50;0
WireConnection;53;0;48;0
WireConnection;53;1;52;0
WireConnection;53;2;54;0
WireConnection;55;0;53;0
WireConnection;56;0;55;0
WireConnection;89;0;88;0
WireConnection;78;0;77;0
WireConnection;77;0;76;0
WireConnection;76;0;74;0
WireConnection;76;1;75;0
WireConnection;70;1;73;0
WireConnection;63;0;62;0
WireConnection;43;0;42;1
WireConnection;66;1;67;0
WireConnection;74;0;70;1
WireConnection;64;0;61;0
WireConnection;61;0;58;0
WireConnection;61;1;63;0
WireConnection;68;0;66;0
WireConnection;58;1;60;0
WireConnection;0;0;79;0
WireConnection;0;1;80;0
WireConnection;0;4;83;0
WireConnection;0;9;84;0
WireConnection;0;11;85;0
WireConnection;0;12;86;0
WireConnection;0;13;90;0
WireConnection;0;5;91;0
ASEEND*/
//CHKSM=E49B3801F2158A2CFAF999106AB5511EF2C555EE