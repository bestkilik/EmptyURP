// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "GE/CharacterHD/StandardHair"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)]_Cull("Cull", Float) = 2
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[Header(Base)][Toggle(_ALPHATEST_ON)] _ALPHATEST("Alpha Test", Float) = 0
		[NoScaleOffset]_MainTex("Base", 2D) = "white" {}
		[NoScaleOffset]_BumpMap("Normal Map", 2D) = "bump" {}
		[Toggle]_UsePhysicMap("UsePhysicMap", Float) = 0
		_PhysicMap("PhysicMap(MSO", 2D) = "white" {}
		_Scatter("Scatter", Range( 0 , 1)) = 0.3
		_Smoothness("Smoothness", Range( 0 , 1)) = 0.5
		[NoScaleOffset][Space(15)][Header(Hair Info)]_FlowMap("FlowMap(RG:Flow, B:Jitter)", 2D) = "white" {}
		_JitterTiling("Jitter Tiling", Float) = 1
		_JitterScale("Jitter Scale", Range( 0 , 2)) = 1
		[ASEEnd]_JitterOffset("Jitter Offset", Range( -1 , 1)) = 0

		
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
		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Transparent" "Queue"="AlphaTest" }
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

		struct HairInputData
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
			half3   tangentWS;
		};

		struct hairBRDFData
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

		half U_D_GGX(hairBRDFData brdfData, half HdN, half LdH2)
		{
			half a = brdfData.roughness;
			half a2 = brdfData.roughness2;
			float d = HdN * HdN * brdfData.roughness2MinusOne + 1.00001f;
			return a2 / ((d * d) * max(0.1h, LdH2) * brdfData.normalizationTerm);
		}

		inline void InitializeHairBRDFData(half3 albedo, half metallic, half specular, half smoothness, half alpha, out hairBRDFData outBRDFData)
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

		half3 LightingHair(hairBRDFData brdfData, Light light, half3 N, half3 T, half3 V, half hairMask, half occlusion)
		{
			half atten = light.shadowAttenuation * light.distanceAttenuation;
			half3 L = light.direction;
			half LdN = dot(L, N);
			half sLdN = saturate(LdN);

			half diffOcc = occlusion*0.5+0.5;

			half KajiyaDiffuse = saturate(1 - abs( dot(T, L)));
			half3 FakeNormal = normalize(V - T * dot(V, T));
			half NoL = saturate((dot(FakeNormal, L) + 1 ) / 4.0);
			half DiffuseScatter = lerp(NoL, KajiyaDiffuse, 0.33);
			half scatter = 1.0 - brdfData.metallic;
			half t2Scl = 2.0 - 1.5 * scatter;

			half3 H = SafeNormalize(L + V);
			half HdN = dot(H, N);
			half LdH = saturate(dot(L, H));
			half LdH2 = LdH*LdH;

			half HdT = dot(H, T);
			HdT *= HdT;
			HdT = sqrt(1.0 - HdT);
			HdN = lerp(saturate(HdN), HdT, hairMask);

			half speLit = U_D_GGX(brdfData, HdN, LdH2) * lerp(1.0, 0.1, hairMask);
			speLit *= sLdN;
			speLit = clamp(speLit, 0.0, 25.0);
			half speLit2 = D_GGX(saturate(max(0.463*brdfData.perceptualRoughness + 0.432, brdfData.perceptualRoughness * 1.2)), HdN, LdH2);
			half rScl = 1.0 - brdfData.perceptualRoughness;
			rScl = 1.0 - rScl * rScl;
			DiffuseScatter = (DiffuseScatter * scatter + (speLit2 * t2Scl * rScl * (scatter*0.5 + 0.5) * 0.2));

			half3 difLit = lerp(sLdN, DiffuseScatter, hairMask) * brdfData.diffuse;

			half3 color = difLit + speLit * brdfData.specular;
			color *= occlusion;
			color *= light.color * atten;
			return color;
		}

		half3 AdvEnvironmentBRDF(hairBRDFData brdfData, half3 indirectDiffuse, half3 indirectSpecular, half fresnelTerm)
		{
			half3 c = indirectDiffuse * brdfData.diffuse;
			float surfaceReduction = 1.0 / (brdfData.roughness2 + 1.0);
			c += surfaceReduction * indirectSpecular * lerp(brdfData.specular, brdfData.grazingTerm, fresnelTerm);
			return c;
		}

		half3 GlobalIlluminationHair(hairBRDFData brdfData, half3 bakedGI, half hairMask, half occlusion, half3 positionWS, half3 normalWS, half3 tangentWS, half3 viewDirectionWS)
		{
			half3 fixNrm = normalize(lerp(normalWS, tangentWS, hairMask));
			half3 reflectVector = reflect(-viewDirectionWS, fixNrm);
			reflectVector = normalize(lerp(reflectVector, -reflectVector, hairMask));
			half fresnelTerm = Pow4(1.0 - saturate(dot(normalWS, viewDirectionWS)));

			half3 indirectDiffuse = bakedGI * occlusion;
			half3 indirectSpecular = GlossyEnvironmentReflection(reflectVector, brdfData.perceptualRoughness, occlusion);

			return AdvEnvironmentBRDF(brdfData, indirectDiffuse, indirectSpecular, fresnelTerm);
		}

		half4 UniversalHairLit(HairInputData inputData, half3 albedo, half metallic, half specular, half smoothness, half hairMask, half occlusion, half3 emission, half alpha)
		{
			hairBRDFData brdfData;
			smoothness = smoothness *0.825 + 0.15;
			InitializeHairBRDFData(albedo, metallic, specular, smoothness, alpha, brdfData);

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

			half3 color = GlobalIlluminationHair(brdfData, inputData.bakedGI, hairMask, occlusion, inputData.positionWS, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS);
			color += LightingHair(brdfData, mainLight, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS, hairMask, occlusion);

			#ifdef _ADDITIONAL_LIGHTS
				uint pixelLightCount = q();
				for (uint lightIndex = 0u; lightIndex < pixelLightCount; ++lightIndex)
				{
					Light light = GetAdditionalLight(lightIndex, inputData.positionWS, shadowMask);
					color += LightingHair(brdfData, light, inputData.normalWS, inputData.tangentWS, inputData.viewDirectionWS, hairMask, occlusion);
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
			Blend SrcAlpha OneMinusSrcAlpha
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
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			sampler2D _MainTex;
			sampler2D _BumpMap;
			sampler2D _FlowMap;
			sampler2D _PhysicMap;
			CBUFFER_START( UnityPerMaterial )
			float _JitterTiling;
			float _JitterScale;
			float _JitterOffset;
			float _UsePhysicMap;
			float _Scatter;
			float _Smoothness;
			CBUFFER_END


			real3 ASESafeNormalize(float3 inVec)
			{
				real dp3 = max(FLT_MIN, dot(inVec, inVec));
				return inVec* rsqrt( dp3);
			}
			
			
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord7.xy = v.texcoord0.xy;
				o.ase_texcoord7.zw = v.texcoord1.xy;
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

				float2 texCoord7 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv9 = texCoord7;
				float4 tex2DNode4 = tex2D( _MainTex, uv9 );
				float3 albedo14 = (tex2DNode4).rgb;
				
				float3 normal22 = UnpackNormalScale( tex2D( _BumpMap, uv9 ), 1.0f );
				
				float3 appendResult47 = (float3((tex2D( _FlowMap, uv9 )).rg , 0.0));
				float3 temp_cast_0 = (0.5).xxx;
				float temp_output_64_0 = ( ( 2.0 * ( tex2D( _FlowMap, ( uv9 * _JitterTiling ) ).b - 0.5 ) * _JitterScale * ( max( ( _JitterOffset * -3.0 ) , 0.0 ) + 1.0 ) ) + _JitterOffset );
				float3 normalizeResult70 = ASESafeNormalize( ( ( ( ( appendResult47 - temp_cast_0 ) * 2.0 ) * ( 1.0 - temp_output_64_0 ) ) + ( temp_output_64_0 * normal22 ) ) );
				float3 tangent71 = normalizeResult70;
				
				float3 appendResult41 = (float3(_Scatter , _Smoothness , 1.0));
				float2 texCoord80 = IN.ase_texcoord7.zw * float2( 1,1 ) + float2( 0,0 );
				float2 uv279 = texCoord80;
				float3 appendResult83 = (float3((tex2D( _PhysicMap, uv9 )).rg , tex2D( _PhysicMap, uv279 ).b));
				float3 break42 = (( _UsePhysicMap )?( appendResult83 ):( appendResult41 ));
				float metallic17 = break42.x;
				
				float smoothness19 = break42.y;
				
				float occlusion21 = break42.z;
				
				float alpha16 = tex2DNode4.a;
				
				float3 Albedo = albedo14;
				float3 Normal = normal22;
				float3 Tangent = tangent71;
				float3 Emission = 0;
				float Metallic = metallic17;
				float Smoothness = smoothness19;
				float Specular = 0.5;
				float HairMask = 1.0;
				float Occlusion = occlusion21;
				float Alpha = alpha16;
				float AlphaClipThreshold = 0.5;
				
				#if defined(_ALPHATEST_ON)
				clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				HairInputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				inputData.normalWS = normalize(TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal )));
				inputData.tangentWS = normalize(TransformTangentToWorld(Tangent, half3x3( WorldTangent, WorldBiTangent, WorldNormal )));

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

				half4 color = UniversalHairLit(
					inputData,
					Albedo,
					Metallic,
					Specular,
					Smoothness,
					HairMask,
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
			
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
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
			CBUFFER_START( UnityPerMaterial )
			float _JitterTiling;
			float _JitterScale;
			float _JitterOffset;
			float _UsePhysicMap;
			float _Scatter;
			float _Smoothness;
			CBUFFER_END

			
			
			float3 _LightDirection;

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
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

				float2 texCoord7 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv9 = texCoord7;
				float4 tex2DNode4 = tex2D( _MainTex, uv9 );
				float alpha16 = tex2DNode4.a;
				
				#ifdef _ALPHATEST_ON
				float Alpha = alpha16;
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

			#pragma shader_feature_local _ALPHATEST_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
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
			CBUFFER_START( UnityPerMaterial )
			float _JitterTiling;
			float _JitterScale;
			float _JitterOffset;
			float _UsePhysicMap;
			float _Scatter;
			float _Smoothness;
			CBUFFER_END


			
			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
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

				float2 texCoord7 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv9 = texCoord7;
				float4 tex2DNode4 = tex2D( _MainTex, uv9 );
				float alpha16 = tex2DNode4.a;
				

				#ifdef _ALPHATEST_ON
				float Alpha = alpha16;
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
			CBUFFER_START( UnityPerMaterial )
			float _JitterTiling;
			float _JitterScale;
			float _JitterOffset;
			float _UsePhysicMap;
			float _Scatter;
			float _Smoothness;
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

				float2 texCoord7 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float2 uv9 = texCoord7;
				float4 tex2DNode4 = tex2D( _MainTex, uv9 );
				float3 albedo14 = (tex2DNode4).rgb;
				
				float alpha16 = tex2DNode4.a;
				
				
				float3 Albedo = albedo14;
				float3 Emission = 0;
				float Alpha = alpha16;
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
271;19;1532;936;2660.78;1503.14;1.504844;True;False
Node;AmplifyShaderEditor.CommentaryNode;75;-2424.101,-1345.079;Inherit;False;536;431;Comment;4;9;7;79;80;;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;7;-2374.101,-1290.079;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;76;-1700.203,-762.1342;Inherit;False;1019.435;280;Comment;5;10;4;13;16;14;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;9;-2147.101,-1295.079;Inherit;False;uv;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;10;-1650.203,-687.0504;Inherit;False;9;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SamplerNode;4;-1455.16,-712.1342;Inherit;True;Property;_MainTex;Base;1;1;[NoScaleOffset];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ComponentMaskNode;13;-1131.769,-711.9226;Inherit;False;True;True;True;False;1;0;COLOR;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;73;-2516.094,-449.4082;Inherit;False;2182.625;747.6924;Comment;27;48;47;46;58;49;51;50;43;52;54;55;56;57;62;44;61;59;64;66;65;68;67;69;70;71;60;63;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;16;-1126.769,-615.9225;Inherit;False;alpha;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;14;-904.769,-710.9226;Inherit;False;albedo;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;77;-2533.979,-766.6871;Inherit;False;803.1895;280;Comment;3;12;6;22;;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;74;-1719.212,-1773.069;Inherit;False;1487.906;831.0665;Comment;15;37;41;21;35;42;38;17;19;11;5;34;40;78;82;83;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;59;-1871.094,38.42741;Inherit;False;Property;_JitterScale;Jitter Scale;9;0;Create;True;0;0;0;False;0;False;1;0;0;2;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;55;-2290.094,-132.8138;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;56;-2454.094,-63.81381;Inherit;False;Property;_JitterTiling;Jitter Tiling;8;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;40;-1313.93,-1553.069;Inherit;False;Constant;_Float0;Float 0;7;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;48;-1357.807,-393.6747;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMaxOpNode;62;-1870.836,163.2841;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;17;-469.6101,-1553.772;Inherit;False;metallic;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;69;-868.6461,-143.5288;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;66;-1032.817,-260.4257;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;47;-1533.938,-394.4096;Inherit;False;FLOAT3;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;35;-1212.93,-1445.069;Inherit;False;True;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;58;-1540.094,-107.8138;Inherit;False;4;4;0;FLOAT;2;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;67;-1247.646,53.47124;Inherit;False;22;normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DynamicAppendNode;83;-1015.266,-1347.083;Inherit;False;FLOAT3;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.OneMinusNode;65;-1205.835,-172.7159;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;60;-2376.959,90.26595;Inherit;False;Property;_JitterOffset;Jitter Offset;10;0;Create;True;0;0;0;False;0;False;0;0;-1;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;49;-1550.844,-292.6559;Inherit;False;Constant;_Float1;Float 1;8;0;Create;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;54;-2466.094,-138.8138;Inherit;False;9;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;71;-557.4681,-148.7741;Inherit;False;tangent;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;52;-2110.631,-161.7616;Inherit;True;Property;_FlowMap1;FlowMap(RG:Flow, B:Jitter);7;1;[SingleLineTexture];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Instance;43;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;63;-1724.836,161.2841;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;44;-2255.926,-375.4087;Inherit;False;9;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;64;-1362.836,-60.71585;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.NormalizeNode;70;-728.6461,-143.5288;Inherit;False;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;6;-2285.073,-716.6871;Inherit;True;Property;_BumpMap;Normal Map;2;1;[NoScaleOffset];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;43;-2072.229,-398.9339;Inherit;True;Property;_FlowMap;FlowMap(RG:Flow, B:Jitter);7;1;[NoScaleOffset];Create;False;0;0;0;False;2;Space(15);Header(Hair Info);False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;68;-1043.646,-53.52877;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;5;-1515.47,-1448.765;Inherit;True;Property;_PhysicMap;PhysicMap(MSO;4;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ToggleSwitchNode;34;-885.9301,-1523.069;Inherit;False;Property;_UsePhysicMap;UsePhysicMap;3;0;Create;True;0;0;0;False;0;False;0;True;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;27;-161.3392,1.284859;Inherit;False;21;occlusion;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;51;-1345.804,-289.6553;Inherit;False;Constant;_Float2;Float 2;8;0;Create;True;0;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;41;-1096.93,-1598.069;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;21;-470.6101,-1407.772;Inherit;False;occlusion;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;29;74.66084,-470.715;Inherit;False;Property;_ALPHATEST;Alpha Test;0;0;Create;False;0;0;0;True;1;Header(Base);False;0;0;0;True;_ALPHATEST_ON;Toggle;2;Key0;Key1;Create;True;False;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;28;-163.3392,80.28487;Inherit;False;16;alpha;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;23;-160.3392,-480.715;Inherit;False;14;albedo;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ComponentMaskNode;46;-1744.932,-399.4082;Inherit;False;True;True;False;False;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;12;-2483.979,-693.7969;Inherit;False;9;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.GetLocalVarNode;24;-160.3392,-401.715;Inherit;False;22;normal;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;25;-171.3392,-235.7152;Inherit;False;17;metallic;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;50;-1194.776,-394.675;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;26;-172.3392,-159.7151;Inherit;False;19;smoothness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;57;-1726.094,-84.81382;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;38;-1444.93,-1637.069;Inherit;False;Property;_Smoothness;Smoothness;6;0;Create;True;0;0;0;False;0;False;0.5;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;37;-1445.93,-1723.069;Inherit;False;Property;_Scatter;Scatter;5;0;Create;True;0;0;0;False;0;False;0.3;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;82;-1700.266,-1211.083;Inherit;False;79;uv2;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;22;-1954.789,-710.856;Inherit;False;normal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;79;-2143.766,-1107.583;Inherit;False;uv2;-1;True;1;0;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;80;-2371.766,-1103.583;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BreakToComponentsNode;42;-663.9301,-1501.069;Inherit;False;FLOAT3;1;0;FLOAT3;0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.GetLocalVarNode;72;-168.2535,-314.3435;Inherit;False;71;tangent;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;11;-1700.212,-1423.405;Inherit;False;9;uv;1;0;OBJECT;;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;19;-468.6101,-1483.772;Inherit;False;smoothness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;61;-2044.836,153.2841;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;-3;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;78;-1520.266,-1235.083;Inherit;True;Property;_PhysicMap1;PhysicMap(MSO);4;1;[SingleLineTexture];Create;False;0;0;0;False;0;False;-1;None;None;True;0;False;gray;Auto;False;Instance;5;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;31;290.6053,-47.48808;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;f226630bdb2bf214ba6c167fd35cf69d;True;ShadowCaster;0;1;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;32;290.6053,-47.48808;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;f226630bdb2bf214ba6c167fd35cf69d;True;DepthOnly;0;2;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;33;74.98643,-335.9132;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;200;1;New Amplify Shader;f226630bdb2bf214ba6c167fd35cf69d;True;Meta;0;3;Meta;1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;False;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;30;99.98643,-321.9132;Float;False;True;-1;2;;200;12;GE/CharacterHD/StandardHair;f226630bdb2bf214ba6c167fd35cf69d;True;Forward;0;0;Forward;13;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;True;-2;False;False;False;False;False;False;False;False;False;True;True;0;True;-3;255;True;-4;255;True;-5;0;True;-6;0;True;-7;0;True;-8;0;True;-9;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Transparent=RenderType;Queue=AlphaTest=Queue=0;False;False;0;True;True;2;5;False;-1;10;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;8;Alpha Test;0;0;Cast Shadows;1;0;Receive Shadows;1;0;GPU Instancing;0;638302703464871264;LOD CrossFade;0;638302703406159277;DOTS Instancing;0;0;Vertex Position,InvertActionOnDeselection;1;0;Built-in Fog;1;638302703450990125;0;4;True;True;True;True;False;;False;0
WireConnection;9;0;7;0
WireConnection;4;1;10;0
WireConnection;13;0;4;0
WireConnection;16;0;4;4
WireConnection;14;0;13;0
WireConnection;55;0;54;0
WireConnection;55;1;56;0
WireConnection;48;0;47;0
WireConnection;48;1;49;0
WireConnection;62;0;61;0
WireConnection;17;0;42;0
WireConnection;69;0;66;0
WireConnection;69;1;68;0
WireConnection;66;0;50;0
WireConnection;66;1;65;0
WireConnection;47;0;46;0
WireConnection;35;0;5;0
WireConnection;58;1;57;0
WireConnection;58;2;59;0
WireConnection;58;3;63;0
WireConnection;83;0;35;0
WireConnection;83;2;78;3
WireConnection;65;0;64;0
WireConnection;71;0;70;0
WireConnection;52;1;55;0
WireConnection;63;0;62;0
WireConnection;64;0;58;0
WireConnection;64;1;60;0
WireConnection;70;0;69;0
WireConnection;6;1;12;0
WireConnection;43;1;44;0
WireConnection;68;0;64;0
WireConnection;68;1;67;0
WireConnection;5;1;11;0
WireConnection;34;0;41;0
WireConnection;34;1;83;0
WireConnection;41;0;37;0
WireConnection;41;1;38;0
WireConnection;41;2;40;0
WireConnection;21;0;42;2
WireConnection;46;0;43;0
WireConnection;50;0;48;0
WireConnection;50;1;51;0
WireConnection;57;0;52;3
WireConnection;22;0;6;0
WireConnection;79;0;80;0
WireConnection;42;0;34;0
WireConnection;19;0;42;1
WireConnection;61;0;60;0
WireConnection;78;1;82;0
WireConnection;30;0;23;0
WireConnection;30;1;24;0
WireConnection;30;11;72;0
WireConnection;30;3;25;0
WireConnection;30;4;26;0
WireConnection;30;5;27;0
WireConnection;30;6;28;0
ASEEND*/
//CHKSM=012EE4ACC8337126D9D0ACADF571E306959B4EFD