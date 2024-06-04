Shader "GE/Scene/Basic_PP-HpTree"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)]_Cull("Cull", Float) = 2
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[Header(Main Properties)]_Color("Color", Color) = (1,1,1,1)
		_MainTex("Base", 2D) = "white" {}
		[Toggle][Header(Base)]_ZWrite("ZWrite", Float) = 1
		[Toggle(_ALPHATEST_ON)] _ALPHATEST("Alpha Test", Float) = 0
		_AlphaCutoff("Alpha Clip Threshold", Range( 0 , 1)) = 0.5
		_BumpMap("Normal Map", 2D) = "bump" {}
		_NormalScale("Normal Scale", Range( 0 , 2)) = 1
		
		[Space(10)][Header(Pivot Paint)]
		_WindSpeedMultiplier("Wind Speed Multiplier", Float) = 1.0
		_TreeAnimationSines("Tree Animation Sines", Vector) = (10.7457, 9.33, 18.15, 46.24)
		[Space(10)]
		_BranchRadiusMotionFalloff("Branch Radius Motion Falloff", Float) = 21.0
		_BranchSineRateAlongLength("Branch Sine Rate Along Length", Float) = 2.0
		_BranchBendStrength("Branch Bend Strength", Float) = 0.03
		_BranchAnimationStrength("Branch Animation Strength", Float) = 3.0
		[Toggle]_sep0("========================",Float) = 0
		_LeafSineRateAlongLength("Leaf Sine Rate Along Length", Float) = 0
		_LeafAnimationStrength("Leaf Animation Strength", Float) = 0.01
		_LeafDirectSw("Leaves animation dictated by branches direction (0) or leaves direction (1)", Range(0,1)) = 1.0

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

		float4 _FixTime;
		float4 _WindVector;

		float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
		{
			original -= center;
			float C = cos( angle );
			float S = sin( angle );
			float t = 1 - C;
			float m00 = t * u.x * u.x + C;
			float m01 = t * u.x * u.y - S * u.z;
			float m02 = t * u.x * u.z + S * u.y;
			float m10 = t * u.x * u.y + S * u.z;
			float m11 = t * u.y * u.y + C;
			float m12 = t * u.y * u.z - S * u.x;
			float m20 = t * u.x * u.z - S * u.y;
			float m21 = t * u.y * u.z + S * u.x;
			float m22 = t * u.z * u.z + C;
			float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
			return mul( finalMatrix, original ) - original;
		}

		float3 DeriveNormalZ(float2 inXY)
		{
			float z = sqrt(max(1.0 - dot(inXY, inXY), 0.000001));
			return float3(inXY, z);
		}

		float LinearSine(float value, float period){
			float a = frac((0.25 * period + value) / period);
			float a1 = a*2.0;
			a = lerp(a1, 2.0 - a1, floor(a1));
			// float b = (3.0 - 2.0*a)*a*a;
			return a;
		}

		float LinearSineRounded(float value, float period){
			float a = frac((0.25 * period + value) / period);
			float a1 = a*2.0;
			a = lerp(a1, 2.0 - a1, floor(a1));
			a = (3.0 - 2.0*a)*a*a;
			return a;
		}

		float TreeAnimationSines(float timeAndGrad, float4 LS){
			float ls1 = clamp(LinearSine(timeAndGrad, LS.x),  0.0, 0.5);
			float ls23 = LinearSineRounded(timeAndGrad, LS.y) * LinearSineRounded(timeAndGrad, LS.z);
			float ls4 = clamp(LinearSine(timeAndGrad, LS.w), -0.5, 0.5);
			return ls1 + ls23 + ls4;
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
		//endregion HLSLINCLUDE
		
		Pass
		{
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			Blend Off
			ZWrite [_ZWrite]
			ZTest LEqual
			Offset [_OffsetFactor],[_OffsetUnit]
			ColorMask RGBA

			HLSLPROGRAM
			#pragma multi_compile_instancing
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ FOG_LINEAR
			#pragma multi_compile _ _COVERSNOW
			#pragma multi_compile _ _SNOWBRUSHMASK
			#pragma multi_compile _ LowQuality

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
				float4 color     : COLOR;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				float4 texcoord3  : TEXCOORD3;
				float4 texcoord4  : TEXCOORD4;
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
			sampler2D _BumpMap;
			#if _SNOWBRUSHMASK
			sampler2D SnowMaskTex;
			float4 maskRect;
			#endif
			CBUFFER_START( UnityPerMaterial )
			float4 _Color;
			float4 _MainTex_ST;
			float _ZWrite;
			float _NormalScale;
			float _AlphaCutoff;
			float _WindSpeedMultiplier;
			float _BranchRadiusMotionFalloff;
			float _BranchSineRateAlongLength;
			float _BranchBendStrength;
			float _BranchAnimationStrength;
			float _LeafSineRateAlongLength;
			float _LeafAnimationStrength;
			float _LeafDirectSw;
			float4 _TreeAnimationSines;
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

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#ifndef LowQuality
				float windSpeed = _WindVector.a * _FixTime.y * _WindSpeedMultiplier;
				float3 windVec = normalize(_WindVector.xyz + float3(0.00001, 0.0, 0.00001));
				float windStrength = length(_WindVector.xyz);

				float3 branchPos = (v.color.rgb*float3(2,1,2)-float3(1,0,1)) * 50.0;
				branchPos = TransformObjectToWorld(branchPos);
				float3 branchXAxis = float3(v.texcoord1.xy, v.texcoord3.y);
				branchXAxis = TransformObjectToWorldDir(branchXAxis);
				float3 branchWindRotAxis = normalize(cross(branchXAxis, windVec));
				float branchFlowGrad = dot(branchPos, windVec);
				
				float3 leafPivPos = float3(v.texcoord2.xy, v.texcoord3.x);
				leafPivPos = TransformObjectToWorld(leafPivPos);
				float3 leafXAxis = normalize(frac(leafPivPos)*2.0-1.0);
				leafXAxis = TransformObjectToWorldDir(leafXAxis);
				float3 leafRotAxis = normalize(cross(leafXAxis, windVec));
				float leafFlowGrad = dot(leafPivPos, windVec);

				float leafMask = abs(v.texcoord3.x) > 0 ? 1.0 : 0.0;
				float movementMask = dot(v.color, float4(1,1,1,1));

				float branchDistMask = (distance(branchPos, positionWS) / _BranchRadiusMotionFalloff);
				branchDistMask *= branchDistMask;
				branchDistMask = saturate(branchDistMask);
				float branchTimeAndGrad = branchDistMask * _BranchSineRateAlongLength + windSpeed;
				branchTimeAndGrad += branchFlowGrad;
				float treeAnm = TreeAnimationSines(branchTimeAndGrad, _TreeAnimationSines);
				float wdb = dot(windVec, branchXAxis)*0.5+0.5;
				float wd1 = 1.0-saturate(0.5*wdb);
				float branchRotAngle = 2.0*wdb+treeAnm;
				branchRotAngle = branchRotAngle*windStrength*_BranchAnimationStrength*branchDistMask*_BranchBendStrength*wd1;
				float3 branchPosOffset = RotateAroundAxis(branchPos, positionWS, branchWindRotAxis, branchRotAngle);

				float leafDist = distance(leafPivPos, positionWS);
				float leafDistMask = leafDist * _LeafSineRateAlongLength + windSpeed;
				leafDistMask += leafFlowGrad;
				float leafAnm = LinearSineRounded(leafDistMask, 1.0);
				leafAnm += treeAnm;
				float wdL = dot(windVec, leafXAxis)*0.5+0.5;
				float wd2 = 1.0-saturate(0.5*wdL);
				float wdS = lerp(wd1, wd2, _LeafDirectSw);
				float leafRotAngle = leafAnm + wdL;
				leafRotAngle = leafRotAngle * windStrength * _LeafAnimationStrength * leafDist * wdS;
				float3 leafPosOffset = RotateAroundAxis(leafPivPos, positionWS, leafRotAxis, leafRotAngle) * leafMask;

				positionWS += movementMask > 0 ? branchPosOffset+leafPosOffset: float3(0,0,0);
				#endif

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
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION) || defined(ASE_NEEDS_FRAG_SCREEN_POSITION_NORMALIZED)
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
				float2 uv21 = uv_MainTex;
				float4 temp_output_7_0 = ( _Color * tex2D( _MainTex, uv21 ) );
				float3 albedo14 = (temp_output_7_0).rgb;
				
				float3 unpack12 = UnpackNormalScale( tex2D( _BumpMap, uv21 ), _NormalScale );
				unpack12.z = lerp( 1, unpack12.z, saturate(_NormalScale) );
				float3 normal16 = unpack12;
				
				float alpha15 = (temp_output_7_0).a;
				
				float3 Albedo = albedo14;
				float3 Normal = normal16;
				float3 Emission = 0;
				float Occlusion = 1;
				float Alpha = alpha15;
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

				#ifdef _COVERSNOW
				//snow
				float snowProc = 1.0-_SnowControl;
				float snowTile = 0.001 * _SnowTiling;
				float detailMask = 1-saturate(abs(snowProc-0.5)*10-4);

				float smoothRange = lerp(1.0, 16.0, saturate(_SnowControl*4)); //16.0;
				float srScl = smoothRange / (smoothRange - 1.0);
				float srBias = 2.0 / smoothRange;

				float topCoverMask = (inputData.normalWS.y - (1.0 - _CoverSnowRange)) / (0.333 * _CoverSnowRange);
				topCoverMask = saturate(topCoverMask * _CoverSnowThickness);

				float2 sideUV_0 = WorldPosition.xy * snowTile;
				float2 sideUV_1 = WorldPosition.zy * snowTile;
				float sideMask = saturate(abs(inputData.normalWS.z) * 2 - 0.5);
				float frostMask = lerp(tex2D(_SnowInfo, sideUV_1).b, tex2D(_SnowInfo, sideUV_0).b, sideMask) * saturate(1 - 1.5*abs(inputData.normalWS.y));

				float2 snowMaskUV = (WorldPosition.xz - _SnowRect.xy) / _SnowRect.zw;
				float2 snowUV = WorldPosition.xz * snowTile;
				float2 snowUV2 = snowUV * 0.25;
				float4 snowInfo = tex2D(_SnowInfo, snowUV);
				float4 snowInfo2 = tex2D(_SnowInfo, snowUV2);
				float4 snowArea = UNITY_SAMPLE_TEX2DARRAY(_SnowAreaArray, float3(snowMaskUV, _AreaIndex));
				float snowMask = snowArea.g;
				float3 snowColor = lerp(float3(0.122139,0.245342,0.456411) ,float3(0.896269,0.896269,0.896269),snowInfo.x*snowInfo2.x);
				float coverMask = 1-0.75*saturate((snowMask - snowProc)*16);
				
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
				Albedo = lerp(lerp(desatAlbedo * 0.75, Albedo, coverMask), snowColor, snowMask * (topCoverMask + frostMask));
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

				half4 color = UniversalLambert(
					inputData, 
					Albedo, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef ASE_FOG
					color.rgb = MixFog(color.rgb, inputData.fogCoord);
				#endif
				// color.rgb  = IN.color.rgb;
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			#pragma shader_feature_local _ALPHATEST_ON
			#pragma multi_compile _ LowQuality

			struct VertexInput
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 color     : COLOR;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				float4 texcoord3  : TEXCOORD3;
				float4 texcoord4  : TEXCOORD4;
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
			float4 _Color;
			float4 _MainTex_ST;
			float _ZWrite;
			float _NormalScale;
			float _AlphaCutoff;
			float _WindSpeedMultiplier;
			float _BranchRadiusMotionFalloff;
			float _BranchSineRateAlongLength;
			float _BranchBendStrength;
			float _BranchAnimationStrength;
			float _LeafSineRateAlongLength;
			float _LeafAnimationStrength;
			float _LeafDirectSw;
			float4 _TreeAnimationSines;
			CBUFFER_END

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

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#ifndef LowQuality
				float windSpeed = _WindVector.a * _FixTime.y * _WindSpeedMultiplier;
				float3 windVec = normalize(_WindVector.xyz + float3(0.00001, 0.0, 0.00001));
				float windStrength = length(_WindVector.xyz);

				float3 branchPos = (v.color.rgb*float3(2,1,2)-float3(1,0,1)) * 50.0;
				branchPos = TransformObjectToWorld(branchPos);
				float3 branchXAxis = float3(v.texcoord1.xy, v.texcoord3.y);
				branchXAxis = TransformObjectToWorldDir(branchXAxis);
				float3 branchWindRotAxis = normalize(cross(branchXAxis, windVec));
				float branchFlowGrad = dot(branchPos, windVec);
				
				float3 leafPivPos = float3(v.texcoord2.xy, v.texcoord3.x);
				leafPivPos = TransformObjectToWorld(leafPivPos);
				float3 leafXAxis = normalize(frac(leafPivPos)*2.0-1.0);
				leafXAxis = TransformObjectToWorldDir(leafXAxis);
				float3 leafRotAxis = normalize(cross(leafXAxis, windVec));
				float leafFlowGrad = dot(leafPivPos, windVec);

				float leafMask = abs(v.texcoord3.x) > 0 ? 1.0 : 0.0;
				float movementMask = dot(v.color, float4(1,1,1,1));

				float branchDistMask = (distance(branchPos, positionWS) / _BranchRadiusMotionFalloff);
				branchDistMask *= branchDistMask;
				branchDistMask = saturate(branchDistMask);
				float branchTimeAndGrad = branchDistMask * _BranchSineRateAlongLength + windSpeed;
				branchTimeAndGrad += branchFlowGrad;
				float treeAnm = TreeAnimationSines(branchTimeAndGrad, _TreeAnimationSines);
				float wdb = dot(windVec, branchXAxis)*0.5+0.5;
				float wd1 = 1.0-saturate(0.5*wdb);
				float branchRotAngle = 2.0*wdb+treeAnm;
				branchRotAngle = branchRotAngle*windStrength*_BranchAnimationStrength*branchDistMask*_BranchBendStrength*wd1;
				float3 branchPosOffset = RotateAroundAxis(branchPos, positionWS, branchWindRotAxis, branchRotAngle);

				float leafDist = distance(leafPivPos, positionWS);
				float leafDistMask = leafDist * _LeafSineRateAlongLength + windSpeed;
				leafDistMask += leafFlowGrad;
				float leafAnm = LinearSineRounded(leafDistMask, 1.0);
				leafAnm += treeAnm;
				float wdL = dot(windVec, leafXAxis)*0.5+0.5;
				float wd2 = 1.0-saturate(0.5*wdL);
				float wdS = lerp(wd1, wd2, _LeafDirectSw);
				float leafRotAngle = leafAnm + wdL;
				leafRotAngle = leafRotAngle * windStrength * _LeafAnimationStrength * leafDist * wdS;
				float3 leafPosOffset = RotateAroundAxis(leafPivPos, positionWS, leafRotAxis, leafRotAngle) * leafMask;

				positionWS += movementMask > 0 ? branchPosOffset+leafPosOffset: float3(0,0,0);
				#endif

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

				float2 uv_MainTex = IN.ase_texcoord2.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float2 uv21 = uv_MainTex;
				float4 temp_output_7_0 = ( _Color * tex2D( _MainTex, uv21 ) );
				float alpha15 = (temp_output_7_0).a;
				

				#ifdef _ALPHATEST_ON
				float Alpha = alpha15;
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#pragma shader_feature_local _ALPHATEST_ON
			#pragma multi_compile _ LowQuality

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				float4 color     : COLOR;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				float4 texcoord3  : TEXCOORD3;
				float4 texcoord4  : TEXCOORD4;
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
			float4 _Color;
			float4 _MainTex_ST;
			float _ZWrite;
			float _NormalScale;
			float _AlphaCutoff;
			float _WindSpeedMultiplier;
			float _BranchRadiusMotionFalloff;
			float _BranchSineRateAlongLength;
			float _BranchBendStrength;
			float _BranchAnimationStrength;
			float _LeafSineRateAlongLength;
			float _LeafAnimationStrength;
			float _LeafDirectSw;
			float4 _TreeAnimationSines;
			CBUFFER_END

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.texcoord0.xy;
				o.ase_texcoord2.zw = 0;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#ifndef LowQuality
				float windSpeed = _WindVector.a * _FixTime.y * _WindSpeedMultiplier;
				float3 windVec = normalize(_WindVector.xyz + float3(0.00001, 0.0, 0.00001));
				float windStrength = length(_WindVector.xyz);

				float3 branchPos = (v.color.rgb*float3(2,1,2)-float3(1,0,1)) * 50.0;
				branchPos = TransformObjectToWorld(branchPos);
				float3 branchXAxis = float3(v.texcoord1.xy, v.texcoord3.y);
				branchXAxis = TransformObjectToWorldDir(branchXAxis);
				float3 branchWindRotAxis = normalize(cross(branchXAxis, windVec));
				float branchFlowGrad = dot(branchPos, windVec);
				
				float3 leafPivPos = float3(v.texcoord2.xy, v.texcoord3.x);
				leafPivPos = TransformObjectToWorld(leafPivPos);
				float3 leafXAxis = normalize(frac(leafPivPos)*2.0-1.0);
				leafXAxis = TransformObjectToWorldDir(leafXAxis);
				float3 leafRotAxis = normalize(cross(leafXAxis, windVec));
				float leafFlowGrad = dot(leafPivPos, windVec);

				float leafMask = abs(v.texcoord3.x) > 0 ? 1.0 : 0.0;
				float movementMask = dot(v.color, float4(1,1,1,1));

				float branchDistMask = (distance(branchPos, positionWS) / _BranchRadiusMotionFalloff);
				branchDistMask *= branchDistMask;
				branchDistMask = saturate(branchDistMask);
				float branchTimeAndGrad = branchDistMask * _BranchSineRateAlongLength + windSpeed;
				branchTimeAndGrad += branchFlowGrad;
				float treeAnm = TreeAnimationSines(branchTimeAndGrad, _TreeAnimationSines);
				float wdb = dot(windVec, branchXAxis)*0.5+0.5;
				float wd1 = 1.0-saturate(0.5*wdb);
				float branchRotAngle = 2.0*wdb+treeAnm;
				branchRotAngle = branchRotAngle*windStrength*_BranchAnimationStrength*branchDistMask*_BranchBendStrength*wd1;
				float3 branchPosOffset = RotateAroundAxis(branchPos, positionWS, branchWindRotAxis, branchRotAngle);

				float leafDist = distance(leafPivPos, positionWS);
				float leafDistMask = leafDist * _LeafSineRateAlongLength + windSpeed;
				leafDistMask += leafFlowGrad;
				float leafAnm = LinearSineRounded(leafDistMask, 1.0);
				leafAnm += treeAnm;
				float wdL = dot(windVec, leafXAxis)*0.5+0.5;
				float wd2 = 1.0-saturate(0.5*wdL);
				float wdS = lerp(wd1, wd2, _LeafDirectSw);
				float leafRotAngle = leafAnm + wdL;
				leafRotAngle = leafRotAngle * windStrength * _LeafAnimationStrength * leafDist * wdS;
				float3 leafPosOffset = RotateAroundAxis(leafPivPos, positionWS, leafRotAxis, leafRotAngle) * leafMask;

				positionWS += movementMask > 0 ? branchPosOffset+leafPosOffset: float3(0,0,0);
				#endif

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

				float2 uv_MainTex = IN.ase_texcoord2.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float2 uv21 = uv_MainTex;
				float4 temp_output_7_0 = ( _Color * tex2D( _MainTex, uv21 ) );
				float alpha15 = (temp_output_7_0).a;
				

				#ifdef _ALPHATEST_ON
				float Alpha = alpha15;
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
			#define ASE_FOG 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"

			#pragma shader_feature_local _ALPHATEST_ON
			#pragma multi_compile _ LowQuality

			struct VertexInput
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				float4 tangent   : TANGENT;
				float4 color     : COLOR;
				float4 texcoord0  : TEXCOORD0;
				float4 texcoord1  : TEXCOORD1;
				float4 texcoord2  : TEXCOORD2;
				float4 texcoord3  : TEXCOORD3;
				float4 texcoord4  : TEXCOORD4;
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
			float4 _Color;
			float4 _MainTex_ST;
			float _ZWrite;
			float _NormalScale;
			float _AlphaCutoff;
			float _WindSpeedMultiplier;
			float _BranchRadiusMotionFalloff;
			float _BranchSineRateAlongLength;
			float _BranchBendStrength;
			float _BranchAnimationStrength;
			float _LeafSineRateAlongLength;
			float _LeafAnimationStrength;
			float _LeafDirectSw;
			float4 _TreeAnimationSines;
			CBUFFER_END

			VertexOutput vert( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.ase_texcoord2.xy = v.texcoord0.xy;
				o.ase_texcoord2.zw = 0;
				
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );

				#ifndef LowQuality
				float windSpeed = _WindVector.a * _FixTime.y * _WindSpeedMultiplier;
				float3 windVec = normalize(_WindVector.xyz + float3(0.00001, 0.0, 0.00001));
				float windStrength = length(_WindVector.xyz);

				float3 branchPos = (v.color.rgb*float3(2,1,2)-float3(1,0,1)) * 50.0;
				branchPos = TransformObjectToWorld(branchPos);
				float3 branchXAxis = float3(v.texcoord1.xy, v.texcoord3.y);
				branchXAxis = TransformObjectToWorldDir(branchXAxis);
				float3 branchWindRotAxis = normalize(cross(branchXAxis, windVec));
				float branchFlowGrad = dot(branchPos, windVec);
				
				float3 leafPivPos = float3(v.texcoord2.xy, v.texcoord3.x);
				leafPivPos = TransformObjectToWorld(leafPivPos);
				float3 leafXAxis = normalize(frac(leafPivPos)*2.0-1.0);
				leafXAxis = TransformObjectToWorldDir(leafXAxis);
				float3 leafRotAxis = normalize(cross(leafXAxis, windVec));
				float leafFlowGrad = dot(leafPivPos, windVec);

				float leafMask = abs(v.texcoord3.x) > 0 ? 1.0 : 0.0;
				float movementMask = dot(v.color, float4(1,1,1,1));

				float branchDistMask = (distance(branchPos, positionWS) / _BranchRadiusMotionFalloff);
				branchDistMask *= branchDistMask;
				branchDistMask = saturate(branchDistMask);
				float branchTimeAndGrad = branchDistMask * _BranchSineRateAlongLength + windSpeed;
				branchTimeAndGrad += branchFlowGrad;
				float treeAnm = TreeAnimationSines(branchTimeAndGrad, _TreeAnimationSines);
				float wdb = dot(windVec, branchXAxis)*0.5+0.5;
				float wd1 = 1.0-saturate(0.5*wdb);
				float branchRotAngle = 2.0*wdb+treeAnm;
				branchRotAngle = branchRotAngle*windStrength*_BranchAnimationStrength*branchDistMask*_BranchBendStrength*wd1;
				float3 branchPosOffset = RotateAroundAxis(branchPos, positionWS, branchWindRotAxis, branchRotAngle);

				float leafDist = distance(leafPivPos, positionWS);
				float leafDistMask = leafDist * _LeafSineRateAlongLength + windSpeed;
				leafDistMask += leafFlowGrad;
				float leafAnm = LinearSineRounded(leafDistMask, 1.0);
				leafAnm += treeAnm;
				float wdL = dot(windVec, leafXAxis)*0.5+0.5;
				float wd2 = 1.0-saturate(0.5*wdL);
				float wdS = lerp(wd1, wd2, _LeafDirectSw);
				float leafRotAngle = leafAnm + wdL;
				leafRotAngle = leafRotAngle * windStrength * _LeafAnimationStrength * leafDist * wdS;
				float3 leafPosOffset = RotateAroundAxis(leafPivPos, positionWS, leafRotAxis, leafRotAngle) * leafMask;

				positionWS += movementMask > 0 ? branchPosOffset+leafPosOffset: float3(0,0,0);
				#endif

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
				float2 uv21 = uv_MainTex;
				float4 temp_output_7_0 = ( _Color * tex2D( _MainTex, uv21 ) );
				float3 albedo14 = (temp_output_7_0).rgb;
				
				float alpha15 = (temp_output_7_0).a;
				
				
				float3 Albedo = albedo14;
				float3 Emission = 0;
				float Alpha = alpha15;
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