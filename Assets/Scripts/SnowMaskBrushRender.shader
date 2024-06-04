Shader "TextureBrush/SnowMaskBrushRender"
{
	Properties
	{
		_MainTex ("Texture", 2D) = "white" {}
		_OrigTex ("Original Tex", 2D) = "white" {}
		_SoftRange("Soft Range", Range(0,1)) = 0.2
		_BrushOpacity("Brush Opacity", Range(0,1)) = 1.0
	}
	SubShader
	{
		Tags { "RenderType"="Transparent" "Queue"="Transparent" "IgnoreProjector"="True"}
		LOD 0

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag

			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
				float2 uv     : TEXCOORD0;
			};

			struct v2f
			{
				float4 vertex   : SV_POSITION;
				float4 uv       : TEXCOORD0;
				float3 worldPos : TEXCOORD1;
			};

			sampler2D _MainTex;
			sampler2D _OrigTex;
			float4 _MainTex_ST;
			float4 maskRect;
			float4 brushPos;
			float _SoftRange;
			float _BrushOpacity;
			int brushOn;

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.uv.xy = TRANSFORM_TEX(v.uv, _MainTex);
				o.worldPos = mul(unity_ObjectToWorld, v.vertex).xyz;
				o.uv.zw = (o.worldPos.xz - maskRect.xy) / maskRect.zw;

				return o;
			}

			fixed4 frag (v2f i) : SV_Target
			{
				float2 brushCenter = (brushPos.xz - maskRect.xy) / maskRect.zw;
				float dist = length((i.uv.xy - brushCenter) * maskRect.zw);

				float mask = (dist/brushPos.w);
				mask = ((mask + _SoftRange - 1.0)/_SoftRange);
				mask = 1.0 - smoothstep(0.0,1.0,mask);

				fixed4 col = tex2D(_MainTex, i.uv.xy);
				col.rgb = lerp(col.rgb, fixed3(0,0,0), mask * _BrushOpacity);
				
				return col;
			}
			ENDCG
		}
	}
}
