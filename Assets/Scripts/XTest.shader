Shader "TextureBrush/XTest"
{
	Properties
	{
		_MainTex ("Texture", 2D) = "white" {}
		_SnowMaskTex ("Texture", 2D) = "white" {}
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

			sampler2D _SnowMaskTex;
			sampler2D _MainTex;
			float4 _MainTex_ST;
			float4 maskRect;

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
				
				fixed4 col = 0;//tex2D(SnowMaskTex, i.uv.zw);
				col.rgb += tex2Dlod(_SnowMaskTex, float4(i.uv.zw, 0, 0)).r;
				col.rgb += tex2Dlod(_SnowMaskTex, float4(i.uv.zw, 0, 1)).r;
				col.rgb += tex2Dlod(_SnowMaskTex, float4(i.uv.zw, 0, 2)).r;
				col.rgb += tex2Dlod(_SnowMaskTex, float4(i.uv.zw, 0, 3)).r;
				col.rgb *= 0.25;

				return col;
			}
			ENDCG
		}
	}
}
