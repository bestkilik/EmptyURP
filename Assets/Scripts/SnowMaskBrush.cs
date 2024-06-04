using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SnowMaskBrush : MonoBehaviour
{
	public int MaskSize = 512;
	public GameObject brush;
	public bool brushOn = false;

	[Range(0.0f, 1.0f)]
	public float brushSoftRange = 0.5f;

	[Range(0.0f, 1.0f)]
	public float brushOpacity = 1.0f;

	private static Material texProcMat = null;
	private Vector4 texRect = new Vector4(0,0,0,0);
	private RenderTexture rt0;
	private RenderTexture rt_last;

	private Renderer _r;
	private Material _m;

	private static int get2powHigh(int into)
	{
		--into;
		into |= into >> 1;
		into |= into >> 2;
		into |= into >> 4;
		into |= into >> 8;
		into |= into >> 16;
		return ++into;
	}

	public void discardRT(RenderTexture rt)
	{
		if (rt != null)
		{
			RenderTexture prev = RenderTexture.active;
			RenderTexture.active = rt;
			RenderTexture.active.DiscardContents();
			RenderTexture.ReleaseTemporary(rt);
			RenderTexture.active = prev;
		}
	}

	public void clearRT(RenderTexture rt)
	{
		if (rt != null)
		{
			RenderTexture prev = RenderTexture.active;
			RenderTexture.active = rt;
			GL.Clear(true, true, Color.white);
			RenderTexture.active = prev;
		}
	}
	// Start is called before the first frame update
	private void Start()
	{
		int actSize = get2powHigh(MaskSize);
		
		_r = GetComponent<Renderer>();
		if (!_r) return;

		_m = _r.material;
		if (!_m) return;

		rt0     = RenderTexture.GetTemporary(actSize, actSize, 0, RenderTextureFormat.R8, RenderTextureReadWrite.sRGB);
		rt0.useMipMap = true;
		rt_last = RenderTexture.GetTemporary(actSize, actSize, 0, RenderTextureFormat.R8, RenderTextureReadWrite.sRGB);

		clearRT(rt0);
		clearRT(rt_last);

		if (texProcMat == null)
		{
			texProcMat = new Material(Shader.Find("TextureBrush/SnowMaskBrushRender"));
		}
	}

	public void DrawBrush()
	{
		if (!_r || !_m) return;

		Bounds bounds = _r.bounds;
		texRect = new Vector4(bounds.min.x,
							  bounds.min.z,
							  bounds.size.x,
							  bounds.size.z);

		Shader.SetGlobalVector("maskRect", texRect);

		if (brush && brushOn)
		{
			texProcMat.SetFloat("_SoftRange", brushSoftRange);
			texProcMat.SetFloat("_BrushOpacity", brushOpacity);

			Shader.SetGlobalVector("brushPos",
				new Vector4(
					brush.transform.position.x,
					brush.transform.position.y,
					brush.transform.position.z,
					brush.transform.localScale.x
				));

			Graphics.Blit(rt_last, rt0, texProcMat);
			Graphics.Blit(rt0, rt_last);

			_m.SetTexture("_SnowMaskTex", rt0);
		}
	}

	// Update is called once per frame
	void Update()
	{
		DrawBrush();
	}
}
