using UnityEngine;

[ExecuteInEditMode]
public class ShaderGlobalSettings : MonoBehaviour
{
	private float timer;

	void Start()
	{
		timer = 0.0f;
	}

	// Update is called once per frame
	void Update()
	{
		timer += Time.deltaTime;
		if (timer >= 43200.0f) timer = 0.0f;

		Shader.SetGlobalVector("_FixTime", new Vector4(timer*0.05f, timer, timer*2.0f, timer*3.0f));
	}

	private void OnApplicationFocus(bool focus)
	{
		if (focus) ResetTimer();
	}

	/// <summary>
	/// 重置計時器
	/// </summary>
	public void ResetTimer()
	{
		timer = 0.0f;
	}
}
