using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class EditGlobalSettings : EditorWindow
{
	[MenuItem("美術工具/全域設置")]
	public static void OpenGlobalSettingsEditor()
	{
		EditGlobalSettings win = (EditGlobalSettings)GetWindow(typeof(EditGlobalSettings), false, "全域設置");
		win.minSize = new Vector2(400, 400);
		win.maxSize = new Vector2(400, 400);
	}

	private void OnGUI()
	{
		if(GUILayout.Button("Set Sprite Color", GUILayout.Height(30)))
		{
			Shader.SetGlobalColor("_SpriteColor", Color.white);
		}
	}
}
