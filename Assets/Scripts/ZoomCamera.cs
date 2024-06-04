using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ZoomCamera : MonoBehaviour
{
	public float ZoomSpeed = 0.5f;
	// Start is called before the first frame update
	void Start()
	{
		
	}

	// Update is called once per frame
	void Update()
	{
		
	}

	public void ZoomIn(){
		float dt = Time.deltaTime;
		transform.position += (ZoomSpeed * transform.forward);
	}

	public void ZoomOut()
	{
		float dt = Time.deltaTime;
		transform.position -= (ZoomSpeed * transform.forward);

	}
}
