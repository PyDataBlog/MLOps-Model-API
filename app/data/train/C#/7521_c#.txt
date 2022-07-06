using UnityEngine;
using System.Collections;

[RequireComponent(typeof(BoxCollider2D))]
public class PlayerPhysics : MonoBehaviour {

	public LayerMask collisionMask;
	public int coliderRaysX = 3;
	public int coliderRaysY = 3;


	private BoxCollider2D collider;
	private Vector3 size;
	private Vector3 center;

	private Vector3 originalSize;
	private Vector3 originalCenter;
	private float colliderScaleX;
	private float colliderScaleY;


	private float skin = .005f;

	//[HideInInspector]
	public bool grounded;
	[HideInInspector]
	public bool movementStop;

	Ray ray;
	RaycastHit hit;

	void Start(){
		collider = GetComponent<BoxCollider2D> ();
		//collider.bounds.extents = new Vector3 (0,0,0);
		colliderScaleX = transform.localScale.x;
		colliderScaleY = transform.localScale.y;
		originalSize = collider.size;
		originalCenter = collider.center;
		Debug.Log(collider.size+" "+collider.center+"/"+originalSize +" "+ originalCenter);
		SetColider (originalSize,originalCenter);
	}

	public void Move(Vector2 moveAmount){

		float deltaY = moveAmount.y;
		float deltaX = moveAmount.x;
		Vector2 p = transform.position;
		grounded = false;
		for  (int i=0; i<coliderRaysY; i++) {
			float dir = Mathf.Sign(deltaY);
			float x =(p.x + center.x - size.x/2) + size.x/(coliderRaysY-1)*i;
			float y = p.y + center.y+size.y/2 * dir;
			
			
			ray = new Ray(new Vector2(x,y),new Vector2(0,dir));
			Debug.DrawRay(ray.origin,ray.direction);
			if(Physics.Raycast(ray, out hit,Mathf.Abs(deltaY)+skin,collisionMask)){
				float dst = Vector3.Distance(ray.origin,hit.point);
				
				if(dst>skin){
					deltaY=dst*dir-skin*dir;
				}
				else{
					deltaY=0;
				}
				grounded = true;
				break;
			}
		}

		movementStop = false;
		for  (int i=0; i<coliderRaysX; i++) {
			float dir = Mathf.Sign(deltaX);
			float x =p.x + center.x + size.x/2*dir;
			float y = p.y + center.y - size.y/2 + size.y/(coliderRaysX-1) * i;
//			Debug.Log(x +" "+y+" "+ p.x +" "+center.x+" "+size.x+" "+size.x/2*dir);
			
			
			ray = new Ray(new Vector2(x,y),new Vector2(dir,0));
			Debug.DrawRay(ray.origin,ray.direction);
			if(Physics.Raycast(ray, out hit,Mathf.Abs(deltaX)+skin,collisionMask)){
				float dst = Vector3.Distance(ray.origin,hit.point);
				
				if(dst>skin){
					deltaX=dst*dir-skin*dir;
				}
				else{
					deltaX=0;
				}
				movementStop = true;
				break;
			}
		}
		if(!grounded && !movementStop){
			Vector3 playerDir = new Vector3 (deltaX, deltaY, 0);
			Vector3 o = new Vector3 (p.x + center.x + size.x/coliderRaysX*Mathf.Sign(deltaX), p.y + center.y+size.y/coliderRaysY * Mathf.Sign(deltaY),0);
			Debug.DrawRay(o,playerDir.normalized);
			ray = new Ray (o, playerDir.normalized);
			if(Physics.Raycast(ray,Mathf.Sqrt (deltaX*deltaX + deltaY*deltaY),collisionMask)){
				grounded = true;
				deltaY = 0;
			}
		}

		Vector2 finalTransform = new Vector2 (deltaX, deltaY);

		transform.Translate (finalTransform);
	}

	public void SetColider(Vector3 size, Vector3 center){
		collider.size = size;
		collider.center = center;
		Debug.Log(size+" "+center);
		this.size.x = size.x * colliderScaleX;
		this.size.y = size.y * colliderScaleY;
		this.center = center * colliderScaleX;
	}

}
