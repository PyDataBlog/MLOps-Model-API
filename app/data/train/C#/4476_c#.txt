using UnityEngine;

public class SimInit : MonoBehaviour
{
    public int MaxMolecules = 500;

    public GameObject MoleculeAPrefab;
    public GameObject Walls;

    public void Start()
    {
        for ( var i = 0; i < MaxMolecules; i++ ) {
            Vector3 pos = new Vector3(
                        Random.Range( -9, 9 ) + ( Random.Range( 0, 1000 ) / 1000 ),
                        Random.Range( -9, 9 ) + ( Random.Range( 0, 1000 ) / 1000 ),
                        0 );

            Instantiate( MoleculeAPrefab, pos, Quaternion.identity );
        }
    }

    public void Update()
    {
        if ( Input.anyKeyDown ) {
            Walls.SetActive( !Walls.activeInHierarchy );
        }
    }
}
