using UnityEngine;
using System.Collections;
using UnityEngine.SceneManagement;

public class Before1v1Game : MonoBehaviour {
    public GUISkin skin_mapa_Forta;
    public GUISkin skin_mapa_Koila;
    public GUISkin skin_mapa_Gajuta;
    public GUISkin skin_mapa_Omaron;
    public GUISkin skin_mapa_Pountedal;
    public GUISkin skin_mapa_Rountha;
    public GUISkin menuSkin;

    GUIStyle style_mapa_Forta;
    GUIStyle style_mapa_Koila;
    GUIStyle style_mapa_Gajuta;
    GUIStyle style_mapa_Omaron;
    GUIStyle style_mapa_Pountedal;
    GUIStyle style_mapa_Rountha;

    GUIStyle text;
    string selected;
    Languages language = new Languages();
    string Jazyk;
    public static string zakladna = "";
    // Use this for initialization
    void Start()
    {
        text = menuSkin.button;
        style_mapa_Forta = skin_mapa_Forta.button;
        style_mapa_Gajuta = skin_mapa_Gajuta.button;
        style_mapa_Koila = skin_mapa_Koila.button;
        style_mapa_Omaron = skin_mapa_Omaron.button;
        style_mapa_Pountedal = skin_mapa_Pountedal.button;
        style_mapa_Rountha = skin_mapa_Rountha.button;

        selected = "Forta";
        Jazyk = setLanguage.global_jazyk;
        if (Jazyk == "") Jazyk= "english";
        text.fontSize = (int)(Mathf.Round(Screen.width / 30));
    }

    // Update is called once per frame
    void Update()
    {

    }

    void OnGUI()
    {
        if (GUI.Button(new Rect(Screen.width / 2.25f, Screen.height / 9, Screen.width / 4, Screen.height / 5), "", style_mapa_Forta))
        {
            selected = "Forta";
        }

        if (GUI.Button(new Rect(Screen.width / 2.5f, Screen.height / 2.5f, Screen.width / 4, Screen.height / 3.5f), "", style_mapa_Gajuta))
        {
            selected = "Gajuta";
        }

        if (GUI.Button(new Rect(Screen.width / 5, Screen.height / 10, Screen.width / 5, Screen.height / 5), "", style_mapa_Koila))
        {
            selected = "Koila";
        }

        if (GUI.Button(new Rect(Screen.width / 1.5f, Screen.height / 1.5f, Screen.width / 8, Screen.height / 9), "", style_mapa_Omaron))
        {
            selected = "Omaron";
        }

        if (GUI.Button(new Rect(Screen.width / 9, Screen.height / 1.35f, Screen.width / 6, Screen.height / 7), "", style_mapa_Pountedal))
        {
            selected = "Pountedal";
        }

        if (GUI.Button(new Rect(Screen.width / 1.35f, Screen.height / 2.25f, Screen.width / 4, Screen.height / 5), "", style_mapa_Rountha))
        {
            selected = "Rountha";
        }
        GUI.Label(new Rect(0, 0, Screen.width, Screen.height/8), language.GetSlovo(Jazyk,"ingame_selectarea"),text);
        GUI.Label(new Rect(0, Screen.height - Screen.height / 8, Screen.width / 2, Screen.height / 8), language.GetSlovo(Jazyk, "ingame_selected") + " "+ selected, text);
        if (GUI.Button(new Rect(Screen.width / 2, Screen.height - Screen.height / 8, Screen.width / 3, Screen.height / 8), language.GetSlovo(Jazyk, "ingame_accept"), text)) 
        {
            zakladna = "selected";
            SceneManager.LoadScene(5);
        }
    }
}
