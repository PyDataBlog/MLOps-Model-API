package trusis.config.personaggi;

import java.util.ArrayList;
import java.util.List;

import trusis.config.personaggi.define.Personaggio;
import trusis.config.personaggi.define.stdPersonaggio;
import trusis.config.poteri.UccisioneNotturna;
import trusis.config.poteri.define.Potere;

public class Trusis extends stdPersonaggio
{
    private String nome="Trusis";
    private String fazione="Cattivi";
    private boolean vivo=true;
    private boolean killable=true;
    private List<Personaggio> conoscenti = new ArrayList<Personaggio>();
    
    public Trusis(List<Potere> listPoteri)
    {
        setPoteri(listPoteri);
    }
    public Trusis()
    {
        UccisioneNotturna uccisioneNotturna = new UccisioneNotturna();
        List<Potere> poteriTrusis = new ArrayList<Potere>();
        poteriTrusis.add(uccisioneNotturna);
        setter(poteriTrusis);
    }
    private void setter(List<Potere> poteri)
    {
        setNome(nome);
        setFazione(fazione);
        setVivo(vivo);
        setKillable(killable);
        setPoteri(poteri);
        setConoscenti(conoscenti);
    }
}
