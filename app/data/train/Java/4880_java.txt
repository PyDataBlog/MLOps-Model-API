//
// Ce fichier a été généré par l'implémentation de référence JavaTM Architecture for XML Binding (JAXB), v2.2.11 
// Voir <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Toute modification apportée à ce fichier sera perdue lors de la recompilation du schéma source. 
// Généré le : 2017.09.18 à 11:31:40 AM CEST 
//


package fr.gouv.api.Boamp_v230;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Classe Java pour EnumAvisImplique complex type.
 * 
 * <p>Le fragment de schéma suivant indique le contenu attendu figurant dans cette classe.
 * 
 * <pre>
 * &lt;complexType name="EnumAvisImplique"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;choice&gt;
 *         &lt;element name="MARCHE_PUBLIC" type="{http://www.w3.org/2001/XMLSchema}anyType"/&gt;
 *         &lt;element name="ACCORD_CADRE" type="{http://www.w3.org/2001/XMLSchema}anyType"/&gt;
 *         &lt;element name="SAD" type="{http://www.w3.org/2001/XMLSchema}anyType"/&gt;
 *       &lt;/choice&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "EnumAvisImplique", propOrder = {
    "marchepublic",
    "accordcadre",
    "sad"
})
public class EnumAvisImplique {

    @XmlElement(name = "MARCHE_PUBLIC")
    protected Object marchepublic;
    @XmlElement(name = "ACCORD_CADRE")
    protected Object accordcadre;
    @XmlElement(name = "SAD")
    protected Object sad;

    /**
     * Obtient la valeur de la propriété marchepublic.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Object getMARCHEPUBLIC() {
        return marchepublic;
    }

    /**
     * Définit la valeur de la propriété marchepublic.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setMARCHEPUBLIC(Object value) {
        this.marchepublic = value;
    }

    /**
     * Obtient la valeur de la propriété accordcadre.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Object getACCORDCADRE() {
        return accordcadre;
    }

    /**
     * Définit la valeur de la propriété accordcadre.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setACCORDCADRE(Object value) {
        this.accordcadre = value;
    }

    /**
     * Obtient la valeur de la propriété sad.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Object getSAD() {
        return sad;
    }

    /**
     * Définit la valeur de la propriété sad.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setSAD(Object value) {
        this.sad = value;
    }

}
