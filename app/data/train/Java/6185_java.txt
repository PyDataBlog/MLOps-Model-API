package com.fedevela.core.asic.pojos;

/**
 * Created by fvelazquez on 27/03/14.
 */
import java.util.Date;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.*;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.hibernate.annotations.*;

@Entity
@Table(name = "CHECKLIST_CAP", catalog = "", schema = "PROD")
@XmlRootElement
public class ChecklistCap implements java.io.Serializable {

    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected ChecklistCapPK checklistCapPK;
    @Basic(optional = false)
    @Column(name = "SCLTCOD")
    private Integer scltcod;
    @Column(name = "FECHA_CAPTURA", insertable = true, updatable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date fechaCaptura;
    @Column(name = "FECHA_MODIFICACION")
    @Temporal(TemporalType.TIMESTAMP)
    private Date fechaModificacion;
    @Column(name = "USUARIO_CAPTURA", insertable = true, updatable = false)
    private String usuarioCaptura;
    @Column(name = "FECHA")
    @Temporal(TemporalType.TIMESTAMP)
    private Date fecha;
    @Column(name = "CATEGORIA")
    private Integer categoria;
    @Column(name = "VALOR")
    private Integer valor;
    @Column(name = "OBSERVACIONES")
    private String observaciones;
    @Column(name = "DOCDEF")
    private Integer docdef;
    @Column(name = "CARACTERISTICA")
    private String caracteristica;
    @Column(name = "FECHA_UTIL")
    @Temporal(TemporalType.TIMESTAMP)
    private Date fechaUtil;
    @Lob
    @Column(name = "DATO")
    private String dato;
    @JoinColumn(name = "NUNICODOC", referencedColumnName = "NUNICODOC", insertable = false, updatable = false)
    @ManyToOne(optional = false)
    private CabeceraDoc cabeceraDoc;
    @OneToOne
    @JoinColumnsOrFormulas({
            @JoinColumnOrFormula(column=@JoinColumn(name = "NUNICODOCT", referencedColumnName = "ETIQUETA", insertable = false, updatable = false)),
            @JoinColumnOrFormula(formula=@JoinFormula(value = "'T'", referencedColumnName = "TIPO"))})
    @NotFound(action = NotFoundAction.IGNORE)
    private VwEtiqueta etiqueta;

    public ChecklistCap() {
    }

    public ChecklistCap(ChecklistCapPK checklistCapPK) {
        this.checklistCapPK = checklistCapPK;
    }

    public ChecklistCap(Long nunicodoc, Long nunicodoct, Short doccod) {
        this.checklistCapPK = new ChecklistCapPK(nunicodoc, nunicodoct, doccod);
    }

    /*
     public ChecklistCap(long nunicodoct) {
     this.nunicodoct = nunicodoct;
     }

     public ChecklistCap(long nunicodoct, long nunicodoc) {
     this.nunicodoct = nunicodoct;
     this.nunicodoc = nunicodoc;
     }
     *
     */
    @XmlTransient
    public CabeceraDoc getCabeceraDoc() {
        return cabeceraDoc;
    }

    public void setCabeceraDoc(CabeceraDoc cabeceraDoc) {
        this.cabeceraDoc = cabeceraDoc;
    }

    public String getCaracteristica() {
        return caracteristica;
    }

    public void setCaracteristica(String caracteristica) {
        this.caracteristica = caracteristica;
    }

    public Integer getCategoria() {
        return categoria;
    }

    public void setCategoria(Integer categoria) {
        this.categoria = categoria;
    }

    public Integer getDocdef() {
        return docdef;
    }

    public void setDocdef(Integer docdef) {
        this.docdef = docdef;
    }

    public Date getFecha() {
        return fecha;
    }

    public void setFecha(Date fecha) {
        this.fecha = fecha;
    }

    public Date getFechaCaptura() {
        return fechaCaptura;
    }

    public void setFechaCaptura(Date fechaCaptura) {
        this.fechaCaptura = fechaCaptura;
    }

    public Date getFechaModificacion() {
        return fechaModificacion;
    }

    public void setFechaModificacion(Date fechaModificacion) {
        this.fechaModificacion = fechaModificacion;
    }

    public String getObservaciones() {
        return observaciones;
    }

    public void setObservaciones(String observaciones) {
        this.observaciones = observaciones;
    }

    public Integer getScltcod() {
        return scltcod;
    }

    public void setScltcod(Integer scltcod) {
        this.scltcod = scltcod;
    }

    public String getUsuarioCaptura() {
        return usuarioCaptura;
    }

    public void setUsuarioCaptura(String usuarioCaptura) {
        this.usuarioCaptura = usuarioCaptura;
    }

    public Integer getValor() {
        return valor;
    }

    public void setValor(Integer valor) {
        this.valor = valor;
    }

    @XmlTransient
    public ChecklistCapPK getChecklistCapPK() {
        return checklistCapPK;
    }

    public void setChecklistCapPK(ChecklistCapPK checklistCapPK) {
        this.checklistCapPK = checklistCapPK;
    }

    public Long getNunicodoc() {
        return checklistCapPK.getNunicodoc();
    }

    public void setNunicodoc(Long nunicodoc) {
        if (checklistCapPK == null) {
            checklistCapPK = new ChecklistCapPK();
        }
        checklistCapPK.setNunicodoc(nunicodoc);
    }

    public Long getNunicodoct() {
        return checklistCapPK.getNunicodoct();
    }

    public void setNunicodoct(Long nunicodoct) {
        if (checklistCapPK == null) {
            checklistCapPK = new ChecklistCapPK();
        }
        checklistCapPK.setNunicodoct(nunicodoct);
    }

    public Short getDoccod() {
        return checklistCapPK.getDoccod();
    }

    public void setDoccod(Short doccod) {
        if (checklistCapPK == null) {
            checklistCapPK = new ChecklistCapPK();
        }
        checklistCapPK.setDoccod(doccod);
    }

    public Date getFechaUtil() {
        return fechaUtil;
    }

    public void setFechaUtil(Date fechaUtil) {
        this.fechaUtil = fechaUtil;
    }

    public String getDato() {
        return dato;
    }

    public void setDato(String dato) {
        this.dato = dato;
    }

    @XmlTransient
    @JsonIgnore
    public VwEtiqueta getEtiqueta() {
        return etiqueta;
    }

    public void setEtiqueta(VwEtiqueta etiqueta) {
        this.etiqueta = etiqueta;
    }

    /**
     * Este codigo se queda comentado porque tenemos el error actualmente de
     * insertar T's duplicadas.
     *
     * @param obj
     * @return
     */

    /*@Override
    public int hashCode() {
        int hash = 5;
        hash = 11 * hash + (this.checklistCapPK != null && this.checklistCapPK.getNunicodoct() != null ? this.checklistCapPK.getNunicodoct().hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ChecklistCap other = (ChecklistCap) obj;
        Long id = this.checklistCapPK == null ? null : this.checklistCapPK.getNunicodoct();
        Long ot = other.checklistCapPK == null ? null : other.checklistCapPK.getNunicodoct();
        if (id != ot && (id == null || !id.equals(ot))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "ChecklistCap{" + "checklistCapPK=" + checklistCapPK + ", scltcod=" + scltcod + ", fechaCaptura=" + fechaCaptura + ", fechaModificacion=" + fechaModificacion + ", usuarioCaptura=" + usuarioCaptura + ", fecha=" + fecha + ", categoria=" + categoria + ", valor=" + valor + ", observaciones=" + observaciones + ", docdef=" + docdef + ", caracteristica=" + caracteristica + ", cabeceraDoc=" + cabeceraDoc + '}';
    }*/

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final ChecklistCap other = (ChecklistCap) obj;
        if (this.checklistCapPK != other.checklistCapPK && (this.checklistCapPK == null || !this.checklistCapPK.equals(other.checklistCapPK))) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 73 * hash + (this.checklistCapPK != null ? this.checklistCapPK.hashCode() : 0);
        return hash;
    }

    @Override
    public String toString() {
        return "ChecklistCap{" + "checklistCapPK=" + checklistCapPK + ", scltcod=" + scltcod + ", fechaCaptura=" + fechaCaptura + ", fechaModificacion=" + fechaModificacion + ", usuarioCaptura=" + usuarioCaptura + ", fecha=" + fecha + ", categoria=" + categoria + ", valor=" + valor + ", observaciones=" + observaciones + ", docdef=" + docdef + ", caracteristica=" + caracteristica + ", fechaUtil=" + fechaUtil + ", dato=" + dato + ", cabeceraDoc=" + cabeceraDoc + ", etiqueta=" + etiqueta + '}';
    }

}
