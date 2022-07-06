
package uk.co.listpoint.context;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Context6Type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="Context6Type">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="Data Standard"/>
 *     &lt;enumeration value="Application"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "Context6Type", namespace = "http://www.listpoint.co.uk/schemas/v6")
@XmlEnum
public enum Context6Type {

    @XmlEnumValue("Data Standard")
    DATA_STANDARD("Data Standard"),
    @XmlEnumValue("Application")
    APPLICATION("Application");
    private final String value;

    Context6Type(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static Context6Type fromValue(String v) {
        for (Context6Type c: Context6Type.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
