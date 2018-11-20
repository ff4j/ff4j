package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link String }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListString extends PropertyList<String, PropertyString > {

    /** Serial */
    private static final long serialVersionUID = -7978535215652429543L;
    
    public PropertyListString(String uid) {
        super(uid);
    }
    public PropertyListString(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListString(String uid, List<String> value) {
        super(uid, value);
    }
    public PropertyListString(String uid, String... value) {
        super(uid, Arrays.asList(value));
    }
  

}
