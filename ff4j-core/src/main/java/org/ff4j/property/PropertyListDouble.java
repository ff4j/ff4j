package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Double}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListDouble extends PropertyList<Double , PropertyDouble > {
        
    /** Serial Number. */
    private static final long serialVersionUID = 4285280536552033023L;
    
    public PropertyListDouble(String uid) {
        super(uid);
    }
    public PropertyListDouble(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListDouble(String uid, List< Double > value) {
        super(uid, value);
    }
    public PropertyListDouble(String uid, Double ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
