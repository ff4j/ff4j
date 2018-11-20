package org.ff4j.property;

import java.util.Date;
import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Date}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListDate extends PropertyList<Date, PropertyDate > {
     
    /** Serial Number. */
    private static final long serialVersionUID = 3088617073441400442L;
    
    public PropertyListDate(String uid) {
        super(uid);
    }
    public PropertyListDate(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListDate(String uid, List< Date  > value) {
        super(uid, value);
    }
    public PropertyListDate(String uid, Date  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
