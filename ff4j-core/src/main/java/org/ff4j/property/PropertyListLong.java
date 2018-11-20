package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Long }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListLong extends PropertyList<Long , PropertyLong> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 6233268464258209549L;
    
    public PropertyListLong(String uid) {
        super(uid);
    }
    public PropertyListLong(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListLong(String uid, List< Long  > value) {
        super(uid, value);
    }
    public PropertyListLong(String uid, Long  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
