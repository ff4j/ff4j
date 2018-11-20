package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Integer }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListInt extends PropertyList<Integer, PropertyInt> {
    
    /** Serial Number. */
    private static final long serialVersionUID = -8027880382680931498L;
    
    public PropertyListInt(String uid) {
        super(uid);
    }
    public PropertyListInt(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListInt(String uid, List< Integer  > value) {
        super(uid, value);
    }
    public PropertyListInt(String uid, Integer  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
