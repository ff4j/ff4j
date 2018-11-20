package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Float }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListFloat extends PropertyList<Float  , PropertyFloat > {
        
    /** Serial Number. */
    private static final long serialVersionUID = 1279250936700437786L;
    
    public PropertyListFloat(String uid) {
        super(uid);
    }
    public PropertyListFloat(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListFloat(String uid, List< Float  > value) {
        super(uid, value);
    }
    public PropertyListFloat(String uid, Float  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
