package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Class}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListClass extends PropertyList<Class<?> , PropertyClass > {
     
    /** Serial Number. */
    private static final long serialVersionUID = 3268988106433496994L;
    
    public PropertyListClass(String uid) {
        super(uid);
    }
    public PropertyListClass(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListClass(String uid, List< Class<?> > value) {
        super(uid, value);
    }
    public PropertyListClass(String uid, Class<?> ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
