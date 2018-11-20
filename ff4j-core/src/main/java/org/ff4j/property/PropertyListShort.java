package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Short }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListShort extends PropertyList<Short, PropertyShort > {

    /** Serial */
    private static final long serialVersionUID = -7978535215652429543L;
    
    public PropertyListShort(String uid) {
        super(uid);
    }
    public PropertyListShort(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListShort(String uid, List<Short> value) {
        super(uid, value);
    }
    public PropertyListShort(String uid, Short... value) {
        super(uid, Arrays.asList(value));
    }
  

}
