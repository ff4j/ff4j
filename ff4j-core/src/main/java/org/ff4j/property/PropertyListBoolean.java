package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

public class PropertyListBoolean extends PropertyList<Boolean , PropertyBoolean > {

    /** Serial */
    private static final long serialVersionUID = -7978535215652429543L;
    
    public PropertyListBoolean(String uid) {
        super(uid);
    }
    public PropertyListBoolean(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListBoolean(String uid, List<Boolean > value) {
        super(uid, value);
    }
    public PropertyListBoolean(String uid, Boolean ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
