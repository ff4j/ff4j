package org.ff4j.property;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

public class PropertySetString extends PropertySet<String, PropertyString > {

    /** Serial */
    private static final long serialVersionUID = -7978535215652429543L;
    
    public PropertySetString(String uid) {
        super(uid);
    }
    public PropertySetString(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertySetString(String uid, Set<String> value) {
        super(uid, value);
    }
    public PropertySetString(String uid, String... value) {
        super(uid, new LinkedHashSet<String>(Arrays.asList(value)));
    }
  

}
