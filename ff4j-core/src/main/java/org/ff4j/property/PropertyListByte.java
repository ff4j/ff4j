package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

public class PropertyListByte extends PropertyList<Byte , PropertyByte > {
    
    /** Serial Number. */
    private static final long serialVersionUID = 5183534695780541411L;
    
    public PropertyListByte(String uid) {
        super(uid);
    }
    public PropertyListByte(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListByte(String uid, List<Byte > value) {
        super(uid, value);
    }
    public PropertyListByte(String uid, Byte ... value) {
        super(uid, Arrays.asList(value));
    }

}
