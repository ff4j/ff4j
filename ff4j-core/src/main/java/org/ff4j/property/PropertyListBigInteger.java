package org.ff4j.property;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

public class PropertyListBigInteger extends PropertyList<BigInteger, PropertyBigInteger > {

    /** Serial */
    private static final long serialVersionUID = -7978535215652429543L;
    
    public PropertyListBigInteger(String uid) {
        super(uid);
    }
    public PropertyListBigInteger(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListBigInteger(String uid, List<BigInteger> value) {
        super(uid, value);
    }
    public PropertyListBigInteger(String uid, BigInteger... value) {
        super(uid, Arrays.asList(value));
    }
  

}
