package org.ff4j.property;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link BigDecimal}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListBigDecimal extends PropertyList<BigDecimal, PropertyBigDecimal > {
   
    /** Serial */
    private static final long serialVersionUID = -7372034033467455946L;
    
    public PropertyListBigDecimal(String uid) {
        super(uid);
    }
    public PropertyListBigDecimal(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListBigDecimal(String uid, List<BigDecimal> value) {
        super(uid, value);
    }
    public PropertyListBigDecimal(String uid, BigDecimal... value) {
        super(uid, Arrays.asList(value));
    }
  

}
