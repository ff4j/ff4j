package org.ff4j.property;

import java.time.Instant;
import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link Instant }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListInstant extends PropertyList<Instant  , PropertyInstant> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 7949667014583651675L;
    
    public PropertyListInstant(String uid) {
        super(uid);
    }
    public PropertyListInstant(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListInstant(String uid, List< Instant  > value) {
        super(uid, value);
    }
    public PropertyListInstant(String uid, Instant  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
