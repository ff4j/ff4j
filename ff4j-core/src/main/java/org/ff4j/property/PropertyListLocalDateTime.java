package org.ff4j.property;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

/**
 * Load property as list of {@link LocalDateTime }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListLocalDateTime extends PropertyList<LocalDateTime  , PropertyLocalDateTime> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 4607459332732197884L;
    
    public PropertyListLocalDateTime(String uid) {
        super(uid);
    }
    public PropertyListLocalDateTime(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListLocalDateTime(String uid, List< LocalDateTime  > value) {
        super(uid, value);
    }
    public PropertyListLocalDateTime(String uid, LocalDateTime  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
