package org.ff4j.property;

import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

/**
 * Load property as list of {@link Calendar}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListCalendar extends PropertyList<Calendar , PropertyCalendar > {
    
    /** Serial Number. */
    private static final long serialVersionUID = -4891966327935318293L;
    
    public PropertyListCalendar(String uid) {
        super(uid);
    }
    public PropertyListCalendar(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListCalendar(String uid, List< Calendar > value) {
        super(uid, value);
    }
    public PropertyListCalendar(String uid, Calendar ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
