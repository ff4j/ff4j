package org.ff4j.property;

import java.util.Arrays;
import java.util.List;

import org.ff4j.property.PropertyLogLevel.LogLevel;

/**
 * Load property as list of {@link LogLevel }.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyListLogLevel extends PropertyList<LogLevel  , PropertyLogLevel> {
    
    /** Serial Number. */
    private static final long serialVersionUID = 8808039388183628746L;
    
    public PropertyListLogLevel(String uid) {
        super(uid);
    }
    public PropertyListLogLevel(String uid, String valueAsString) {
        super(uid, valueAsString);
    }
    public PropertyListLogLevel(String uid, List< LogLevel  > value) {
        super(uid, value);
    }
    public PropertyListLogLevel(String uid, LogLevel  ... value) {
        super(uid, Arrays.asList(value));
    }
  

}
