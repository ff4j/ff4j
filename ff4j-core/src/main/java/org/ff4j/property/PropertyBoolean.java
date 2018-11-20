package org.ff4j.property;

import org.ff4j.property.exception.InvalidPropertyTypeException;

/**
 * Boolean Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyBoolean extends Property< Boolean > {

    /**
     * represent a boolean propertu.
     */
    private static final long serialVersionUID = -3108407128242804565L;
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyBoolean(String name) {
        this(name, false);
    }
    
    /**
     * Constructor by enum expression.
     *
     * @param uid
     *      unique name
     * @param lvlv
     *     flag value
     */
    public PropertyBoolean(String uid, boolean lvl) {
        this(uid, String.valueOf(lvl));
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      flag value
     */
    public PropertyBoolean(String uid, String value) {
       super(uid, value);
       setFixedValues(Boolean.TRUE, Boolean.FALSE);
    }
    
    /** {@inheritDoc} */
    @Override
    public Boolean fromString(String v) {
        if (!Boolean.TRUE.toString().equals(v.toLowerCase()) &&
            !Boolean.FALSE.toString().equals(v) ) {
            throw new InvalidPropertyTypeException("Cannot cast " + v + "to expected " + Boolean.class);
        }
        return new Boolean(v);
    }

}
