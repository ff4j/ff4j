package org.ff4j.property.list;

import java.util.List;

/**
 * Specialization of {@link AbstractPropertyList} using Double.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyListBoolean extends AbstractPropertyList < Boolean > {

    /** Serial. */
    private static final long serialVersionUID = 2044668915134536364L;

    /**
     * Default constructor.
     */
    public PropertyListBoolean() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyListBoolean(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyListBoolean(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyListBoolean(String uid, List<Boolean> value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public  List<Boolean> fromString(String v) {
        return super.fromString(v);
    }
    
}
