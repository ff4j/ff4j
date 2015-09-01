package org.ff4j.property.list;

import java.util.List;

/**
 * Specialization of {@link AbstractPropertyList} using Double.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyList extends AbstractPropertyList < String > {

    /** Serial. */
    private static final long serialVersionUID = 2044668915134536364L;

    /**
     * Default constructor.
     */
    public PropertyList() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyList(String name) {
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
    public PropertyList(String uid, String value) {
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
    public PropertyList(String uid, List<String> value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public  List<String> fromString(String v) {
        return super.fromString(v);
    }
    
}
