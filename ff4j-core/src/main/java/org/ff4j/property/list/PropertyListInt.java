package org.ff4j.property.list;

import java.util.List;

/**
 * Specialization of {@link AbstractPropertyList} using Integer.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyListInt extends AbstractPropertyList < Integer > {

    /** Serial. */
    private static final long serialVersionUID = 2044668915134536364L;
    
    /**
     * Default constructor.
     */
    public PropertyListInt() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyListInt(String name) {
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
    public PropertyListInt(String uid, String value) {
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
    public PropertyListInt(String uid, List<Integer> value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public  List<Integer> fromString(String v) {
        return super.fromString(v);
    }


}
