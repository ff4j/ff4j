package org.ff4j.property;

/**
 * Implementation of Property for java {@link Class}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyClass extends Property< Class<?> > {

    /** Serial. */
    private static final long serialVersionUID = 1215847831344778135L;

    /**
     * Default constructor.
     */
    public PropertyClass() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyClass(String name) {
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
    public PropertyClass(String uid, String value) {
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
    public PropertyClass(String uid, Class<?> value) {
       super(uid, value);
    }
    /** {@inheritDoc} */
    @Override
    public Class<?> fromString(String v) {
        try {
            return Class.forName(v);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("The target class has not been found", e);
        }
    }

}
