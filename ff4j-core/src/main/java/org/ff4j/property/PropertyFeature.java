package org.ff4j.property;

import org.ff4j.core.Feature;

/**
 * {@link Feature} could be embedded into a property to leverage on property store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyFeature extends Property < Feature > {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = -2652848439723919709L;
   
    /**
     * Default constructor.
     */
    public PropertyFeature() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyFeature(String name) {
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
    public PropertyFeature(String uid, String value) {
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
    public PropertyFeature(Feature feat) {
       super(feat.getUid(), feat);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyFeature(String uid, Feature feat) {
       super(uid, feat);
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature fromString(String v) {
        return new Feature(v);
    }
    
}
