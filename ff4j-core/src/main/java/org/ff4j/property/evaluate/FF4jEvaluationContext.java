package org.ff4j.property.evaluate;

import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.utils.Assert;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Optional;

/**
 * Evaluation context.
 */
public class FF4jEvaluationContext extends HashMap<String, Property<?>> {

    /**
     * Constructor.
     *
     * @param properties
     *      list of properties
     */
    public FF4jEvaluationContext(Property<?>... properties) {
        super();
        if (properties != null) Arrays.stream(properties).forEach(this::putProperty);
    }

    /**
     * Get property based on its key.
     *
     * @param uid
     *      identifier
     * @return
     *      return property
     */
    public Optional<Property<?>> findProperty(String uid) {
        Assert.assertHasLength(uid);
        return containsKey(uid) ? Optional.ofNullable(get(uid)) : Optional.empty();
    }

    /**
     * Get property based on its key.
     *
     * @param uid
     *      identifier
     * @return
     *      return property
     */
    public Optional<String> findString(String uid) {
        return findProperty(uid).map(Property::getValueAsString);
    }

    /**
     * Get property based on its key.
     *
     * @param uid
     *      identifier
     * @return
     *      return property
     */
    public Property<?> getProperty(String uid) {
        return findProperty(uid).orElseThrow(() -> new PropertyNotFoundException(uid));
    }

    /**
     * Add a property.
     *
     * @param property
     *      property to add
     */
    public void putProperty(Property<?> property) {
        Assert.assertNotNull(property);
        put(property.getUid(), property);
    }

}
