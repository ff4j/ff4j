package org.ff4j.cassandra.store;

import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;

/**
 * Implements of {@link PropertyStore} for sotre Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreCassandra extends AbstractPropertyStore {

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
    }

}
