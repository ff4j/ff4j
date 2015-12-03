package org.ff4j.store;

import java.util.Map;

import org.ff4j.ehcache.FF4jEhCacheWrapper;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import net.sf.ehcache.Element;
import net.sf.ehcache.config.Configuration;

/**
 * Store {@link AbstractProperty} into EHCache.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreEhCache extends AbstractPropertyStore {
    
    /** Wrap EHCACHE Manager. */
    private FF4jEhCacheWrapper wrapper;

    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache() {
        wrapper = new FF4jEhCacheWrapper();
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache(Configuration cacheConfig) {
        wrapper = new FF4jEhCacheWrapper(cacheConfig);
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache(String xmlEhCacheConfig) {
        wrapper = new FF4jEhCacheWrapper(xmlEhCacheConfig);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamNotNull(name, "Property name");
        return  (wrapper.getCacheProperties().get(name) != null);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(AbstractProperty<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        wrapper.getCacheProperties().put(new Element(property.getName(), property));
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return  (AbstractProperty<?>) wrapper.getCacheProperties().get(name).getObjectValue();
    }

    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        AbstractProperty<?> fp = readProperty(name);
        fp.setValueFromString(newValue);
        updateProperty(fp);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(AbstractProperty<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null");
        }
        if (!existProperty(property.getName())) {
            throw new PropertyNotFoundException(property.getName());
        }
        wrapper.getCacheProperties().put(new Element(property.getName(), property));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        wrapper.getCacheProperties().remove(name);
    }

    @Override
    public Map<String, AbstractProperty<?>> readAllProperties() {
        // TODO Auto-generated method stub
        return null;
    }
}
