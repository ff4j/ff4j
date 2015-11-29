package org.ff4j.store;

import java.util.HashMap;
import java.util.Map;

import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

/**
 * Generic {@link PropertyStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreJCache extends AbstractPropertyStore {

    /** Cache Manager. */ 
    private FF4jJCacheManager cacheManager;
    
    /**
     * Initialisaiton of CacheManager.
     */
    public PropertyStoreJCache() {
        cacheManager = new FF4jJCacheManager();
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreJCache(String cachingProviderClassName) {
        // Initialisation of CACHE
        cacheManager = new FF4jJCacheManager(cachingProviderClassName);
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public PropertyStoreJCache(FF4jJCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamNotNull(name, "Property name");
        return (getCacheManager().getProperty(name) != null);
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
        getCacheManager().putProperty(property);
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return getCacheManager().getProperty(name);
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
        getCacheManager().putProperty(property);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        getCacheManager().evictProperty(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, AbstractProperty<?>> readAllProperties() {
        Map<String, AbstractProperty<?>> myMap = new HashMap<String, AbstractProperty<?>>();
        getCacheManager().getPropertiesCache().forEach(e->myMap.put(e.getKey(), e.getValue()));
        return myMap;
    }
    
    /**
     * Getter accessor for attribute 'cacheManager'.
     *
     * @return
     *       current value of 'cacheManager'
     */
    public FF4jJCacheManager getCacheManager() {
        return cacheManager;
    }

    /**
     * Setter accessor for attribute 'cacheManager'.
     * @param cacheManager
     *      new value for 'cacheManager '
     */
    public void setCacheManager(FF4jJCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

}
