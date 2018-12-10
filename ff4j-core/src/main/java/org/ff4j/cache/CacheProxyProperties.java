package org.ff4j.cache;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.feature.repository.FeatureRepository;
import org.ff4j.property.Property;
import org.ff4j.property.repository.PropertyRepository;

/**
 * Access to {@link FeatureRepository} could generate some overhead and decrease performances. This is the reason why cache is provided
 * though proxies.
 * 
 * As applications are distributed, the cache itself could be distributed. The default implement is
 * {@link InMemoryFeatureStoreCacheProxy} but other are provided to use distributed cache system as redis or memcached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class CacheProxyProperties extends CacheProxy< String, Property<?>> implements PropertyRepository {

    /** Target property store to be proxified to cache properties. */
    private PropertyRepository targetPropertyStore;
    
    /**
     * Initialization through constructor.
     * 
     * @param store
     *            target store to retrieve features
     * @param cache
     *            cache manager to limit overhead of store
     */
    public CacheProxyProperties(PropertyRepository fStore, CacheManager< String, Property<?> > cache) {
        this.cacheManager        = cache;
        this.targetPropertyStore  = fStore;
        this.scheduler = new CachePollingSchedulerProperties(fStore, cache);
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        getTargetPropertyStore().createSchema();
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> read(String name, Property<?> defaultValue) {
        Optional < Property<?> > fp = getCacheManager().get(name);
        // Not in cache but may has been created from now
        // Or in cache but with different value that default
        if (!fp.isPresent()) {
            fp = Optional.of(getTargetPropertyStore().read(name, defaultValue));
            getCacheManager().put(fp.get());
        }
        return fp.get();
    }

    /** {@inheritDoc} */
    @Override
    public void update(String name, String newValue) {
        // Retrieve the full object from its name
        Property<?> fp = getTargetPropertyStore().read(name);
        fp.setValueFromString(newValue);
        // Update value in target store
        getTargetPropertyStore().save(fp);
        // Remove from cache old value
        getCacheManager().evict(fp.getUid());
        // Add new value in the cache
        getCacheManager().put(fp);
    }
    
    /** {@inheritDoc} */
    @Override
    public void saveProperty(Property<?> fp) {
        // Update value in target store
        getTargetPropertyStore().save(fp);
        // Remove from cache old value
        getCacheManager().evict(fp.getUid());
        // Add new value in the cache
        getCacheManager().put(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String uid) {
        // Update value in target store
        getTargetPropertyStore().delete(uid);
        // Remove from cache old value
        getCacheManager().evict(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> listPropertyNames() {
        return getTargetPropertyStore().listPropertyNames();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        return getTargetPropertyStore().isEmpty();
    } 
    
    /**
     * Getter accessor for attribute 'target'.
     * 
     * @return current value of 'target'
     */
    public PropertyRepository getTargetPropertyStore() {
        if (targetPropertyStore == null) {
            throw new IllegalArgumentException("ff4j-core: Target for cache proxy has not been provided");
        }
        return targetPropertyStore;
    }     
}
