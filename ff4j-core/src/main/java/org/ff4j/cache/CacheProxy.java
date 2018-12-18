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

import java.io.Serializable;
import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.FF4jEntity;
import org.ff4j.FF4jRepository;
import org.ff4j.FF4jRepositoryListener;
import org.ff4j.FF4jRepositoryObserver;
import org.ff4j.event.repository.EventAuditTrailRepository;

/**
 * Cache abstraction for ff4j.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 * @author Andre Blaszczyk (@AndrBLASZCZYK)
 *
 * @param <K>
 *      cache key
 * @param <V>
 *      cache value
 */
public class CacheProxy < K extends Serializable, V extends FF4jEntity<?> > 
            extends FF4jRepositoryObserver < FF4jRepositoryListener < V > >  
            implements FF4jRepository< String, V > {
    
    /** cache manager. */
    protected CacheManager< String , V > cacheManager;
    
    /** Daemon to fetch data from target store to cache on a fixed delay basis. */
    protected CachePollingScheduler< V > scheduler = null;
    
    /** Target store. */
    protected FF4jRepository< String, V > targetStore;
    
    /**
     * Start the polling of target store is required.
     */
    public void startPolling(long delay) {
        if (scheduler == null) {
            throw new IllegalStateException("The poller has not been initialize, please check");
        }
        scheduler.start(delay);
    }
    
    /**
     * Stop the polling of target store is required.
     */
    public void stopPolling() {
        if (scheduler == null) {
            throw new IllegalStateException("The poller has not been initialize, please check");
        }
        scheduler.stop();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exists(String uid) {
        // not in cache but maybe created from last access
        if (cacheManager.get(uid) == null) {
            return getTargetStore().exists(uid);
        }
        return true;
    }
    
    /** {@inheritDoc} */
    @Override
    public V read(String uid) {
        Optional<V> fp = cacheManager.get(uid);
        // not in cache but may has been created from now
        if (!fp.isPresent()) {
            V f = getTargetStore().read(uid);
            cacheManager.put(f.getUid(), f);
            fp = Optional.of(f);
        }
        return fp.get();
    }

    /** {@inheritDoc} */
    @Override
    public Stream <V> findAll() {
        // Cannot be sure of whole cache - do not test any feature one-by-one : accessing FeatureStore
        return getTargetStore().findAll();
    }    
    
    /** {@inheritDoc} */
    @Override
    public void save(Iterable<V> entities) {
        if (entities != null) {
            getTargetStore().save(entities);
            entities.forEach(p -> cacheManager.put(p.getUid(), p));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void delete(Iterable<String> entities) {
        if (entities != null) {
            // Access target store
            getTargetStore().delete(entities);
            // even is not present, evict won't failed
            entities.forEach(p -> cacheManager.evict(p));
        }
    }
    

    /** {@inheritDoc} */
    @Override
    public long count() {
        return getTargetStore().count();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        return getTargetStore().isEmpty();
    }

    /** {@inheritDoc} */
    @Override
    public Optional<V> find(String id) {
        Optional <V> fp = cacheManager.get(id);
        // not in cache but may has been created from now
        if (!fp.isPresent()) {
            fp = getTargetStore().find(id);
            if (fp.isPresent()) {
                cacheManager.put(fp.get().getUid(), fp.get());
            }
        }
        return fp;
    }    
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        // Cache Operations : As modification, flush cache for this
        cacheManager.clear();
        getTargetStore().deleteAll();
    }
    
    /** {@inheritDoc} */
    public boolean isCached() {
        return true;
    }

    /** {@inheritDoc} */
    public String getCacheProvider() {
        if (cacheManager != null) {
            return cacheManager.getCacheProviderName();
        } else {
            return null;
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        /* Most of the time there is nothing to do. The feature and properties are createdat runtime.
         * But not always (JDBC, Mongo, Cassandra)... this is the reason why the dedicated store must 
         * override this method. It a default implementation (Pattern Adapter).
         */
        return;
    }
    
    /** {@inheritDoc} */
    public Stream<String> findAllIds() {
    	return null;
	}
    
    /**
     * Getter accessor for attribute 'cacheManager'.
     * 
     * @return current value of 'cacheManager'
     */
    public CacheManager< String, V> getCacheManager() {
        if (cacheManager == null) {
            throw new IllegalArgumentException("CacheManager for cache proxy has not been provided but it's required");
        }
        return cacheManager;
    }
   
    /**
     * Getter accessor for attribute 'targetStore'.
     *
     * @return
     *       current value of 'targetStore'
     */
    public FF4jRepository<String, V> getTargetStore() {
        return targetStore;
    }

    @Override
    public void registerAuditListener(EventAuditTrailRepository auditTrail) {}

    @Override
    public void unRegisterAuditListener() {}
	

}
