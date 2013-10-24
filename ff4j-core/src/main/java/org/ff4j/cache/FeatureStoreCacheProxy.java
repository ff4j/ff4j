package org.ff4j.cache;

/*
 * #%L FeatureStoreCacheProxy.java (ff4j-core) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the
 * License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.store.FeatureStore;

/**
 * Access to {@link FeatureStore} could generate some overhead and decrease performances. This is the reason why cache is provided
 * though proxies.
 * 
 * As applications are distributed, the cache itself could be distributed. The default implement is
 * {@link InMemoryFeatureStoreCacheProxy} but other are provided to use distributed cache system as redis or memcached.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreCacheProxy implements FeatureStore {

    /** Target feature store to be proxified to cache features. */
    private FeatureStore target;

    /** cache manager. */
    private FeatureCacheManager cacheManager;

    /**
     * Allow Ioc and defeine default constructor.
     */
    public FeatureStoreCacheProxy() {}

    /**
     * Initialization through constructor.
     * 
     * @param store
     *            target store to retrieve features
     * @param cache
     *            cache manager to limit overhead of store
     */
    public FeatureStoreCacheProxy(FeatureStore store, FeatureCacheManager cache) {
        this.target = store;
        this.cacheManager = cache;
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featureId) {
        // Cache Operations : As modification, flush cache for this
        getCacheManager().evict(featureId);
        // Reach target
        getTarget().enable(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featureId) {
        // Cache Operations : As modification, flush cache for this
        getCacheManager().evict(featureId);
        // Reach target
        getTarget().disable(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featureId) {
        // not in cache but maybe created from now
        if (null == getCacheManager().get(featureId)) {
            return getTarget().exist(featureId);
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        getCacheManager().evict(fp.getUid());
        getTarget().create(fp);
        getCacheManager().put(fp);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        Feature fp = getCacheManager().get(featureUid);
        // not in cache but may has been created from now
        if (null == fp) {
            fp = getTarget().read(featureUid);
            if (fp != null) {
                getCacheManager().put(fp);
            }
        }
        return fp;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        // Cannot be sure of whole cache - do not test any feature one-by-one : accessing FeatureStore
        return getTarget().readAll();
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String featureId) {
        // even is not present, evict won't failed
        getCacheManager().evict(featureId);
        // Access target store
        getTarget().delete(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        getCacheManager().evict(fp.getUid());
        getTarget().update(fp);
    }

    @Override
    public void grantRoleOnFeature(String featureId, String roleName) {
        getCacheManager().evict(featureId);
        getTarget().grantRoleOnFeature(featureId, roleName);
    }

    @Override
    public void removeRoleFromFeature(String featureId, String roleName) {
        getCacheManager().evict(featureId);
        getTarget().removeRoleFromFeature(featureId, roleName);
    }

    /**
     * Getter accessor for attribute 'target'.
     * 
     * @return current value of 'target'
     */
    public FeatureStore getTarget() {
        if (target == null) {
            throw new IllegalArgumentException("ff4j-core: Target for cache proxy has not been provided");
        }
        return target;
    }

    /**
     * Setter accessor for attribute 'target'.
     * 
     * @param target
     *            new value for 'target '
     */
    public void setTarget(FeatureStore target) {
        this.target = target;
    }

    /**
     * Getter accessor for attribute 'cacheManager'.
     * 
     * @return current value of 'cacheManager'
     */
    public FeatureCacheManager getCacheManager() {
        if (cacheManager == null) {
            throw new IllegalArgumentException("ff4j-core: CacheManager for cache proxy has not been provided but it's required");
        }
        return cacheManager;
    }

    /**
     * Setter accessor for attribute 'cacheManager'.
     * 
     * @param cacheManager
     *            new value for 'cacheManager '
     */
    public void setCacheManager(FeatureCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }
}
