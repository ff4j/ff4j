package org.ff4j.cache;

import java.util.Map;
import java.util.WeakHashMap;

import org.ff4j.core.Feature;
import org.ff4j.store.FeatureStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Default InMemory implementation of cacheProxy.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class InMemoryFeatureStoreCacheProxy extends AbstractFeatureStoreCacheProxy {

    /** Logger for the class. */
    static final Logger LOG = LoggerFactory.getLogger(InMemoryFeatureStoreCacheProxy.class);

    /** Default TTL is one hour. */
    private static final long DEFAULT_TTL = 3600L;

    /** Time to live for cache entries. */
    private long ttl = DEFAULT_TTL;

    /** Cached Feature Map */
    protected Map<String, InMemoryCacheEntry<Feature>> cache = new WeakHashMap<String, InMemoryCacheEntry<Feature>>();

    /**
     * Default constructor to be called by IoC.
     */
    public InMemoryFeatureStoreCacheProxy() {}

    /**
     * Constructor with target {@link FeatureStore}.
     * 
     * @param target
     *            target {@link FeatureStore}
     */
    public InMemoryFeatureStoreCacheProxy(FeatureStore target) {
        setTarget(target);
    }

    /**
     * Constructor with target {@link FeatureStore}.
     * 
     * @param target
     *            target {@link FeatureStore}
     */
    public InMemoryFeatureStoreCacheProxy(FeatureStore target, long ttlSecond) {
        setTarget(target);
        setTtl(ttlSecond);
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        cache.clear();
        LOG.debug("InMemoryCache has been cleared");
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String featureId) {
        if (cache.containsKey(featureId)) {
            cache.remove(featureId);
            LOG.debug("evict feature {} from cache", featureId);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void put(Feature feat) {
        if (feat == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null feature into cache");
        }
        if (feat.getUid() == null || feat.getUid().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert feature with null identifier into cache");
        }
        cache.put(feat.getUid(), new InMemoryCacheEntry<Feature>(feat));
        LOG.debug("Adding {} to cache", feat.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public Feature get(String featureId) {
        InMemoryCacheEntry<Feature> ice = cache.get(featureId);
        if (ice != null) {
            // a feature is stored in cache with this identifier
            if ((System.currentTimeMillis() - ice.getInsertedDate()) >= (1000L * ttl)) {
                // it has reach its time-to-live
                LOG.debug("{} has reached its maximum ttl, evict from cache", featureId);
                evict(featureId);
            } else {
                // return cached value
                return ice.getEntry();
            }
        }
        // ... from here cache does not contain expected id
        Feature feat = getTarget().read(featureId);
        LOG.debug("Reading {} from target store to populate cache", featureId);
        // propagate FeatureNotFoundException if expected
        put(feat);
        return feat;
    }

    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return cache;
    }

    /**
     * Getter accessor for attribute 'ttl'.
     * 
     * @return current value of 'ttl'
     */
    public long getTtl() {
        return ttl;
    }

    /**
     * Setter accessor for attribute 'ttl'.
     * 
     * @param ttl
     *            new value for 'ttl '
     */
    public void setTtl(long ttl) {
        this.ttl = ttl;
    }

}
