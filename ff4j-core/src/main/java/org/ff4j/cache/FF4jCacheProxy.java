package org.ff4j.cache;

import java.time.Duration;
import java.util.Optional;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import org.ff4j.FF4jClient;
import org.ff4j.FF4jClientConfiguration;
import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;
import org.ff4j.feature.exception.FeatureAlreadyExistException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;

/**
 * Implementation of local cache to offload persistence.
 */
public class FF4jCacheProxy extends Backend {

    /** Delay before starting polling. */
    public static final long INITIAL_DELAY = 0;

    /** FF4j reference. */
    private final FF4jClient ff4j;

    /** Configuration reference. */
    private final FF4jClientConfiguration ff4jConfiguration;

    /** Cache manager. */
    private final FF4jCache cache;

    /** Time-to-live for items in the cache. */
    protected final Duration cacheTimeToLive;

    /** Scheduler for the worker. */
    private ScheduledExecutorService executor;

    /**
     * Set up the proxy with polling and local data.
     *
     * @param config
     *      ff4j configuration
     * @param ff4j
     *      ff4j reference to lookup for backends when not in cache
     */
    public FF4jCacheProxy(FF4jClient ff4j, FF4jClientConfiguration config) {
        super();
        this.ff4j = ff4j;
        this.ff4jConfiguration = config;
        this.cache = config.getCache();
        this.cacheTimeToLive   = config.getCacheTimeToLive();
        if (config.isCachePolling()) startPolling();
    }

    /** Enable polling. */
    public void startPolling() {
        executor = Executors.newScheduledThreadPool(1, r -> {
            Thread t = new Thread(r, "FF4jCacheAutoRefreshWorker");
            t.setDaemon(true);
            return t;
        });
        // Create worker
        FF4jCacheWorker worker = new FF4jCacheWorker(ff4j, cache, ff4jConfiguration.getNamespace());
        // Setup and start scheduler
        executor.scheduleWithFixedDelay(worker, INITIAL_DELAY,
                ff4jConfiguration.getCachePollingInterval().toMillis(), TimeUnit.MILLISECONDS);
    }

    /** Stop Polling. */
    public void stopPolling() {
        if (executor != null) {
            executor.shutdown();
            executor = null;
        }
    }

    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllNamespaces() {
        // Access store as the cache might not be accurate
        return super.findAllNamespaces();
    }

    /** {@inheritDoc} */
    @Override
    public void createNamespace(String namespace) {
        // Namespaces not in the cache
        super.createNamespace(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteNamespace(String namespace) {
        // Namespaces not in the cache
        super.deleteNamespace(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existNamespace(String namespace) {
        // Namespaces not in the cache
        return super.existNamespace(namespace);
    }

    @Override
    public Stream<Flag> findAllFeatures(String namespace) {
        return super.findAllFeatures(namespace);
    }

    @Override
    public boolean existsFeature(String namespace, String uid) {
        return false;
    }

    @Override
    public void toggleOnFeature(String namespace, String uid)
            throws FeatureNotFoundException {
        findFeatureById(namespace, uid).ifPresentOrElse(
                f -> this.saveFeature(namespace, f.toggleOn()),
                () -> { throw new FeatureNotFoundException(namespace, uid); } );
    }

    @Override
    public void createFeature(String namespace, Flag feature)
    throws FeatureAlreadyExistException {

    }

    @Override
    public void saveFeature(String namespace, Flag feature)
    throws FeatureNotFoundException {
        super.saveFeature(namespace, feature);
        try {
            cache.evictFeature(namespace, feature.getUid());
        } catch(RuntimeException re) {
            cache.onException(re);
        }
    }

    @Override
    public void deleteFeature(String namespace, String uid)
    throws FeatureNotFoundException {

    }

    @Override
    public void renameFeature(String namespace, String old, String uid)
    throws FeatureNotFoundException {

    }

    /** {@inheritDoc} */
    @Override
    public void createProperty(String namespace, Property<?> property)
    throws PropertyAlreadyExistException {
    }

    @Override
    public Optional<Flag> findFeatureById(String namespace, String uid) {
        return Optional.empty();
    }

    @Override
    public Stream<Property<?>> findAllProperties(String namespace) {
        return null;
    }

    @Override
    public boolean existsProperty(String namespace, String uid) {
        return false;
    }

    @Override
    public void saveProperty(String namespace, Property<?> property) {

    }

    @Override
    public void deleteProperty(String namespace, String uid) {

    }

    @Override
    public void renameProperty(String namespace, String old, String uid) {

    }

    @Override
    public Optional<Property<?>> findPropertyById(String namespace, String uid) {
        return Optional.empty();
    }

    @Override
    public void createSchema() {

    }

    @Override
    public FF4jClientConfiguration readConfiguration() {
        return null;
    }

    @Override
    public void saveConfiguration(FF4jClientConfiguration config) {

    }
}
