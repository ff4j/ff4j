package org.ff4j.cache;

import org.ff4j.FF4j;
import org.ff4j.FF4jClient;
import org.ff4j.FF4jConfiguration;
import org.ff4j.property.Property;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

import java.time.Duration;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * Implementation of local cache to offload persistence.
 */
public class FF4jCache implements FF4jClient {

    /** Delay before starting polling. */
    public static final long INITIAL_DELAY = 0;

    /** FF4j reference. */
    private final FF4j ff4j;

    /** Configuration reference. */
    private final FF4jConfiguration ff4jConfiguration;

    /** Cache manager. */
    private final FF4jCacheRepository cacheRepository;

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
    public FF4jCache(FF4j ff4j, FF4jConfiguration config) {
        super();
        this.ff4j = ff4j;
        this.ff4jConfiguration = config;
        this.cacheRepository = config.getCacheRepository();
        this.cacheTimeToLive = config.getCacheTimeToLive();
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
        FF4jCacheWorker worker = new FF4jCacheWorker(ff4j, cacheRepository, ff4jConfiguration.getWorkspace());
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

    @Override
    public boolean test(String workspace, String uid, FF4jEvaluationContext context) {
        return false;
    }

    @Override
    public Stream<String> getWorkspaces() {
        return null;
    }

    @Override
    public Map<String, Boolean> getFeatureFlags(String workspace, FF4jEvaluationContext context) {
        return null;
    }

    @Override
    public Optional<Boolean> findFeatureFlag(String workspace, String uid, FF4jEvaluationContext context) {
        return Optional.empty();
    }

    @Override
    public Stream<Property<?>> getProperties(String workspace) {
        return null;
    }

    @Override
    public Optional<Property<?>> findProperty(String workspace, String uid) {
        return Optional.empty();
    }

    /**
     * Gets cacheRepository
     *
     * @return value of cacheRepository
     */
    public FF4jCacheRepository getCacheRepository() {
        return cacheRepository;
    }
}
