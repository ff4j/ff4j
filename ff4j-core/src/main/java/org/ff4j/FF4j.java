package org.ff4j;

import org.ff4j.backend.Backend;
import org.ff4j.backend.BackendSupport;
import org.ff4j.backend.BackendTopology;
import org.ff4j.cache.FF4jCache;
import org.ff4j.persistence.inmemory.BackendRepositoryInMemory;
import org.ff4j.property.Property;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Main class for FF4j.
 */
public class FF4j implements FF4jClient {

    /**
     * Logger for our Client.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(FF4j.class);

    /**
     * Starting time, use to compute uptime.
     */
    private final Instant startTime = Instant.now();

    /**
     * Hold configuration.
     */
    protected final FF4jConfiguration config;

    /**
     * Backends with load-balancing and fail-over.
     */
    protected final BackendTopology backends;

    /**
     * Shared context passed over strategies.
     */
    private final ThreadLocal<FF4jContext> context = new ThreadLocal<>();

    /**
     * If enabled will cache values and auto refresh
     */
    private final FF4jCache cache;

    /**
     * Initialization with default configuration.
     * - no cache
     * - single in memory backend
     */
    public FF4j() {
        this(builder().withBackends(new BackendSupport(new BackendRepositoryInMemory())));
        LOGGER.info("FF4j has been successfully initialized");
    }

    /**
     * Initialization with a Builder.
     *
     * @return configuration
     */
    public static FF4jConfiguration builder() {
        return new FF4jConfiguration();
    }

    /**
     * Initialization with provided configuration.
     *
     * @param conf
     *         configuration of the client
     */
    protected FF4j(FF4jConfiguration conf) {
        // Configuration reference
        this.config = conf;

        // Enforce user/password to each backend if provided at root level
        if ((conf.username != null) && (conf.password != null)) {
            conf.backendDeployment
                    .values().stream()
                    .flatMap(List::stream)
                    .forEach(b -> b.setCredentials(conf.username, conf.password));
        }

        // Backends
        this.backends = new BackendTopology(conf.localDatacenter, conf.backendDeployment);

        // Backends Cache
        this.cache = (conf.getCacheRepository() != null) ? new FF4jCache(this, conf) : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getCurrentWorkspace() {
        return Optional.ofNullable(this.config.workspace).orElse(DEFAULT_WORKSPACE);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<String> getWorkspaces() {
        // not cached
        return getBackendOperations().getWorkspaces();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Boolean> getFeatureFlags(String workspace, FF4jEvaluationContext context) {
        // Cannot cache if dynamic context provided
        if (cache != null && context == null) cache.getFeatureFlags(workspace);
        Map<String, Boolean> flags = getBackendOperations().getFeatureFlags(workspace, context);
        if (cache != null && context == null) {
            flags.forEach((k,v) -> cache.getCacheRepository().cacheFeatureFlag(workspace, k, v));
        }
        return flags;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Boolean> findFeatureFlag(String workspace, String uid, FF4jEvaluationContext context) {
        if (cache != null && context == null) return cache.findFeatureFlag(workspace, uid, null);
        Optional<Boolean> flag = getBackendOperations().findFeatureFlag(workspace, uid, context);
        flag.ifPresent(f -> cache.getCacheRepository().cacheFeatureFlag(workspace, uid, f));
        return flag;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<Property<?>> getProperties(String workspace) {
        if (cache != null) cache.getProperties(workspace);
        List<Property<?>> repoProperties = getBackendOperations().getProperties(workspace).toList();
        if (cache != null) {
            repoProperties.forEach(p -> cache.getCacheRepository().cacheProperty(workspace, p));
        }
        return repoProperties.stream();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Property<?>> findProperty(String workspace, String uid) {
        if (cache != null) {
            Optional<Property<?>> cachedProperty = cache.findProperty(workspace, uid);
            if (cachedProperty.isPresent()) return cachedProperty;
        }
        Optional<Property<?>> repoProperty = getBackendOperations().findProperty(workspace, uid);
        if (cache != null && repoProperty.isPresent()) {
            cache.getCacheRepository().cacheProperty(workspace, repoProperty.get());
        }
        return repoProperty;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean test(String workspace, String uid, FF4jEvaluationContext context) {
        // Cannot cache if dynamic context provided
        if (cache != null && context == null) cache.test(workspace, uid);
        boolean flag = getBackendOperations().test(workspace, uid, context);
        if (cache != null && context == null) {
            cache.getCacheRepository().cacheFeatureFlag(workspace, uid, flag);
        }
        return flag;
    }

    /**
     * Look up for a backend (or local cache).
     *
     * @return current backend
     */
    public Backend getBackendOperations() {
        return backends.lookupBackend().getResource();
    }

    /**
     * Gets start time
     *
     * @return value of start
     */
    public Instant getStartTime() {
        return startTime;
    }

    /**
     * Gets context
     *
     * @return value of context
     */
    public ThreadLocal<FF4jContext> getContext() {
        return context;
    }

    /**
     * Gets config
     *
     * @return value of config
     */
    public FF4jConfiguration getConfiguration() {
        return config;
    }


}