package org.ff4j;

import org.ff4j.backend.Backend;
import org.ff4j.backend.BackendSupport;
import org.ff4j.cache.FF4jCacheRepository;
import org.ff4j.cache.FF4jCacheInMemory;
import org.ff4j.utils.Assert;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * FF4j can retrieve configuration from the backend.
 */
public class FF4jConfiguration {

    /** Logger for our Client. */
    private static final Logger LOGGER = LoggerFactory.getLogger(FF4jConfiguration.class);

    /** Default load balancer. */
    public static final String DEFAULT_DATACENTER = "default";

    /** Default load balancer. */
    public static final String DEFAULT_BACKEND_NAME = "default";

    // -- Keys in the db for configuration

    /** Enable cache at client side. */
    public static final String CONF_CACHE_ENABLE = "ff4j-config-client-cache";

    /** Enable cache polling/auto-refresh. */
    public static final String CONF_CACHE_TTL = "ff4j-config-client-cache-ttl";

    /** Enable cache polling/auto-refresh. */
    public static final String CONF_CACHE_POLLING = "ff4j-config-client-cache-polling";

    /** Enable cache polling/auto-refresh. */
    public static final String CONF_CACHE_CLASS = "ff4j-config-client-cache-class";

    /** Enable cache at client side. */
    public static final String CONF_NAMESPACE = "ff4j-config-client-namespace";

    /** Client identifier. */
    protected String username;

    /** Client secret. */
    protected String password;

    /** Configuration Key could be in a dedicated namespace. */
    protected String configNamespace = FF4j.DEFAULT_WORKSPACE;

    /** Current Namespace. */
    protected String workspace = FF4j.DEFAULT_WORKSPACE;

    /** List of Backends to load Data. */
    protected Map<String, List<Backend>> backendDeployment = new HashMap<>();

    /** Preferred datacenter. */
    protected String localDatacenter;

    /** Toggle cache auto-refresh at client side. */
    protected boolean cachePolling = false;

    /** Time to live for items in the cache. */
    protected Duration cachePollingInterval =  Duration.of(60, ChronoUnit.SECONDS);

    /** Time to live for items in the cache. */
    protected Duration cacheTimeToLive =  Duration.of(60, ChronoUnit.SECONDS);

    /** Explicit Cache configuration. */
    protected FF4jCacheRepository cacheRepository;

    /** Enable configuration Keys. */
    protected Map<String, Object> customProperties = new HashMap<>();

    /**
     * Default constructor.
     */
    public FF4jConfiguration() {}

    /**
     * Provide clientId.
     *
     * @param username
     *            username
     * @param password
     *           password
     * @return self reference
     */
    public FF4jConfiguration withCredentials(String username, String password) {
        return withUsername(username).withPassword(password);
    }

    /**
     * Provide clientId.
     *
     * @param cache
     *            cache implementation
     * @return self reference
     */
    public FF4jConfiguration withCache(FF4jCacheRepository cache) {
        Assert.assertNotNull(cache);
        this.cacheRepository = cache;
        return withUsername(username).withPassword(password);
    }

    /**
     * Provide clientId.
     *
     * @param username
     *            username
     * @return self reference
     */
    public FF4jConfiguration withUsername(String username) {
        Assert.assertHasLength(username, "username");
        this.username = username;
        LOGGER.debug("Username '%s' has been provided", username);
        return this;
    }

    /**
     * Provide clientSecret.
     *
     * @param password
     *           password
     * @return self reference
     */
    public FF4jConfiguration withPassword(String password) {
        Assert.assertHasLength(password, "password");
        this.password = password;
        return this;
    }

    /**
     * Provide namespace.
     *
     * @param namespace
     *            namespace
     * @return self reference
     */
    public FF4jConfiguration withNamespace(String namespace) {
        Assert.assertHasLength(namespace, "namespace");
        this.workspace = namespace;
        return this;
    }

    /**
     * Provide namespace for config keys and properties
     *
     * @param namespace
     *            namespace
     * @return self reference
     */
    public FF4jConfiguration withNamespaceConfig(String namespace) {
        Assert.assertHasLength(namespace, "namespace");
        this.configNamespace = namespace;
        return this;
    }

    /**
     * Enable cache
     *
     * @return
     *      current object
     */
    public FF4jConfiguration withCache() {
        cacheRepository = new FF4jCacheInMemory();
        return this;
    }

    /**
     * Enable cache
     *
     * @param ttl
     *      cache time to live
     * @return
     *      current object
     */
    public FF4jConfiguration withCacheTtl(Duration ttl) {
        cacheTimeToLive = ttl;
        return this;
    }

    /**
     * Enable cache polling.
     *
     * @return
     *      current object
     */
    public FF4jConfiguration withCachePolling() {
        cachePolling = true;
        return this;
    }

    /**
     * Enable cache polling.
     *
     * @param pollingInterval
     *      polling interval
     * @return
     *      current object
     */
    public FF4jConfiguration withCachePolling(Duration pollingInterval) {
        cachePolling = true;
        cachePollingInterval = pollingInterval;
        return this;
    }

    /**
     * Add special parameter in configuration.
     *
     * @param key
     *      param key
     * @param value
     *      param value
     * @return
     *      current object
     */
    public FF4jConfiguration withProperty(String key, Object value) {
        Assert.assertNotNull(key, "Key cannot be null nor empty");
        Assert.assertNotNull(value, "Value cannot be null nor empty");
        customProperties.put(key, value);
        return this;
    }

    /**
     * Provide namespace.
     *
     * @param datacenter
     *            namespace
     * @return self reference
     */
    public FF4jConfiguration withLocalDatacenter(String datacenter) {
        Assert.assertHasLength(datacenter, "datacenter");
        this.localDatacenter = workspace;
        return this;
    }

    /**
     * Builder pattern.
     *
     * @param backend
     *      backend added to deployment
     * @return
     *     current reference
     */
    public FF4jConfiguration addBackend(BackendSupport backend) {
        return addBackend(DEFAULT_DATACENTER, backend);
    }

    /**
     * Builder pattern.
     *
     * @param dcName
     *      dc name
     * @param backend
     *      backend added to deployment
     * @return
     *     current reference
     */
    public FF4jConfiguration addBackend(String dcName, BackendSupport backend) {
        Assert.assertHasLength(dcName);
        Assert.assertNotNull(backend);
        if (!backendDeployment.containsKey(dcName)) {
            backendDeployment.put(dcName, new ArrayList<>());
        }
        backendDeployment.get(dcName).add(backend);
        if (localDatacenter == null) withLocalDatacenter(dcName);
        return this;
    }

    /**
     * Provide backend lists
     *
     * @param backend
     *      current backend
     * @return
     *      current object
     */
    public FF4jConfiguration withBackends(BackendSupport... backend) {
        return withBackends(DEFAULT_DATACENTER, backend);
    }

    /**
     * Add a list of backends
     *
     * @param backends
     *      list of backends
     * @param datacenter
     *      datacenter name
     * @return
     *      current object
     */
    public FF4jConfiguration withBackends(String datacenter, BackendSupport... backends) {
        Assert.assertNotNull(backends, "Backend cannot be null nor empty");
        LOGGER.debug("Declaring {} backend(s) in datacenter '{}'.", backends.length, datacenter);
        Arrays.stream(backends).forEach(b -> this.addBackend(datacenter, b));
        return this;
    }

    /**
     * Gets clientId
     *
     * @return value of clientId
     */
    public String getUsername() {
        return username;
    }

    /**
     * Gets clientSecret
     *
     * @return value of clientSecret
     */
    public String getPassword() {
        return password;
    }

    /**
     * Gets cachePolling
     *
     * @return value of cachePolling
     */
    public boolean isCachePolling() {
        return cachePolling;
    }

    /**
     * Gets cachePollingInterval
     *
     * @return value of cachePollingInterval
     */
    public Duration getCachePollingInterval() {
        return cachePollingInterval;
    }

    /**
     * Gets cacheTtl
     *
     * @return value of cacheTtl
     */
    public Duration getCacheTimeToLive() {
        return cacheTimeToLive;
    }

    /**
     * Gets extraProperties
     *
     * @return value of extraProperties
     */
    public Map<String, Object> getCustomProperties() {
        return customProperties;
    }

    /**
     * Gets namespace
     *
     * @return value of namespace
     */
    public String getWorkspace() {
        return workspace;
    }

    /**
     * Gets localDatacenter
     *
     * @return value of localDatacenter
     */
    public String getLocalDatacenter() {
        return localDatacenter;
    }

    /**
     * Gets cache
     *
     * @return value of cache
     */
    public FF4jCacheRepository getCacheRepository() {
        return cacheRepository;
    }

    /**
     * Working with configuration.
     *
     * @return
     *      ff4j instance
     */
    public FF4j build() {
        return new FF4j(this);
    }
}
