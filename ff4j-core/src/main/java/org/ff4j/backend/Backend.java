package org.ff4j.backend;

import org.ff4j.FF4jClientConfiguration;
import org.ff4j.FF4jContext;
import org.ff4j.feature.Flag;
import org.ff4j.feature.exception.FeatureAlreadyExistException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;
import org.ff4j.security.AuthenticationManager;
import org.ff4j.utils.Assert;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.concurrent.CompletableFuture.runAsync;
import static org.ff4j.utils.CompletableFutures.allDone;

/**
 * SuperClass for ApiEndpoint.
 */
public class Backend implements BackendRepository {

    /** Instance id. */
    protected final String id;

    /** Client identifier. */
    protected String username;

    /** Client secret. */
    protected String password;

    /** Authentication third party client. */
    protected final AuthenticationManager authenticationManager;

    /** Shared context passed over strategies (EvaluationContext). */
    private final static ThreadLocal<FF4jContext> context = new ThreadLocal<>();

    /** Accessing Data (delegate). */
    protected final BackendRepository backendRepository;

    /** Listener on a repository. */
    protected Map<String, BackendRepositoryListener> backendRepositoryListeners = new HashMap<>();

    /**
     * Constructor for the Cache Proxy only.
     */
    protected Backend() {
        id                    = UUID.randomUUID().toString();
        backendRepository     = null;
        authenticationManager = null;
    }

    /**
     * Constructor with the builder.
     *
     * @param builder
     *          current builder
     */
    protected Backend(BackendBuilder builder) {
        Assert.assertNotNull(builder);
        this.id                         = builder.id;
        this.username                   = builder.username;
        this.password                   = builder.password;
        this.backendRepository          = builder.backendRepository;
        this.backendRepositoryListeners = builder.repositoryListeners;
        this.authenticationManager      = builder.authenticationManager;
    }

    /**
     * Builder pattern for the backend.
     *
     * @return
     *      current builder
     */
    public static BackendBuilder builder() {
        return new BackendBuilder();
    }

    /**
     * Non authentication backend
     *
     * @param backendRepositoru
     *      backend implementation
     */
    public Backend(BackendRepository backendRepositoru) {
        this(FF4jClientConfiguration.DEFAULT_BACKEND_NAME, backendRepositoru, null, null, null);
    }

    /**
     * Non authentication backend
     *
     * @param id
     *      service identifier
     * @param backendRepositoru
     *      backend implementation
     */
    public Backend(String id, BackendRepository backendRepositoru) {
        this(id, backendRepositoru, null, null, null);
    }

    /**
     * Full-fledged constructor.
     *
     * @param id
     *      service identifier
     * @param backend
     *      backend implementation
     * @param username
     *      user identifier
     * @param password
     *      user password
     * @param authenticationManager
     *      authentication layer
     */
    public Backend(String id, BackendRepository backend, String username, String password, AuthenticationManager authenticationManager) {
        this.id = id;
        this.username          = username;
        this.password          = password;
        this.backendRepository = backend;
        this.authenticationManager = authenticationManager;
        notifyListenersAsync(r -> r.onInitialization(getId()));
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        if (hasAnyPermission(AuthenticationManager.FF4J_PERMISSION_CREATE_SCHEMA)) {
            backendRepository.createSchema();
            notifyListenersAsync(r -> r.onCreateSchema(getId()));
        }
    }

    /** {@inheritDoc} */
    @Override
    public FF4jClientConfiguration readConfiguration() {
        // TODO RBAC
        return backendRepository.readConfiguration();
    }

    /** {@inheritDoc} */
    @Override
    public void saveConfiguration(FF4jClientConfiguration config) {
        // TODO RBAC
        backendRepository.saveConfiguration(config);
        notifyListenersAsync(r -> r.onSaveConfiguration(config));
    }

    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllNamespaces() {
        // TODO RBAC
        return backendRepository.findAllNamespaces();
    }

    /** {@inheritDoc} */
    @Override
    public void createNamespace(String namespace) {
        // TODO RBAC
        backendRepository.createNamespace(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteNamespace(String namespace) {
        // TODO RBAC
        backendRepository.deleteNamespace(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existNamespace(String namespace) {
        // TODO RBAC
        return backendRepository.existNamespace(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Flag> findAllFeatures(String namespace) {
        // TODO RBAC
        return backendRepository.findAllFeatures(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existsFeature(String namespace, String uid) {
        // TODO RBAC
        return backendRepository.existsFeature(namespace, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createFeature(String namespace, Flag feature)
    throws FeatureAlreadyExistException {
        if (!existNamespace(namespace)) createNamespace(namespace);
        // TODO RBAC
        backendRepository.createFeature(namespace, feature);
    }

    /** {@inheritDoc} */
    @Override
    public void saveFeature(String namespace, Flag feature)
    throws FeatureNotFoundException {
        // TODO RBAC
        backendRepository.saveFeature(namespace, feature);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String namespace, String uid)
    throws FeatureNotFoundException {
        // TODO RBAC
        backendRepository.deleteFeature(namespace, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void renameFeature(String namespace, String old, String uid)
    throws FeatureNotFoundException {
        // TODO RBAC
        backendRepository.renameFeature(namespace, old, uid);
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Flag> findFeatureById(String namespace, String uid) {
        // TODO RBAC
        return backendRepository.findFeatureById(namespace, uid);
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> findAllProperties(String namespace) {
        // TODO RBAC
        return backendRepository.findAllProperties(namespace);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existsProperty(String namespace, String uid) {
        // TODO RBAC
        return backendRepository.existsProperty(namespace, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void createProperty(String namespace, Property<?> property)
    throws PropertyAlreadyExistException {
        // TODO RBAC
        backendRepository.createProperty(namespace, property);
    }

    /** {@inheritDoc} */
    @Override
    public void saveProperty(String namespace, Property<?> property) {
        // TODO RBAC
        backendRepository.saveProperty(namespace, property);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String namespace, String uid) {
        // TODO RBAC
        backendRepository.deleteProperty(namespace, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void renameProperty(String namespace, String old, String uid) {
        // TODO RBAC
        backendRepository.renameProperty(namespace, old, uid);
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Property<?>> findPropertyById(String namespace, String uid) {
        // TODO RBAC
        return backendRepository.findPropertyById(namespace, uid);
    }

    /**
     * Sent a notification asynchronously to the listener.
     *
     * @param lambda
     *      current lambda
     */
    public void notifyListenersAsync(Consumer<BackendRepositoryListener> lambda) {
        allDone(backendRepositoryListeners.values().stream()
                .map(l -> runAsync(() -> lambda.accept(l)))
                .collect(Collectors.toList()));
    }

    /**
     * Gets id
     *
     * @return value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Update credentials to interact with authManager.
     *
     * @param username
     *      username
     * @param password
     *      password
     */
    public void setCredentials(String username, String password) {
        this.username = username;
        this.password = password;
    }

    /**
     * Test Feature (toggle On / Off).
     *
     * @param namespace
     *      namespace identifier
     * @param uid
     *      feature unique identifier
     * @return
     *      if feature should be toggled
     */
    public boolean test(String namespace, String uid) {
        return findFeatureById(namespace, uid)
                .orElseThrow(() -> new FeatureNotFoundException(namespace, uid))
                .getValue(Backend.getContext()) && permit(namespace, uid);
    }

    /**
     * Current authenticated user is allowed on the feature or anonymous.
     *
     * @param namespace
     *      current namespace
     * @param uid
     *      current flag
     * @return
     *      if user is allowed.
     */
    private boolean permit(String namespace, String uid) {
        // Feature exist
        return true;
    }

    /**
     * Check Permissions.
     *
     * @param permissions
     *      permission
     */
    private boolean hasAnyPermission(String... permissions) {
        if (authenticationManager == null) return true;

        /* Authenticate if needed
        if (null == securityContext.get()) {
            securityContext.set(new SecurityContext(authenticationManager.authenticate(username, password)));
        }

        // Get User
        FF4jUser user = securityContext.get().getUser();
        */
        // Check Permission
        List<String> p = new ArrayList<>();
        //p.addAll(user.getPermissions());
        //p.retainAll(Set.of(permissions));
        return (!p.isEmpty());
    }

    /**
     * Gets securityContext
     *
     * @return value of securityContext
     */
    public static FF4jContext getContext() {
        return context.get();
    }
}
