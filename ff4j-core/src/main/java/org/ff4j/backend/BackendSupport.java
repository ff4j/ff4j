package org.ff4j.backend;

import org.ff4j.FF4jConfiguration;
import org.ff4j.FF4jContext;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureFlagAlreadyExistException;
import org.ff4j.feature.exception.FeatureFlagNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
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
public class BackendSupport implements Backend {

    /**
     * Instance id.
     */
    protected final String id;

    /**
     * Client identifier.
     */
    protected String username;

    /**
     * Client secret.
     */
    protected String password;

    /**
     * Authentication third party client.
     */
    protected final AuthenticationManager authenticationManager;

    /**
     * Shared context passed over strategies (EvaluationContext).
     */
    private final static ThreadLocal<FF4jContext> context = new ThreadLocal<>();

    /**
     * Accessing Data (delegate).
     */
    protected final BackendRepository backendRepository;

    /**
     * Listener on a repository.
     */
    protected Map<String, BackendRepositoryListener> backendRepositoryListeners = new HashMap<>();

    /**
     * Constructor for the Cache Proxy only.
     */
    protected BackendSupport() {
        id = UUID.randomUUID().toString();
        backendRepository = null;
        authenticationManager = null;
    }

    /**
     * Constructor with the builder.
     *
     * @param builder
     *         current builder
     */
    protected BackendSupport(BackendBuilder builder) {
        Assert.assertNotNull(builder);
        this.id = builder.id;
        this.username = builder.username;
        this.password = builder.password;
        this.backendRepository = builder.backendRepository;
        this.backendRepositoryListeners = builder.repositoryListeners;
        this.authenticationManager = builder.authenticationManager;
    }

    /**
     * Builder pattern for the backend.
     *
     * @return current builder
     */
    public static BackendBuilder builder() {
        return new BackendBuilder();
    }

    /**
     * Non authentication backend
     *
     * @param backendRepositoru
     *         backend implementation
     */
    public BackendSupport(BackendRepository backendRepositoru) {
        this(FF4jConfiguration.DEFAULT_BACKEND_NAME, backendRepositoru, null, null, null);
    }

    /**
     * Non authentication backend
     *
     * @param id
     *         service identifier
     * @param backendRepositoru
     *         backend implementation
     */
    public BackendSupport(String id, BackendRepository backendRepositoru) {
        this(id, backendRepositoru, null, null, null);
    }

    /**
     * Full-fledged constructor.
     *
     * @param id
     *         service identifier
     * @param backend
     *         backend implementation
     * @param username
     *         user identifier
     * @param password
     *         user password
     * @param authenticationManager
     *         authentication layer
     */
    public BackendSupport(String id, BackendRepository backend, String username, String password, AuthenticationManager authenticationManager) {
        this.id = id;
        this.username = username;
        this.password = password;
        this.backendRepository = backend;
        this.authenticationManager = authenticationManager;
        notifyListenersAsync(r -> r.onInitialization(getId()));
    }

    /**
     * {@inheritDoc}
     */
    public void createSchema() {
        if (hasAnyPermission(AuthenticationManager.FF4J_PERMISSION_CREATE_SCHEMA)) {
            backendRepository.createSchema();
            notifyListenersAsync(r -> r.onCreateSchema(getId()));
        }
    }

    /**
     * {@inheritDoc}
     */
    public FF4jConfiguration readConfiguration() {
        // TODO RBAC
        return backendRepository.readConfiguration();
    }

    /**
     * {@inheritDoc}
     */
    public void saveConfiguration(FF4jConfiguration config) {
        // TODO RBAC
        backendRepository.saveConfiguration(config);
        notifyListenersAsync(r -> r.onSaveConfiguration(config));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createWorkspace(String workspace) {
        backendRepository.createWorkspace(workspace);
    }

    @Override
    public void deleteWorkspace(String workspace) {

    }

    @Override
    public void createFeature(String workspace, Feature flag) throws FeatureFlagAlreadyExistException {

    }

    @Override
    public void saveFeature(String workspace, Feature flag) throws FeatureFlagNotFoundException {

    }

    @Override
    public void deleteFeature(String workspace, String uid) throws FeatureFlagNotFoundException {

    }

    @Override
    public void renameFeature(String workspace, String old, String uid) throws FeatureFlagNotFoundException {

    }

    @Override
    public void createProperty(String workspace, Property<?> property) throws PropertyAlreadyExistException {

    }

    @Override
    public void saveProperty(String workspace, Property<?> property) {

    }

    @Override
    public void deleteProperty(String workspace, String uid) {

    }

    @Override
    public void renameProperty(String workspace, String old, String uid) {

    }


    /**
     * Sent a notification asynchronously to the listener.
     *
     * @param lambda
     *         current lambda
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

    @Override
    public AuthenticationManager getAuthenticationManager() {
        return null;
    }

    @Override
    public BackendRepository getBackendRepository() {
        return null;
    }

    @Override
    public Stream<BackendRepositoryListener> getListeners() {
        return null;
    }

    /**
     * Update credentials to interact with authManager.
     *
     * @param username
     *         username
     * @param password
     *         password
     */
    public void setCredentials(String username, String password) {
        this.username = username;
        this.password = password;
    }

    /**
     * Test Feature (toggle On / Off).
     *
     * @param namespace
     *         namespace identifier
     * @param uid
     *         feature unique identifier
     * @return if feature should be toggled
     */
    public boolean test(String namespace, String uid) {
        return findFeature(namespace, uid)
                .orElseThrow(() -> new FeatureFlagNotFoundException(namespace, uid))
                .getValue(BackendSupport.getContext()) && isAuthorized(namespace, uid);
    }

    /**
     * Current authenticated user is allowed on the feature or anonymous.
     *
     * @param namespace
     *         current namespace
     * @param uid
     *         current flag
     * @return if user is allowed.
     */
    private boolean isAuthorized(String namespace, String uid) {
        // Feature exist
        return true;
    }

    /**
     * Check Permissions.
     *
     * @param permissions
     *         permission
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

    @Override
    public Stream<String> getWorkspaces() {
        return null;
    }

    @Override
    public Stream<Feature> getFeatures(String workspace) {
        return null;
    }

    @Override
    public Stream<String> getFeaturesNames(String workspace) {
        return null;
    }

    @Override
    public Optional<Feature> findFeature(String workspace, String uid) {
        return Optional.empty();
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

    @Override
    public Stream<String> getPropertiesNames(String workspace) {
        return null;
    }
}
