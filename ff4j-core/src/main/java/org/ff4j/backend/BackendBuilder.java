package org.ff4j.backend;

import org.ff4j.security.AuthenticationManager;
import org.ff4j.utils.Assert;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Builder to create a backend.
 */
public class BackendBuilder {

    /** Instance id. */
    protected String id = UUID.randomUUID().toString();

    /** Client identifier. */
    protected String username;

    /** Client secret. */
    protected String password;

    /** Accessing Data (delegate). */
    protected BackendRepository backendRepository;

    /** Health Check. */
    protected AuthenticationManager authenticationManager;

    /** Listener on a repository. */
    protected Map<String, BackendRepositoryListener> repositoryListeners = new ConcurrentHashMap<>();

    /**
     * Default constructor
     */
    protected BackendBuilder() {}

    /**
     * Builder.
     *
     * @param id
     *      property to update
     * @return
     *      current reference
     */
    public BackendBuilder withId(String id) {
        Assert.assertHasLength(id);
        this.id = id;
        return this;
    }

    /**
     * Builder.
     *
     * @param username
     *      user identifier
     * @param password
     *      user password
     * @return
     *      current reference
     */
    public BackendBuilder withCredentials(String username, String password) {
        Assert.assertHasLength(username);
        Assert.assertHasLength(password);
        this.username = username;
        this.password = password;
        return this;
    }

    /**
     * Builder.
     *
     * @param repo
     *      property to update
     * @return
     *      current reference
     */
    public BackendBuilder withPersistence(BackendRepository repo) {
        Assert.assertNotNull(repo);
        this.backendRepository = repo;
        return this;
    }

    /**
     * Builder.
     *
     * @param auth
     *      property to update
     * @return
     *      current reference
     */
    public BackendBuilder withAuthenticationManager(AuthenticationManager auth) {
        Assert.assertNotNull(auth);
        this.authenticationManager = auth;
        return this;
    }

    /**
     * Register a listener for any action in the repo.
     *
     * @param listener
     *      current listener
     */
    public void registerListener(BackendRepositoryListener listener) {
        Assert.assertNotNull(listener);
        Assert.assertHasLength(listener.getListenerName());
        repositoryListeners.put(listener.getListenerName(), listener);
    }

    /**
     * Builder pattern.
     *
     * @return
     *      current reference
     */
    public BackendSupport build() {
        return new BackendSupport(this);
    }


}
