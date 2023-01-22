package org.ff4j.backend;

import org.ff4j.FF4jClient;
import org.ff4j.security.AuthenticationManager;

import java.util.stream.Stream;

/**
 * Specification for the Backend Provider for FF4j.
 */
public interface Backend extends BackendRepository, FF4jClient {

    /**
     * A Backend needs to provide an identifier.
     *
     * @return
     */
    String getId();

    /**
     * Access Authentication Manager from current backend.
     *
     * @return
     *      authentication manager
     */
    AuthenticationManager getAuthenticationManager();

    /**
     * Access Persistence unit.
     *
     * @return
     *      current persistence
     */
    BackendRepository getBackendRepository();

    /**
     * Hook tracking before and after.
     *
     * @return
     */
    Stream<BackendRepositoryListener> getListeners();

    /**
     * Authentication credentials.
     *
     * @param username
     *      username
     * @param password
     *      password
     */
    void setCredentials(String username, String password);

}
