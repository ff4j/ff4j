package org.ff4j;

import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.security.FF4jUser;

import java.util.Optional;

/**
 * Hold properties in thread local to use in toggle.
 */
public class FF4jContext extends FF4jEvaluationContext {
    
    /** Get reference to ff4j. */
    private final transient FF4j ff4j;

    // -------------------------------------
    // ---- Extra Feature for FF4j       ---
    // -------------------------------------

    /** Security context. */
    private Long authenticatedTime;

    /** Current User who signed in. */
    private FF4jUser user;

    /**
     * Initializing context.
     *
     * @param ff4j
     *           ff4j reference
     */
    public FF4jContext(FF4j ff4j) {
        this.ff4j = ff4j;
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     *
     * @return
     *       current value of 'ff4j'
     */
    public FF4j getFf4j() {
        return ff4j;
    }

    /**
     * Gets authenticatedTime
     *
     * @return value of authenticatedTime
     */
    public Optional<Long> getAuthenticatedTime() {
        return Optional.ofNullable(authenticatedTime);
    }

    /**
     * Gets user
     *
     * @return value of user
     */
    public Optional<FF4jUser> getUser() {
        return Optional.ofNullable(user);
    }
}
