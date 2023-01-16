package org.ff4j.security;

import java.security.Principal;
import java.util.HashSet;
import java.util.Set;

/**
 * Current connected user or application.
 */
public class FF4jUser implements Principal {

    /** client id (username). */
    private String username;

    /** list of permissions granted for this principal. */
    private Set<String> permissions = new HashSet<>();

    /** list of roles for this user. */
    private Set < String > roles = new HashSet<>();

    /** if user is enabled. */
    private boolean enabled;

    /** if user has expired. */
    private boolean expired;

    /** if user is locked. */
    private boolean locked;

    @Override
    public String getName() {
        return username;
    }

    /**
     * Gets permissions
     *
     * @return value of permissions
     */
    public Set<String> getPermissions() {
        return permissions;
    }

    /**
     * Set value for permissions
     *
     * @param permissions new value for permissions
     */
    public void setPermissions(Set<String> permissions) {
        this.permissions = permissions;
    }

    /**
     * Gets enabled
     *
     * @return value of enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Set value for enabled
     *
     * @param enabled new value for enabled
     */
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets expired
     *
     * @return value of expired
     */
    public boolean isExpired() {
        return expired;
    }

    /**
     * Set value for expired
     *
     * @param expired new value for expired
     */
    public void setExpired(boolean expired) {
        this.expired = expired;
    }

    /**
     * Gets locked
     *
     * @return value of locked
     */
    public boolean isLocked() {
        return locked;
    }

    /**
     * Set value for locked
     *
     * @param locked new value for locked
     */
    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    /**
     * Gets roles
     *
     * @return value of roles
     */
    public Set<String> getRoles() {
        return roles;
    }

    /**
     * Set value for roles
     *
     * @param roles new value for roles
     */
    public void setRoles(Set<String> roles) {
        this.roles = roles;
    }
}
