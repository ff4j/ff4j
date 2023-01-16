package org.ff4j.security;

/**
 * Permissions for the object.
 *
 * @param grantee
 *      who is targeted
 * @param right
 *      whic level enabled
 */
public record FF4jPermission(GRANTEE grantee, RIGHT right) {

    /**
     * Who
     */
    public enum GRANTEE { ROLE, USER, EVERYONE }

    /**
     * Different level
     */
    public enum RIGHT { READ, WRITE, ADMIN }

}
