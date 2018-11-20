package org.ff4j.user;

import static org.ff4j.test.AssertUtils.assertNotNull;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;

import org.ff4j.FF4jEntity;
import org.ff4j.security.FF4jPermission;
import org.ff4j.utils.JsonUtils;

/**
 * Set of permission wrapper identified as role.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jRole extends FF4jEntity < FF4jRole > {

    /**
     * Set of Permissions
     */
    private static final long serialVersionUID = -1118422767770041708L;
    
    /** Extra permissions if relevant. */
    private Set < FF4jPermission > permissions = new HashSet<>();
    
    /**
     * Create a user by its userName.
     *
     * @param uid
     *      user unique identifier
     */
    public FF4jRole(String uid) {
        super(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /**
     * Grant one or multiple permission
     * @param perms
     */
    public FF4jRole grant(FF4jPermission... perms) {
        assertNotNull(perms);
        Stream.of(perms).forEach(permissions::add);
        return this;
    }
    
    /**
     * Grant one or multiple permission
     * @param perms
     */
    public FF4jRole revoke(FF4jPermission... perms) {
        assertNotNull(perms);
        Stream.of(perms).forEach(permissions::remove);
        return this;
    }
    
    /**
     * Grant one or multiple permissions.
     * 
     * @param perms
     *      list of permissions to grant
     */
    public FF4jRole grant(String... perms) {
        assertNotNull(perms);
        Stream.of(perms).map(FF4jPermission::valueOf).forEach(permissions::add);
        return this;
    }

    /**
     * Revoke one or multiple permissions.
     * 
     * @param perms
     *      list of permissions to grant
     */
    public FF4jRole revoke(String... perms) {
        assertNotNull(perms);
        Stream.of(perms).map(FF4jPermission::valueOf).forEach(permissions::remove);
        return this;
    }

    /**
     * Convert Feature to JSON.
     * 
     * @return target json
     */
    public String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append(super.baseJson());
        json.append(",\"permissions\":" + JsonUtils.collectionAsJson(permissions));
        json.append("}");
        return json.toString();
    }

}
