package org.ff4j.security;

import org.ff4j.security.FF4jPermission;

import java.util.List;
import java.util.Map;

/**
 * Permissions for different objects in the namespace.
 */
public record NamespaceAuthorizations(

        /**
         * Namespace identifier.
         */
        String namespace,

        /**
         * Policy at namespace level.
         */
        List<FF4jPermission> nameSpacePermissions,

        /**
         * Policy for a feature.
         */
        Map<String, List<FF4jPermission>> featuresPermissions,

        /**
         * Policy for a property.
         */
        Map<String, List<FF4jPermission>> propertiesPermissions) {

}
