package org.ff4j.core;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;

/**
 * Represents a feature flag identified by an unique identifier.
 *
 * <p>
 * Features Flags or Features Toggle have been introduced by Martin Fowler for continuous delivery perspective. It consists of
 * enable/disable some functionalities at runtime.
 *
 * <p>
 * <b>SecurityManagement :</b> Even a feature is enabled, you can limit its usage to a group of users (for instance BETA Tester)
 * before wide over all your users.
 * </p>
 *
 * @author Cedrick Lunven (@clunven)
 */
public class Feature implements Serializable {

    /** serial of the class. */
    private static final long serialVersionUID = -1345806526991179050L;

    /** Unique Feature Identifier */
    private String uid;

    /** Status of target feature, can be enable and disable. */
    private boolean enable = false;

    /** Short description of the feature, use it for information. */
    private String description;

    /** Feature could be grouped to enable/disable the whole group. */
    private String group;

    /** if not empty and @see {@link org.ff4j.security.AuthorizationsManager} provided, limit usage to this roles. */
    private Set<String> permissions = new TreeSet<String>();

    /** Custom behaviour to define if feature if enable or not e.g. A/B Testing capabilities. */
    private FlippingStrategy flippingStrategy;
    
    /** Add you own attributes to a feature. */
    private Map < String, Property<?> > customProperties = new LinkedHashMap<String, Property<?>>();

    /**
     * Simplest constructor initializing feature to disable.
     *
     * @param uid
     *            unique feature name (required)
     */
    public Feature(final String uid) {
        this(uid, false, null);
    }

    /**
     * Simple constructor initializing feature with status enable/disable.
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     */
    public Feature(final String uid, final boolean penable) {
        this(uid, penable, null);
    }

    /**
     * Simplest Constructor (without security concerns)
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     */
    public Feature(final String uid, final boolean penable, final String pdescription) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        this.uid = uid;
        this.enable = penable;
        this.description = pdescription;
    }

    /**
     * Simplest Constructor (without security concerns)
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     */
    public Feature(final String uid, final boolean penable, final String pdescription, final String group) {
        this(uid, penable, pdescription);
        if (group != null && !"".equals(group)) {
            this.group = group;
        }
    }

    /**
     * Constructor with limited access roles definitions
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     * @param auths
     *            limited roles to use the feature even if enabled
     */
    public Feature(final String uid, final boolean penable, final String pdescription, final String group,
            final Collection<String> auths) {
        this(uid, penable, pdescription, group);
        if (auths != null && !auths.isEmpty()) {
            this.permissions = new HashSet<String>(auths);
        }
    }

    /**
     * Constructor with limited access roles definitions
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     * @param auths
     *            limited roles to use the feature even if enabled
     */
    public Feature(final String uid, final boolean penable, final String pdescription, final String group,
            final Collection<String> auths, final FlippingStrategy strat) {
        this(uid, penable, pdescription, group, auths);
        if (strat != null) {
            this.flippingStrategy = strat;
        }
    }
    
    /**
     * Copy constructor.
     *
     * @param f
     *      current feature
     */
    public Feature(final Feature f) {
        this(f.getUid(), f.isEnable(), f.getDescription(), f.getGroup());
        this.permissions.addAll(f.getPermissions());
        // Flipping Strategy
        if (f.getFlippingStrategy() != null) {
            this.flippingStrategy = MappingUtil.instanceFlippingStrategy(f.getUid(),
                    f.getFlippingStrategy().getClass().getName(), 
                    f.getFlippingStrategy().getInitParams());
        }
        // Custom Properties
        if (f.getCustomProperties() != null && !f.getCustomProperties().isEmpty()) {
            for(Property<?> p : f.getCustomProperties().values()) {
                Property<?> targetProp = PropertyFactory.createProperty(
                        p.getName(), p.getType(), p.asString(), p.getDescription(), null);
                if (p.getFixedValues() != null) {
                    for(Object o : p.getFixedValues()) {
                        targetProp.add2FixedValueFromString(o.toString());
                    }
                }
                this.getCustomProperties().put(targetProp.getName(), targetProp);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }

    /**
     * Convert Feature to JSON.
     * 
     * @return target json
     */
    public String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append("\"uid\":\"" + uid + "\"");
        json.append(",\"enable\":" + enable);
        json.append(",\"description\":");
        json.append((null == description) ? "null" : "\"" + description + "\"");
        json.append(",\"group\":");
        json.append((null == group) ? "null" : "\"" + group + "\"");
        // Permissions
        json.append(",\"permissions\":" + JsonUtils.permissionsAsJson(permissions));
        // Flipping strategy
        json.append(",\"flippingStrategy\":" + JsonUtils.flippingStrategyAsJson(flippingStrategy));
        // Custom properties
        json.append(",\"customProperties\":" + JsonUtils.customPropertiesAsJson(customProperties));
        json.append("}");
        return json.toString();
    }
    
    public static Feature fromJson(String jsonString) {
        return null;
    }
    
    /**
     * Enable target feature
     */
    public void enable() {
        this.enable = true;
    }

    /**
     * Disable target feature
     */
    public void disable() {
        this.enable = false;
    }

    /**
     * Toggle target feature (from enable to disable and vice versa)
     */
    public void toggle() {         
        this.enable = !this.enable;
    }

    /**
     * Getter accessor for attribute 'uid'.
     *
     * @return current value of 'uid'
     */
    public String getUid() {
        return uid;
    }

    /**
     * Setter accessor for attribute 'uid'.
     *
     * @param uid
     *            new value for 'uid '
     */
    public void setUid(String uid) {
        this.uid = uid;
    }

    /**
     * Getter accessor for attribute 'enable'.
     *
     * @return current value of 'enable'
     */
    public boolean isEnable() {
        return enable;
    }

    /**
     * Setter accessor for attribute 'enable'.
     *
     * @param enable
     *            new value for 'enable '
     */
    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    /**
     * Getter accessor for attribute 'description'.
     *
     * @return current value of 'description'
     */
    public String getDescription() {
        return description;
    }

    /**
     * Setter accessor for attribute 'description'.
     *
     * @param description
     *            new value for 'description '
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Getter accessor for attribute 'flippingStrategy'.
     *
     * @return current value of 'flippingStrategy'
     */
    public FlippingStrategy getFlippingStrategy() {
        return flippingStrategy;
    }

    /**
     * Setter accessor for attribute 'flippingStrategy'.
     *
     * @param flippingStrategy
     *            new value for 'flippingStrategy '
     */
    public void setFlippingStrategy(FlippingStrategy flippingStrategy) {
        this.flippingStrategy = flippingStrategy;
    }

    /**
     * Getter accessor for attribute 'group'.
     *
     * @return current value of 'group'
     */
    public String getGroup() {
        return group;
    }

    /**
     * Setter accessor for attribute 'group'.
     *
     * @param group
     *            new value for 'group '
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return current value of 'permissions'
     */
    public Set<String> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * 
     * @param permissions
     *            new value for 'permissions '
     */
    public void setPermissions(Set<String> permissions) {
        this.permissions = permissions;
    }
    
    /**
     * Accessor to read a custom property from Feature.
     *
     * @param propId
     *         property
     * @return
     *         property value (if exist)
     */
    @SuppressWarnings("unchecked")
    public <T> Property<T> getProperty(String propId) {
        Util.assertNotNull(propId);
        if (customProperties != null && customProperties.containsKey(propId)) {
          return (Property<T>) customProperties.get(propId);
        }
        throw new PropertyNotFoundException(propId);
    }

    /**
     * Utility to add a property.
     * 
     * @param props
     */
    public <T> void addProperty(Property< T > props) {
        Util.assertNotNull(props);
        if (customProperties == null) {
            customProperties = new LinkedHashMap<String, Property<?>>();
        }
        customProperties.put(props.getName(), props);
    }
    
    /**
     * Getter accessor for attribute 'customProperties'.
     *
     * @return
     *       current value of 'customProperties'
     */
    public Map<String, Property<?>> getCustomProperties() {
        return customProperties;
    }

    /**
     * Setter accessor for attribute 'customProperties'.
     * @param customProperties
     * 		new value for 'customProperties '
     */
    public void setCustomProperties(Map<String, Property<?>> customProperties) {
        this.customProperties = customProperties;
    }
    
}
