package org.ff4j.core;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

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
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
    
    /** Feature could be added to a region (environment, for example DEV, QA, PROD etc....) . */
    private String regionIdentifier;

    /** if not empty and @see {@link org.ff4j.security.AuthorizationsManager} provided, limit usage to this roles. */
    private Set<String> permissions = new TreeSet<String>();

    /** Custom behaviour to define if feature if enable or not e.g. A/B Testing capabilities. */
    private FlippingStrategy flippingStrategy;

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
     * 
     * 
     * @param uid
     *            
     * @param pactive
     *            initial feature state
     * @param pDescription
     *            description of feature.
     */
    
    /**
     * Simplest Constructor with Region Identifier (without security concerns)
     * 
     * @param uid
     *				unique feature name (required)
     * @param penable
     * 				initial feature state
     * @param pdescription
     * 				description of feature.
     * @param group
     * 				Group of the feature - Application name can be a group, useful in multi applications scenarios.
     * @param regionIdentifier
     * 				Region identifier can be used to identify the region for example Beta versus Prod
     */
    public Feature(final String uid, final boolean penable, final String pdescription, final String group, final String regionIdentifier) {
        this(uid, penable, pdescription,group);
        if(regionIdentifier !=null && !"".equals(regionIdentifier)){
        	this.regionIdentifier = regionIdentifier;
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
     * Constructor with limited access roles definitions and region identifier
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     * @param auths
     *            limited roles to use the feature even if enabled
     * @param regionIdentifier
     * 				Region identifier can be used to identify the region for example Beta versus Prod            
     */
    public Feature(final String uid, final boolean penable, final String pdescription, final String group, String regionIdentifier,
            final Collection<String> auths) {
        this(uid, penable, pdescription, group);
		this.regionIdentifier=regionIdentifier;
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
     * Constructor with limited access roles definitions and region identifier
     *
     * @param uid
     *            unique feature name (required)
     * @param penable
     *            initial feature state
     * @param pdescription
     *            description of feature.
     * @param auths
     *            limited roles to use the feature even if enabled
     * @param regionIdentifier
     * 				Region identifier can be used to identify the region for example Beta versus Prod            
     */
    public Feature(String uid, boolean penable,  String description, String groupName,
			String regionIdentifier, Set<String> rights) {
    	
    		this(uid,penable,description,groupName,rights);
    		this.regionIdentifier = regionIdentifier;
	}
    
    public Feature(final String uid, final boolean penable, final String pdescription, final String group,
          String regionIdentifier ,final Collection<String> auths, final FlippingStrategy strat) {
        this(uid, penable, pdescription, group, auths);
        if (strat != null) {
            this.flippingStrategy = strat;
        }
        this.regionIdentifier = regionIdentifier;
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
        
        // Flipping strategy
        json.append(",\"regionIdentifier\":");
        json.append(regionIdentifier);

        // Permissions
        json.append(",\"permissions\":");
        json.append(permissionsAsJson());
        
        // Flipping strategy
        json.append(",\"flippingStrategy\":");
        json.append(flippingStrategyAsJson());
        
        json.append("}");
        return json.toString();
    }
    
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
    public String permissionsAsJson() {
        StringBuilder json = new StringBuilder();
        if (null != permissions) {
            json.append("[");
            if (!permissions.isEmpty()) {
                boolean first = true;
                for (String auth : permissions) {
                    json.append(first ? "" : ",");
                    json.append("\"" + auth + "\"");
                    first = false;
                }
            }
            json.append("]");
        } else {
            json.append("null");
        }
        return json.toString();
    }
    
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
    public String flippingStrategyAsJson() {
        StringBuilder json = new StringBuilder();
        if (null != flippingStrategy) {
            json.append("{\"initParams\":{");
            Map<String , String> iparams = flippingStrategy.getInitParams();
            if (iparams != null && !iparams.isEmpty()) {
                boolean first = true;
                for (Entry<String, String> param : iparams.entrySet()) {
                    json.append(first ? "" : ",");
                    json.append("\"" + param.getKey() + "\":\"" + param.getValue() + "\"");
                    first = false;
                }
            }
            json.append("},\"type\":\"");
            json.append(flippingStrategy.getClass().getCanonicalName());
            json.append("\"}");
        } else {
            json.append("null");
        }
        return json.toString();
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
     * Getter accessor for attribute 'regionIdentifier'
     * @return
     */
	public String getRegionIdentifier() {
		return regionIdentifier;
	}

	/**
	 * Setter accessor for attribute 'regionIdentifier'
	 * @param regionIdentifier
	 */
	public void setRegionIdentifier(String regionIdentifier) {
		this.regionIdentifier = regionIdentifier;
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

}