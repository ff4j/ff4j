package org.ff4j.parser;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.ff4j.feature.Feature;
import org.ff4j.property.Property;
import org.ff4j.user.FF4jRole;
import org.ff4j.user.FF4jUser;
import org.ff4j.utils.JsonUtils;

/**
 * Load settings for ff4j v2 with ACL and users management.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public final class FF4jConfigFile {

    /** Core Meta Data. **/ 
    private boolean autoCreate = false;
    
    /** Core Meta Data. **/ 
    private boolean audit = false; 
    
	/** InMemory Features parsed. */
    private Map < String, Feature > features = new LinkedHashMap<>();
    
    /** InMemory Properties parsed. */
    private Map < String, Property<?> > properties = new LinkedHashMap<>();
    
    /** InMemory Roles parsed. */
    private Map < String, FF4jRole > roles = new HashMap<>();
    
    /** InMemory USers parsed. */
    private Map < String, FF4jUser > users = new LinkedHashMap<>();
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"autoCreate\":").append(autoCreate).append(",");
        sb.append("\"audit\":").append(audit).append(",");
        sb.append("\"features\":").append(JsonUtils.mapAsJson(features)).append(",");
        sb.append("\"properties\":").append(JsonUtils.mapAsJson(properties)).append(",");
        sb.append("\"roles\":").append(JsonUtils.mapAsJson(roles)).append(",");
        sb.append("\"users\":").append(JsonUtils.mapAsJson(users)).append(",");
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Getter accessor for attribute 'roles'.
     *
     * @return
     *       current value of 'roles'
     */
    public Map<String, FF4jRole > getRoles() {
        return roles;
    }

    /**
     * Setter accessor for attribute 'roles'.
     * @param roles
     * 		new value for 'roles '
     */
    public void setRoles(Map<String, FF4jRole> roles) {
        this.roles = roles;
    }

    /**
     * Getter accessor for attribute 'users'.
     *
     * @return
     *       current value of 'users'
     */
    public Map<String, FF4jUser> getUsers() {
        return users;
    }

    /**
     * Setter accessor for attribute 'users'.
     * @param users
     * 		new value for 'users '
     */
    public void setUsers(Map<String, FF4jUser> users) {
        this.users = users;
    }
    
    /**
     * Getter accessor for attribute 'features'.
     *
     * @return
     *       current value of 'features'
     */
    public Map<String, Feature> getFeatures() {
        return features;
    }

    /**
     * Setter accessor for attribute 'features'.
     * @param features
     * 		new value for 'features '
     */
    public void setFeatures(Map<String, Feature> features) {
        this.features = features;
    }

    /**
     * Getter accessor for attribute 'properties'.
     *
     * @return
     *       current value of 'properties'
     */
    public Map<String, Property<?>> getProperties() {
        return properties;
    }

    /**
     * Setter accessor for attribute 'properties'.
     * @param properties
     * 		new value for 'properties '
     */
    public void setProperties(Map<String, Property<?>> properties) {
        this.properties = properties;
    }

    /**
     * Getter accessor for attribute 'autoCreate'.
     *
     * @return
     *       current value of 'autoCreate'
     */
    public boolean isAutoCreate() {
        return autoCreate;
    }

    /**
     * Setter accessor for attribute 'autoCreate'.
     * @param autoCreate
     * 		new value for 'autoCreate '
     */
    public void setAutoCreate(boolean autoCreate) {
        this.autoCreate = autoCreate;
    }

    /**
     * Getter accessor for attribute 'audit'.
     *
     * @return
     *       current value of 'audit'
     */
    public boolean isAudit() {
        return audit;
    }

    /**
     * Setter accessor for attribute 'audit'.
     * @param audit
     * 		new value for 'audit '
     */
    public void setAudit(boolean audit) {
        this.audit = audit;
    }  
    
}
