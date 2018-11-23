package org.ff4j;

import static org.ff4j.test.AssertUtils.assertInstanceOf;
import static org.ff4j.test.AssertUtils.assertNotNull;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.Date;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.ff4j.exception.ItemNotFoundException;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.user.FF4jUser;
import org.ff4j.utils.JsonUtils;


/**
 * Context holder to perform {@link TogglePredicate} evaluations but not only.
 * May be useful to evaluate security, ACL, tags
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jContext {
    
    /** Get reference to ff4j. */
    private transient FF4j ff4j;
    
    /** Current connected user. */
    private Optional < FF4jUser > currentUser = Optional.empty();
    
    /** Current Parameter Map. */
    private transient Map < String, Object > parameters;
    
    /**
     * Default Constructor.
     */
    public FF4jContext(FF4j ff4jRef) {
        this(ff4jRef, null);
    }
    
    /**
     * Initializing context.
     * 
     * @param init
     *            initialisation for parameters.
     */
    public FF4jContext(FF4j ff4jRef, Map<String, Object> init) {
        this.ff4j = ff4jRef;
        this.parameters = (init == null) ? new ConcurrentHashMap<>() : init;
    }
    
    /**
     * Default Constructor.
     *
    public FF4jContext() {
        init();
    }*/

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return JsonUtils.mapAsJson(parameters);
    }

    /**
     * Check inexistence of key within map
     * 
     * @param key
     *            target parameter key
     * @return if the parameter exist
     */
    public boolean containsKey(String key) {
        return parameters.containsKey(key);
    }
    
    /**
     * Get Raw value of the parameter map.
     * 
     * @param key
     *            current key
     * @return object if present in map
     */
    public Optional < Object > getValue(String key) {
        return Optional.ofNullable(getValue(key));
    }
    
    /**
     * Get a value from the context.
     * 
     * @param key
     *      current key
     * @param required
     *      if the parameter is required
     * @return
     *      object
     */
    public Object getValue(String key, boolean required) {
        if (required) {
            return getValue(key).orElseThrow(() -> new ItemNotFoundException(key));
        }
        return getValue(key).orElse(null);
    }
    
    /**
     * Add a value to the parameter list.
     * 
     * @param key
     *            target key
     * @param value
     *            target value
     */
    public void putValues(FF4jContext ctx) {
        if (ctx != null) {
            ctx.parameters.entrySet().stream()
                .forEach(p -> parameters.put(p.getKey(), p.getValue()));
        }
    }
    
    /**
     * Add a value to the parameter list.
     * 
     * @param key
     *            target key
     * @param value
     *            target value
     */
    public void put(String key, Object obj) {
        parameters.put(key, (obj != null) ? obj : null);
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public String getString(String key, boolean required) {
        Object o = getValue(key, required);
        assertInstanceOf(o, String.class);
        return (String) o;
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Optional <Boolean > getBoolean(String key) {
        return Optional.ofNullable(this.getBoolean(key, false));
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Boolean getBoolean(String key, boolean required) {
        Object o = getValue(key, required);
        if (!(o instanceof Boolean)) {
            throw new IllegalArgumentException("Cannot convert parameter to Boolean");
        }
        return (Boolean) o;
    }


    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Optional <Integer> getInt(String key) {
        return Optional.ofNullable(this.getInt(key, false));
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Integer getInt(String key, boolean required) {
        Object o = getValue(key, required);
        if (!(o instanceof Integer)) {
            throw new IllegalArgumentException("Cannot convert parameter to Integer it");
        }
        return (Integer) o;
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Optional <Double> getDouble(String key) {
        return Optional.ofNullable(this.getDouble(key, false));
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Double getDouble(String key, boolean required) {
        Object o = getValue(key, required);
        if (!(o instanceof Double)) {
            throw new IllegalArgumentException("Cannot convert parameter to Double");
        }
        return (Double) o;
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Optional < Date > getDate(String key) {
        return Optional.ofNullable(this.getDate(key, false));
    }

    /**
     * Convenient method to get a string value.
     * 
     * @param key
     *            current key
     * @param required
     *            if value is required
     */
    public Date getDate(String key, boolean required) {
        Object o = getValue(key, required);
        if (!(o instanceof Date)) {
            throw new IllegalArgumentException("Cannot convert parameter to Date");
        }
        return (Date) o;
    }

    /**
     * Default get Value.
     * 
     * @param key
     *            target key
     */
    public Optional <String> getString(String key) {
        assertNotNull(key);
        return Optional.ofNullable(getString(key, false));
    }

    /**
     * Getter accessor for attribute 'parameters'.
     *
     * @return
     *       current value of 'parameters'
     */
    public Map<String, Object> getParameters() {
        return parameters;
    }

    /**
     * Setter accessor for attribute 'parameters'.
     * @param parameters
     * 		new value for 'parameters '
     */
    public void setParameters(Map<String, Object> parameters) {
        this.parameters = parameters;
    }

    /**
     * Getter accessor for attribute 'currentUser'.
     *
     * @return
     *       current value of 'currentUser'
     */
    public Optional<FF4jUser> getCurrentUser() {
        return currentUser;
    }

    /**
     * Setter accessor for attribute 'currentUser'.
     * @param currentUser
     * 		new value for 'currentUser '
     */
    public void setCurrentUser(Optional<FF4jUser> currentUser) {
        this.currentUser = currentUser;
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
     * Setter accessor for attribute 'ff4j'.
     * @param ff4j
     * 		new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }
}
