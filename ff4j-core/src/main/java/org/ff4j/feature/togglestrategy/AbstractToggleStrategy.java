package org.ff4j.feature.togglestrategy;

import java.io.Serializable;

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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import org.ff4j.property.Property;
import org.ff4j.test.AssertUtils;

/**
 * Super class for {@link TogglePredicate} implementation with utilities.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractToggleStrategy implements TogglePredicate,Serializable {

    /** Serial. */
    private static final long serialVersionUID = -2440547428499432159L;
    
    /** Parameters. */
    protected Map<String, Property<?>> properties = new HashMap<String, Property<?>>();
    
    /** Hold featureId. */
    protected String featureUid;
    
    /** default constructor for instrospection. */
    public AbstractToggleStrategy() {}
    
    /**
     * Post parameter parsing.
     */
    public abstract void initialize();
    
    /** {@inheritDoc} */
    @Override
    public void init(String uid, Set<Property<?>> setOfProperties) {
        AssertUtils.assertHasLength(uid);
        this.featureUid = uid;
        if (setOfProperties != null) { 
            setOfProperties.forEach(p -> properties.put(p.getUid(), p));
        }
        initialize();
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> getProperties() {
        return this.properties.values().stream();
    }
    
    /**
     * Get property based on its key.
     *
     * @param myKey
     * @return
     */
    public Optional<Property<?>> getProperty(String myKey) {
        return Optional.ofNullable(properties.get(myKey));
    }
    
    /**
     * Get property based on its key.
     *
     * @param myKey
     *          list key
     * @return
     */
    public Property<?> getRequiredProperty(String myKey) {
        if (!properties.containsKey(myKey)) {
            throw new IllegalArgumentException(String.format("Property '%s' is "
                    + "required for TogglePredicate in feature '%s'", myKey, featureUid));
        }
        return properties.get(myKey);
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /**
     * Getter accessor for attribute 'featureUid'.
     *
     * @return
     *       current value of 'featureUid'
     */
    public String getFeatureUid() {
        return featureUid;
    }
}
