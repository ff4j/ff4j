package org.ff4j;

import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.attributeAsJson;
import static org.ff4j.utils.JsonUtils.valueAsJson;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyFactory;
import org.ff4j.security.FF4jAcl;
import org.ff4j.security.RestrictedAccessObject;
import org.ff4j.test.AssertUtils;

/**
 * Superclass for FF4J objects.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class FF4jEntity<T extends FF4jEntity<?>> implements Comparable<T>, Serializable, RestrictedAccessObject {

    /** serial number. */
    private static final long serialVersionUID = -6001829116967488353L;
    
    /** formatter for creation date and last modified. */
    protected static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");
    
    /** unique identifier. */
    protected String uid;
    
    /** Permission : by Default everyOne can use the Feature. */
    protected FF4jAcl accessControlList = new FF4jAcl();
    
    /** Description of the meaning. */
    protected Optional < String > description = Optional.empty();
    
    /** Related people to contact for any relevant question. */
    protected Optional < String > owner = Optional.empty();
    
    /** Creation date if available in the store. */
    protected Optional < LocalDateTime > creationDate = Optional.empty();
    
    /** Last modified date if available in the underlying store. */
    protected Optional < LocalDateTime > lastModifiedDate = Optional.empty();

    /** Add you own attributes to a feature. */
    protected Map<String, Property<?>> properties = new HashMap<>();
    
    /**
     * Parameterized constructor.
     *
     * @param uid
     *      unique identifier
     */
    protected FF4jEntity(String uid) {
        AssertUtils.assertHasLength(uid);
        this.uid = uid;
        updateLastModifiedDate();
    }
    
    /**
     * Copy constructor.
     * 
     * @param uid
     *      unique identifier
     * @param e
     *      other entity
     */
    protected FF4jEntity(String uid, FF4jEntity<?> e) {
        this(uid);
        assertNotNull(e);
        this.accessControlList = e.getAccessControlList();
        e.getOwner().ifPresent(o -> this.owner = Optional.of(o));
        e.getDescription().ifPresent(d -> this.description = Optional.of(d));
        properties.values().stream().forEach(sp -> addProperty(PropertyFactory.createProperty(sp)));
        updateLastModifiedDate();
    }
    
    /**
     * Json common parts
     * 
     * @return
     *      json expression for the common attributes
     */
    public String baseJson() {
        StringBuilder json = new StringBuilder("\"uid\":" + valueAsJson(uid));
        json.append(attributeAsJson("clazz", getClass().getName()));
        description.ifPresent(
                d -> json.append(attributeAsJson("description", d)));
        owner.ifPresent(
                d -> json.append(attributeAsJson("owner", d)));
        creationDate.ifPresent(
                d -> json.append(attributeAsJson("creationDate", d.format(FORMATTER))));
        lastModifiedDate.ifPresent(
                d -> json.append(attributeAsJson("lastModifiedDate", d.format(FORMATTER))));
         json.append(", \"properties\":[");
         boolean first = true;
         for (Property<?> customProperty : properties.values()) {
             json.append(first ? "" : ",");
             json.append(customProperty.toJson());
             first = false;
         }
         json.append("]");
         json.append(", \"accessControlList\":" + getAccessControlList().toJson());
         return json.toString();   
    }
    
    /** {@inheritDoc} */
    @Override
    public int compareTo(T otherObject) {
        return this.uid.compareTo(otherObject.uid);
    }
    
    /**
     * Each time a feature is edited, update the last modified date.
     * @return
     */
    @SuppressWarnings("unchecked")
    public T updateLastModifiedDate() {
        setLastModified(LocalDateTime.now());
        setCreationDate(getCreationDate().orElse(getLastModifiedDate().get()));
        return (T) this;
    }
    
    // ------------------------------------------------------
    // ------------------- Getters & Setters  ---------------
    // ------------------------------------------------------
    
    // --- Description
    
    /**
     * Getter accessor for attribute 'description'.
     *
     * @return
     *       current value of 'description'
     */
    public Optional<String> getDescription() {
        return description;
    }
    
    /**
     * Accessor setter for description.
     *
     * @param description
     *      current description
     */
    public void setDescription(String description) {
        this.description = Optional.ofNullable(description);
        updateLastModifiedDate();
    }
    
    /**
     * Fluent interface for setter.
     *
     * @param description
     *      current descripton
     * @return
     *      update target description
     */
    @SuppressWarnings("unchecked")
    public T description(String description) {
        setDescription(description);
        return (T) this;
    }
    
    // --- Owner
    
    /**
     * Setter for attribut owner, update the modified date accordingly.
     *
     * @param owner
     *      current owner
     */
    public void setOwner(String owner) {
        this.owner = Optional.ofNullable(owner);
        updateLastModifiedDate();
    }
    
    /**
     * Fluent interface for owner.
     * @param owner
     * @return
     */
    @SuppressWarnings("unchecked")
    public T owner(String owner) {
        setOwner(owner);
        return (T) this;
    }
    
    /**
     * Getter accessor for attribute 'owner'.
     *
     * @return
     *       current value of 'owner'
     */
    public Optional<String> getOwner() {
        return owner;
    }
    
    // --- ACL
    
    /**
     * Getter accessor for attribute 'accessControlList'.
     *
     * @return
     *       current value of 'accessControlList'
     */
    @Override
    public FF4jAcl getAccessControlList() {
        return accessControlList;
    }

    /**
     * Setter accessor for attribute 'accessControlList'.
     * @param accessControlList
     *      new value for 'accessControlList '
     */
    public void setAccessControlList(FF4jAcl accessControlList) {
        updateLastModifiedDate();
        this.accessControlList = accessControlList;
    }
    
    @SuppressWarnings("unchecked")
    public T setCreationDate(LocalDateTime currentDate) {
        this.creationDate = Optional.of(currentDate);
        return (T) this;
    }
    
    @SuppressWarnings("unchecked")
    public T setLastModified(LocalDateTime currentDate) {
        this.lastModifiedDate = Optional.of(currentDate);
        return (T) this;
    }
    
    @SuppressWarnings("unchecked")
    public T setCustomProperties(Map<String, Property<?>> custom) {
        properties = custom;
        updateLastModifiedDate();
        return (T) this;
    }

    @SuppressWarnings("unchecked")
    public T setCustomProperties(Property<?>... properties) {
        if (properties == null) return (T) this;;
        updateLastModifiedDate();
        return setCustomProperties(Arrays.stream(properties).
                collect(Collectors.toMap(Property::getUid, Function.identity())));
    }
    
    /**
     * Add to custom properties.
     *
     * @param properties
     *      target properties to add
     * @return
     *      the new value for current eneityt
     */
    @SuppressWarnings("unchecked")
    public T addCustomProperties(Property<?>... myProperties) {
        if (myProperties != null) {
            properties.putAll(
                    Arrays.stream(myProperties).collect(
                            Collectors.toMap(Property::getUid, Function.identity())));
        }
        updateLastModifiedDate();
        return (T) this;
    }
    
    /**
     * Getter accessor for attribute 'creationDate'.
     *
     * @return
     *       current value of 'creationDate'
     */
    public Optional<LocalDateTime> getCreationDate() {
        return creationDate;
    }

    /**
     * Getter accessor for attribute 'lastModifiedDate'.
     *
     * @return
     *       current value of 'lastModifiedDate'
     */
    public Optional<LocalDateTime> getLastModifiedDate() {
        return lastModifiedDate;
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
     * Getter accessor for attribute 'customProperties'.
     *
     * @return current value of 'customProperties'
     */
    public Map< String, Property<?> > getProperties() {
        return properties;
    }
    
    /**
     * Accessor to read a custom property from Feature.
     *
     * @param propId
     *            property
     * @return property value (if exist)
     */
    public Optional<Property<?>> getProperty(String propId) {
        assertNotNull(propId);
        return Optional.ofNullable(properties.get(propId));
    }
    
    /**
     * Create new custom property.
     * 
     * @param property
     *      target property
     * @return
     *      current object
     */
    public T addProperty(Property<?> property) {
        return addCustomProperties(property);
    }

}
