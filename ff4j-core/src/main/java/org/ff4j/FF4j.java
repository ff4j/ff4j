package org.ff4j;

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

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Set;

import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventRepository;
import org.ff4j.audit.EventType;
import org.ff4j.audit.InMemoryEventRepository;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FeatureXmlParser;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.InMemoryFeatureStore;

/**
 * Main component of the framework, it allows to interact with features. It provides both static and direct access.
 * 
 * <p>
 * It embedded a {@link FeatureStore} to record features statused. By default feature are stored into memory but you would like to
 * persist them in an external storage as database. There are different technologies for store, please check
 * 
 * <code>ff4j-store-* components. </code>
 * 
 * </p>
 * 
 * <p>
 * It embedded a {@link AuthorizationsManager} to limit usage of features through a security filter.
 * </p>
 * <br/>
 * <b>Caution :</b>FF4J does not created roles, it's rely on external security provider as SpringSecurity Apache Chiro.
 * 
 * <ul>
 * Other conception concerns :
 * <li>Most of methods are static to simplify usage.
 * <li>Most of methods return the instance to perform fluent api :
 * 
 * <code>
 * instance.doSomething().doSomething(). and so on.
 * </code>
 * 
 * </ul>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4j {

    /** Storage to persist feature within {@link FeatureStore}. */
    private FeatureStore store = new InMemoryFeatureStore();

    /** Security policy to limit access through ACL with {@link AuthorizationsManager}. */
    private AuthorizationsManager authorizationsManager = null;

    /** Repository for audit event. */
    private EventRepository eventRepository = new InMemoryEventRepository();

    /** Event Publisher. */
    private EventPublisher eventPublisher = null;
    
    /** Do not through {@link FeatureNotFoundException} exception and but feature is required. */
    private boolean autocreate = false;

    /** Intialisation. */
    private final long startTime = System.currentTimeMillis();

    /**
     * Default constructor to allows instanciation through IoC. The created store is an empty {@link InMemoryFeatureStore}.
     */
    public FF4j() {
    }

    /**
     * Constructor initializing ff4j with an InMemoryStore
     */
    public FF4j(String xmlFile) {
        this.store = new InMemoryFeatureStore(xmlFile);
    }

    /**
     * Constructor initializing ff4j with an InMemoryStore using an InputStream
     */
    public FF4j(InputStream xmlFileResourceAsStream) {
        this.store = new InMemoryFeatureStore(xmlFileResourceAsStream);
    }

    /**
     * Elegant way to ask for flipping.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param executionContext
     *            current execution context
     * @return current feature status
     */
    public boolean check(String featureID) {
        return check(featureID, null);
    }

    /**
     * Elegant way to ask for flipping.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param executionContext
     *            current execution context
     * @return current feature status
     */
    public boolean check(String featureID, FlippingExecutionContext executionContext) {
        Feature fp = getFeature(featureID);
        boolean flipped = fp.isEnable();

        // If authorization manager provided, apply security filter
        if (flipped && getAuthorizationsManager() != null) {
            flipped = flipped && isAllowed(fp);
        }

        // If custom strategy has been defined, delegate flipping to
        if (flipped && fp.getFlippingStrategy() != null) {
            flipped = flipped && fp.getFlippingStrategy().evaluate(featureID, getStore(), executionContext);
        }

        // Any modification done is logged into audit system
        getEventPublisher().publish(featureID, flipped);

        return flipped;
    }

    /**
     * Overriding strategy on feature.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param executionContext
     *            current execution context
     * @return
     */
    public boolean checkOveridingStrategy(String featureID, FlippingStrategy strats) {
        return checkOveridingStrategy(featureID, strats, null);
    }

    /**
     * Overriding strategy on feature.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param executionContext
     *            current execution context
     * @return
     */
    public boolean checkOveridingStrategy(String featureID, FlippingStrategy strats, FlippingExecutionContext executionContext) {
        Feature fp = getFeature(featureID);
        boolean flipped = fp.isEnable() && isAllowed(fp);
        if (strats != null) {
            flipped = flipped && strats.evaluate(featureID, getStore(), executionContext);
        }

        // Any modification done is logged into audit system
        getEventPublisher().publish(featureID, flipped);

        return flipped;
    }

    /**
     * Load SecurityProvider roles (e.g : SpringSecurity GrantedAuthorities)
     * 
     * @param featureName
     *            target name of the feature
     * @return if the feature is allowed
     */
    public boolean isAllowed(Feature featureName) {
        // No authorization manager, returning always true
        if (getAuthorizationsManager() == null) {
            return true;
        }
        Set<String> userRoles = getAuthorizationsManager().getCurrentUserPermissions();
        for (String expectedRole : featureName.getPermissions()) {
            if (userRoles.contains(expectedRole)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Read Features from store.
     * 
     * @return get store features
     */
    public Map<String, Feature> getFeatures() {
        return getStore().readAll();
    }

    /**
     * Enable Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j enable(String featureID) {
        try {
            getStore().enable(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                return create(new Feature(featureID, true));
            }
            throw fnfe;
        }
        getEventPublisher().publish(featureID, EventType.ENABLE);
        return this;
    }

    /**
     * Enable group.
     * 
     * @param groupName
     *            target groupeName
     * @return current instance
     */
    public FF4j enableGroup(String groupName) {
        getStore().enableGroup(groupName);
        getEventPublisher().publish(groupName, EventType.ENABLE_GROUP);
        return this;
    }

    /**
     * Disable group.
     * 
     * @param groupName
     *            target groupeName
     * @return current instance
     */
    public FF4j disableGroup(String groupName) {
        getStore().enableGroup(groupName);
        getEventPublisher().publish(groupName, EventType.DISABLE_GROUP);
        return this;
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(Feature fp) {
        getStore().create(fp);
        getEventPublisher().publish(fp.getUid(), EventType.CREATE);
        return this;
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(String featureName, boolean enable, String description) {
        return create(new Feature(featureName, enable, description));
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(String featureName, boolean enable) {
        return create(featureName, enable, "");
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(String featureName) {
        return create(featureName, false, "");
    }

    /**
     * Disable Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j disable(String featureID) {
        try {
            getStore().disable(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                return create(new Feature(featureID, false));
            }
            throw fnfe;
        }
        getEventPublisher().publish(featureID, EventType.DISABLE);
        return this;
    }

    /**
     * Check if target feature exist.
     * 
     * @param featureId
     *            unique feature identifier.
     * @return flag to check existence of
     */
    public boolean exist(String featureId) {
        return getStore().exist(featureId);
    }

    /**
     * The feature will be create automatically if the boolea, autocreate is enabled.
     * 
     * @param featureID
     *            target feature ID
     * @return target feature.
     */
    public Feature getFeature(String featureID) {
        Feature fp = null;
        try {
            fp = getStore().read(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                fp = new Feature(featureID, false);
                getStore().create(fp);
            } else {
                throw fnfe;
            }
        }
        return fp;
    }

    /**
     * Export Feature through FF4J.
     * 
     * @return
     * @throws IOException
     */
    public InputStream exportFeatures() throws IOException {
        return new FeatureXmlParser().exportFeatures(getStore().readAll());
    }

    /**
     * Enable autocreation of features when not found.
     * 
     * @param flag
     *            target value for autocreate flag
     * @return current instance
     */
    public FF4j autoCreate(boolean flag) {
        setAutocreate(flag);
        return this;
    }

    /**
     * Delete feature name.
     * 
     * @param fpId
     *            target feature
     */
    public FF4j delete(String fpId) {
        getStore().delete(fpId);
        getEventPublisher().publish(fpId, EventType.DELETE);
        return this;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        // Render Uptime as String "X day(s) X hours(s) X minute(s) X seconds"
        long uptime = System.currentTimeMillis() - startTime;
        long daynumber = new Double(Math.floor(uptime / (1000 * 3600 * 24))).longValue();
        uptime = uptime - (daynumber * 1000 * 3600 * 24);
        long hourNumber = new Double(Math.floor(uptime / (1000 * 3600))).longValue();
        uptime = uptime - (hourNumber * 1000 * 3600);
        long minutenumber = new Double(Math.floor(uptime / (1000 * 60))).longValue();
        uptime = uptime - (minutenumber * 1000 * 60);
        long secondnumber = new Double(Math.floor(uptime / 1000)).longValue();
        sb.append("\"uptime\":\"");
        sb.append(daynumber + " day(s) ");
        sb.append(hourNumber + " hours(s) ");
        sb.append(minutenumber + " minute(s) ");
        sb.append(secondnumber + " seconds\"");
        // <---
        sb.append(", \"autocreate\":" + isAutocreate());
        sb.append(", \"featuresStore\":");
        sb.append(getStore() == null ? "null" : getStore().toString());
        sb.append(", \"eventRepository\":");
        sb.append(getEventRepository() == null ? "null" : getEventRepository().toString());
        sb.append(", \"authorizationsManager\":");
        sb.append(getAuthorizationsManager() == null ? "null" : getAuthorizationsManager().toString());
        sb.append("}");
        return sb.toString();
    }

    // -------------------------------------------------------------------------
    // ------------------- GETTERS & SETTERS -----------------------------------
    // -------------------------------------------------------------------------

    /**
     * Access store as static way (single store).
     * 
     * @return current store
     */
    public FeatureStore getStore() {
        return store;
    }

    /**
     * NON Static to be use by Injection of Control.
     * 
     * @param fbs
     *            target store.
     */
    public void setStore(FeatureStore fbs) {
        this.store = fbs;
    }

    /**
     * Setter accessor for attribute 'autocreate'.
     * 
     * @param autocreate
     *            new value for 'autocreate '
     */
    public void setAutocreate(boolean autocreate) {
        this.autocreate = autocreate;
    }

    /**
     * Getter accessor for attribute 'authorizationsManager'.
     * 
     * @return current value of 'authorizationsManager'
     */
    public AuthorizationsManager getAuthorizationsManager() {
        return authorizationsManager;
    }

    /**
     * Setter accessor for attribute 'authorizationsManager'.
     * 
     * @param authorizationsManager
     *            new value for 'authorizationsManager '
     */
    public void setAuthorizationsManager(AuthorizationsManager authorizationsManager) {
        this.authorizationsManager = authorizationsManager;
    }

    /**
     * Getter accessor for attribute 'eventRepository'.
     * 
     * @return current value of 'eventRepository'
     */
    public EventRepository getEventRepository() {
        return eventRepository;
    }

    /**
     * Setter accessor for attribute 'eventRepository'.
     * 
     * @param eventRepository
     *            new value for 'eventRepository '
     */
    public void setEventRepository(EventRepository eventRepository) {
        this.eventRepository = eventRepository;
    }

    /**
     * Getter accessor for attribute 'eventPublisher'.
     * 
     * @return current value of 'eventPublisher'
     */
    public EventPublisher getEventPublisher() {
        if (eventPublisher == null) {
            eventPublisher = new EventPublisher(eventRepository);
        }
        return eventPublisher;
    }

    /**
     * Setter accessor for attribute 'eventPublisher'.
     * 
     * @param eventPublisher
     *            new value for 'eventPublisher '
     */
    public void setEventPublisher(EventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }

    /**
     * Getter accessor for attribute 'autocreate'.
     *
     * @return current value of 'autocreate'
     */
    public boolean isAutocreate() {
        return autocreate;
    }

}
