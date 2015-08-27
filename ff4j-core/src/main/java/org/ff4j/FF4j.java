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
import org.ff4j.audit.EventType;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.InMemoryFeatureStore;

/**
 * Main class, it allows to work with features.
 *
 * <ul>
 * <p>
 * <li>
 * It embeddes a {@link FeatureStore} to record features statused. By default, features are stored into memory but you would like
 * to persist them in an external storage (as database) and choose among implementations available in different modules (jdbc,
 * mongo, http...).
 * </p>
 * 
 * <p>
 * <li>It embeddes a {@link AuthorizationsManager} to add permissions and limit usage of features to granted people. FF4J does not
 * created roles, it's rely on external security provider as SpringSecurity Apache Chiro.
 * </p>
 * 
 * <p>
 * <li>It embeddes a {@link EventRepository} to monitoring actions performed on features.
 * </p>
 * 
 * </ul>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4j {

    /** Do not through {@link FeatureNotFoundException} exception and but feature is required. */
    private boolean autocreate = false;

    /** Intialisation. */
    private final long startTime = System.currentTimeMillis();

    /** Version of ff4j. */
    private final String version = getClass().getPackage().getImplementationVersion();
    
    /** Storage to persist feature within {@link FeatureStore}. */
    private FeatureStore fstore = new InMemoryFeatureStore();
    
    /** Storage to persist properties within {@link PropertyStore}. */
    private PropertyStore pStore = new InMemoryPropertyStore();

    /** Security policy to limit access through ACL with {@link AuthorizationsManager}. */
    private AuthorizationsManager authorizationsManager = null;

    /** Repository for audit event. */
    private EventRepository eventRepository = new InMemoryEventRepository();

    /** Event Publisher. */
    private EventPublisher eventPublisher = null;
    
    private ThreadLocal<FlippingExecutionContext> currentExecutionContext = new ThreadLocal<FlippingExecutionContext>();

    public void setFileName(String f) {}

    /**
     * Default constructor to allows instantiation through IoC. The created store is an empty {@link InMemoryFeatureStore}.
     */
    public FF4j() {
    }

    /**
     * Constructor initializing ff4j with an InMemoryStore
     */
    public FF4j(String xmlFile) {
        this.fstore = new InMemoryFeatureStore(xmlFile);
        this.pStore = new InMemoryPropertyStore(xmlFile);
    }

    /**
     * Constructor initializing ff4j with an InMemoryStore using an InputStream. Simplify integration with Android through
     * <code>Asset</code>
     */
    public FF4j(InputStream xmlFileResourceAsStream) {
        this.fstore = new InMemoryFeatureStore(xmlFileResourceAsStream);
    }

    /**
     * Ask if flipped.
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
            flipped = flipped && fp.getFlippingStrategy().evaluate(featureID, getFeatureStore(), executionContext);
        }

        // Any access is logged into audit system
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
            flipped = flipped && strats.evaluate(featureID, getFeatureStore(), executionContext);
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
        // if no permissions, the feature is public
        if (featureName.getPermissions().isEmpty()) {
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
        return getFeatureStore().readAll();
    }
    
    /**
     * Return all properties from store.
     *
     * @return
     * 		target property store.
     */
    public Map < String, AbstractProperty<?>> getProperties() {
    	return getPropertiesStore().readAllProperties();
    }

    /**
     * Enable Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j enable(String featureID) {
        try {
            getFeatureStore().enable(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                return create(new Feature(featureID, true));
            }
            throw fnfe;
        }
        getEventPublisher().publish(featureID, EventType.ENABLE_FEATURE);
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
        getFeatureStore().enableGroup(groupName);
        getEventPublisher().publish(groupName, EventType.ENABLE_FEATUREGROUP);
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
        getFeatureStore().disableGroup(groupName);
        getEventPublisher().publish(groupName, EventType.DISABLE_FEATUREGROUP);
        return this;
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(Feature fp) {
        getFeatureStore().create(fp);
        getEventPublisher().publish(fp.getUid(), EventType.CREATE_FEATURE);
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
            getFeatureStore().disable(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                return create(new Feature(featureID, false));
            }
            throw fnfe;
        }
        getEventPublisher().publish(featureID, EventType.DISABLE_FEATURE);
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
        return getFeatureStore().exist(featureId);
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
            fp = getFeatureStore().read(featureID);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autocreate) {
                fp = new Feature(featureID, false);
                getFeatureStore().create(fp);
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
        return new XmlParser().exportFeatures(getFeatureStore().readAll());
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
        getFeatureStore().delete(fpId);
        getEventPublisher().publish(fpId, EventType.DELETE_FEATURE);
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
        sb.append(", \"version\": \"" + version + "\"");
        sb.append(", \"featuresStore\":");
        sb.append(getFeatureStore() == null ? "null" : getFeatureStore().toString());
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
     * @deprecated use {@link #getFeatureStore()} instead as since 1.4 there are both Features and properties stores  
     * @return current store
     */
    @Deprecated
    public FeatureStore getStore() {
        return fstore;
    }
    
    /**
     * Access store as static way (single store).
     * 
     * @return current store
     */
    public FeatureStore getFeatureStore() {
        return fstore;
    }

    /**
     * NON Static to be use by Injection of Control.
     * 
     * @param fbs
     *            target store.
     */
    @Deprecated
    public void setStore(FeatureStore fbs) {
        this.fstore = fbs;
    }
    
    /**
     * NON Static to be use by Injection of Control.
     * 
     * @param fbs
     *            target store.
     */
    public void setFeatureStore(FeatureStore fbs) {
        this.fstore = fbs;
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
     * Getter accessor for attribute 'autocreate'.
     *
     * @return current value of 'autocreate'
     */
    public boolean isAutocreate() {
        return autocreate;
    }

    /**
     * Getter accessor for attribute 'startTime'.
     *
     * @return
     *       current value of 'startTime'
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * Getter accessor for attribute 'version'.
     *
     * @return
     *       current value of 'version'
     */
    public String getVersion() {
        return version;
    }

    /**
     * Getter accessor for attribute 'pStore'.
     *
     * @return
     *       current value of 'pStore'
     */
    public PropertyStore getPropertiesStore() {
        return pStore;
    }

    /**
     * Setter accessor for attribute 'pStore'.
     * @param pStore
     * 		new value for 'pStore '
     */
    public void setPropertiesStore(PropertyStore pStore) {
        this.pStore = pStore;
    }

    public FlippingExecutionContext getCurrentContext() {
        FlippingExecutionContext context = this.currentExecutionContext.get();
        if (context == null) {
            context = new FlippingExecutionContext();
            this.currentExecutionContext.set(context);
        }
        return context;
    }

    public void removeCurrentContext() {
        this.currentExecutionContext.remove();
    }
}
