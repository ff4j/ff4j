package org.ff4j;

/*
 * #%L FF4j.java (ff4j-core) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureLoader;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.FlippingStrategy;

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
 * FF4J.doSomething().doSomething(). and so on.
 * </code>
 * 
 * </ul>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4j {

    /** Storage to persist feature within {@link FeatureStore}. */
    private FeatureStore store = null;

    /** Security policy to limit access through ACL with {@link AuthorizationsManager}. */
    private AuthorizationsManager authorizationsManager = null;

    /** Do not through {@link FeatureNotFoundException} exception and but feature is required. */
    private boolean autocreate = false;

    /**
     * Default constructor to allows instanciation through IoC. The created store is an empty {@link InMemoryFeatureStore}.
     */
    public FF4j() {
        this(new InMemoryFeatureStore());
    }

    /**
     * Constructor initializing ff4j with an InMemoryStore
     */
    public FF4j(String xmlFile) {
        this(new InMemoryFeatureStore(xmlFile));
    }

    /**
     * Allow creation through IOc even of static access.
     */
    private FF4j(FeatureStore fs) {
        this.store = fs;
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
    public boolean isFlipped(String featureID, Object... executionContext) {
        Feature fp = getFeature(featureID);
        boolean flipped = fp.isEnable();
        // If authorization manager provided, apply security filter
        if (flipped && getAuthorizationsManager() != null) {
            flipped = flipped && isAllowed(fp);
        }
        // If custom strategy has been defined, delegate flipping to
        if (flipped && fp.getFlippingStrategy() != null) {
            flipped = flipped && fp.getFlippingStrategy().activate(featureID, getStore(), executionContext);
        }
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
    public boolean isFlipped(String featureID, FlippingStrategy strats, Object... executionContext) {
        Feature fp = getFeature(featureID);
        boolean flipped = fp.isEnable() && isAllowed(fp);
        // If custom strategy has been defined, load
        if (flipped && strats != null) {
            flipped = strats.activate(featureID, getStore(), executionContext);
        }
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
        Set<String> userRoles = getAuthorizationsManager().getAuthenticatedUserRoles();
        for (String expectedRole : featureName.getAuthorizations()) {
            if (userRoles.contains(expectedRole)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 
     * @return
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
        return this;
    }

    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j create(String featureName, boolean enable, String description) {
        getStore().create(new Feature(featureName, enable, description));
        return this;
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
        return FeatureLoader.exportFeatures(getStore().readAll());
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

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return String.format("FF4j [backingStore=%s, authorizationsManager=%s]", store, authorizationsManager);
    }

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

}
