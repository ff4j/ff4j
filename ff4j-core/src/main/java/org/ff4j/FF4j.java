package org.ff4j;

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

import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.attributeAsJson;
import static org.ff4j.utils.JsonUtils.objectAsJson;

import java.io.InputStream;
import java.util.Collection;
import java.util.Optional;
import java.util.function.Predicate;

import org.ff4j.cache.CacheManager;
import org.ff4j.cache.CacheProxyFeatures;
import org.ff4j.cache.CacheProxyProperties;
import org.ff4j.event.Event;
import org.ff4j.feature.Feature;
import org.ff4j.feature.RepositoryFeatures;
import org.ff4j.feature.ToggleStrategy;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.inmemory.repository.RepositoryAclsInMemory;
import org.ff4j.inmemory.repository.RepositoryAuditTrailInMemory;
import org.ff4j.inmemory.repository.RepositoryFeatureUsageInMemory;
import org.ff4j.inmemory.repository.RepositoryFeaturesInMemory;
import org.ff4j.inmemory.repository.RepositoryPropertiesInMemory;
import org.ff4j.inmemory.repository.RepositoryUsersInMemory;
import org.ff4j.monitoring.AbstractRepositoryFeatureUsage;
import org.ff4j.monitoring.AuditTrail;
import org.ff4j.monitoring.FeatureUsageEventListener;
import org.ff4j.monitoring.RepositoryEventFeatureUsage;
import org.ff4j.property.Property;
import org.ff4j.property.RepositoryProperties;
import org.ff4j.repository.FF4jRepositoryObserver;
import org.ff4j.security.RepositoryAccessControlLists;
import org.ff4j.security.RepositoryUsers;
import org.ff4j.security.domain.FF4jAcl;

/**
 * Main class and public api to work with FF4j. The framework manages features to implement
 * feature toggling patterns and properties for configuration management.
 * 
 * <ul>There are different storage unit to persist different assets
 *  <li>{@link RepositoryFeatures}   : CRUD operation on features 
 *  <li>{@link RepositoryProperties} : CRUD operation on properties 
 *  <li>{@link RepositoryEventFeatureUsage} : Publish and search into feature usages
 *  <li>{@link AuditTrail} : Publish and search into operations
 *  <li>
 * </ul>
 * 
 * @author Cedrick Lunven (@clunven)
 *
 * @since 2.0
 */
public class FF4j extends FF4jRepositoryObserver < FeatureUsageEventListener > implements Predicate<String> {
    
    // -------------------------------------------------------------------------
    // ------------------- META-DATA         -----------------------------------
    // -------------------------------------------------------------------------
    
    /** Top for startup in  order to compute uptime. */
    private final long startTime = System.currentTimeMillis();

    /** Version of ff4j library. */
    private final String version = getClass().getPackage().getImplementationVersion();
    
    /** Source of events defined for monitoring purpose. */
    private Event.Source source = Event.Source.JAVA_API;
   
    /** Flag to ask for automatically create the feature if not found in the store. */
    private boolean autoCreateFeatures = false;
    
    /** Hold properties related to each users. */
    private ThreadLocal < FF4jContext > context = new ThreadLocal<>();
    
    /** Permission : by Default everyOne can use the Feature. */
    protected FF4jAcl accessControlList = new FF4jAcl();

    // -------------------------------------------------------------------------
    // ---------- Repositories (feature, property,event..) ---------------------
    // -------------------------------------------------------------------------
    
    /** Storage to persist event logs. */ 
    private AuditTrail repositoryEventAudit = new RepositoryAuditTrailInMemory();
    
    /** Storage to persist feature within {@link RepositoryFeatures}. */
    private RepositoryFeatures repositoryFeatures = new RepositoryFeaturesInMemory();
   
    /** Storage to persist properties within {@link RepositoryProperties}. */
    private RepositoryProperties repositoryProperties = new RepositoryPropertiesInMemory();
    
    /** Define feature usage. */
    private AbstractRepositoryFeatureUsage repositoryEventFeaturesUsage = new RepositoryFeatureUsageInMemory();
    
    /** ReadOnly but can be extended to have full control on user (and dedicated screen in console). */
    private RepositoryUsers repositoryUsers = new RepositoryUsersInMemory();
    
    /** Storage to persist permissions for ff4j, web console, stores. */
    private RepositoryAccessControlLists repositoryAcl = new RepositoryAclsInMemory();
    
    // -------------------------------------
    // ---------- INIT ---------------------
    // -------------------------------------
    
    /**
     * Base constructor. It allows instantiation through IoC by default will initialized
     * all stores empty and working into memory.
     */
    public FF4j() {
    }
    
    /**
     * Constructor using an XML configuration file to initialized the stores. All operations
     * are performed in memory and lost on restart, useful for testing purposes mainly. 
     * 
     * You should condider to override Repositories to use external storage technology.
     *
     * @param xmlFile
     *          Xml configuration file
     */
    public FF4j(String xmlFile) {
        this.repositoryFeatures           = new RepositoryFeaturesInMemory(xmlFile);
        this.repositoryProperties         = new RepositoryPropertiesInMemory(xmlFile);
        this.repositoryUsers              = new RepositoryUsersInMemory(xmlFile);
        this.repositoryAcl                = new RepositoryAclsInMemory(xmlFile);
        // History is empty
        this.repositoryEventAudit         = new RepositoryAuditTrailInMemory();
        this.repositoryEventFeaturesUsage = new RepositoryFeatureUsageInMemory();
    }

    /**
     * Constructor using an XML configuration stream to initialized the stores. All operations
     * are performed in memory and lost on restart, useful for testing purposes mainly. 
     * 
     * @param xmlConfFileStream
     *      binary stream for XML configuration
     */
    public FF4j(InputStream xmlConfFileStream) {
        this.repositoryFeatures   = new RepositoryFeaturesInMemory(xmlConfFileStream);
        this.repositoryProperties = new RepositoryPropertiesInMemory(xmlConfFileStream);
        this.repositoryUsers      = new RepositoryUsersInMemory(xmlConfFileStream);
        this.repositoryAcl        = new RepositoryAclsInMemory(xmlConfFileStream);
    }
    
    // -------------------------------------
    // ------- FEATURE TOGGLING ------------
    // -------------------------------------

    /**
     * Evaluate if a feature is toggled based on the information in store and
     * current execution context (key/value as threadLocal).
     *
     * @param featureUid
     *          feature unique identifier.
     * @return 
     *          current feature status.
     */
    public boolean check(String featureUid) {
        return check(featureUid);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(String featureUid) {
        return check(featureUid);
    }

    /**
     * Evaluate if a feature is toggled based on the information in store and provided
     * execution context (key/value)
     * 
     * @param featureID
     *            feature unique identifier.
     * @param executionContext
     *            current execution context
     * @return current feature status
     */
    public boolean check(String uid, FF4jContext executionContext) {
        return check(uid, null, executionContext);
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
    public boolean check(String uid, ToggleStrategy strats) {
        return check(uid, strats, getContext());
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
    public boolean check(String uid, ToggleStrategy strats, FF4jContext executionContext) {
        // Read feature from store, must exist
        Feature feature = getFeature(uid);
        boolean featureToggled = false;
        if (feature.isEnable()) {
            // Pick default context or override
            FF4jContext context = (executionContext == null) ? getContext() : executionContext;
            if (strats == null) {
                featureToggled = feature.isToggled(context);
            } else {
                // Overriding the toggleStreategy of the feature
                featureToggled = strats.isToggled(feature, context);
            }
        }
        // Send information that feature will be used
        if (featureToggled) {
            this.notify((listener) -> listener.onFeatureHit(feature));
        }
        return featureToggled;
    }
    
    /**
     * Toggle on feature (even if already toggled)
     * 
     * @param uid
     *            unique feature identifier.
     */
    public FF4j toggleOn(String uid) {
        try {
            getRepositoryFeatures().toggleOn(uid);
        } catch (FeatureNotFoundException fnfe) {
            if (this.autoCreateFeatures) {
                getRepositoryFeatures().create(new Feature(uid).toggleOn());
            } else {
                throw fnfe;
            }
        }
        return this;
    }
    
    /**
     * Toggle on feature (even if not toggled)
    
     * 
     * @param uid
     *            unique feature identifier.
     */
    public FF4j toggleOff(String uid) {
        try {
            getRepositoryFeatures().toggleOff(uid);
        } catch (FeatureNotFoundException fnfe) {
             if (this.autoCreateFeatures) {
                 getRepositoryFeatures().create(new Feature(uid).toggleOff());
             } else {
                throw fnfe;
             }
        }
        return this;
    }
    
    // -------------------------------------
    // ------ CRUD Features (fluent) -------
    // -------------------------------------
    
    /**
     * Create new Feature.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j createFeature(Feature fp) {
        getRepositoryFeatures().create(fp);
        return this;
    }

    /**
     * The feature will be create automatically if the boolea, autocreate is enabled.
     * 
     * @param featureID
     *            target feature ID
     * @return target feature.
     */
    public Feature getFeature(String uid) {
        Optional <Feature > oFeature = getRepositoryFeatures().findById(uid);
        if (!oFeature.isPresent()) {
            if (autoCreateFeatures) {
                Feature autoFeature = new Feature(uid).toggleOff();
                getRepositoryFeatures().create(autoFeature);
                return autoFeature;
            }
            throw new FeatureNotFoundException(uid);
        }
        return oFeature.get();
    }
    
    /**
     * Help to import features.
     * 
     * @param features
     *      set of features.
     * @return
     *      a reference to this object (builder pattern).
     */
    public FF4j importFeatures(Collection < Feature> features) {
        getRepositoryFeatures().save(features);
        return this;
    }
    
    // -------------------------------------
    // ------ CRUD Properties (fluent) -----
    // -------------------------------------
    
    /**
     * Create new Property.
     * 
     * @param featureID
     *            unique feature identifier.
     */
    public FF4j createProperty(Property<?> prop) {
        getRepositoryProperties().create(prop);
        return this;
    }
    
    /**
     * Read property in Store
     * 
     * @param featureID
     *            target feature ID
     * @return target feature.
     */
    public Property<?> getProperty(String propertyName) {
       return getRepositoryProperties().read(propertyName);
    }
    
    /**
     * Help to import propertiess.
     * 
     * @param features
     *      set of features.
     * @return
     *      a reference to this object (builder pattern).
     */
    public FF4j importProperties(Collection < Property<?>> properties) {
        getRepositoryProperties().save(properties);
        return this;
    }

    // -------------------------
    // ------ Caching --------
    // -------------------------
    
    /**
     * Enable a cache proxy.
     * 
     * @param cm
     *      current cache manager
     * @return
     *      current ff4j bean
     */
    public FF4j withCaching(CacheManager<String, Feature> cm, CacheManager<String, Property<?>> pm) {
        withCachingFeatures(cm);
        withCachingProperties(pm);
        return this;
    }
    
    /**
     * Enable a cache for properties.
     * 
     * @param cm
     *      current cache manager
     * @return
     *      current ff4j bean
     */
    public FF4j withCachingProperties(CacheManager<String, Property<?>> pm) {
        setRepositoryProperties(new CacheProxyProperties(getRepositoryProperties(), pm));
        return this;
    }
    
    /**
     * Enable a cache for features.
     * 
     * @param cm
     *      current cache manager
     * @return
     *      current ff4j bean
     */
    public FF4j withCachingFeatures(CacheManager<String, Feature> cm) {
        setRepositoryFeatures(new CacheProxyFeatures(getRepositoryFeatures(), cm));
        return this;
    }
    
    // -------------------------
    // ------ Utilities --------
    // -------------------------
    
    /**
     * Create tables/collections/columns in DB (if required).
     */
    public void createSchema() {
    	// Features
        if (null != getRepositoryFeatures()) {
            getRepositoryFeatures().createSchema();
        }
        // Properties
        if (null != getRepositoryProperties()) {
            getRepositoryProperties().createSchema();
        }
        // AuditTrail
        if (null != getRepositoryEventAudit()) {
            getRepositoryEventAudit().createSchema();
        }
        // Audit
        if (null != getRepositoryEventAudit()) {
        	getRepositoryEventAudit().createSchema();
        }
        // Feature Usage
        if (null != getRepositoryEventAudit()) {
        	getRepositoryEventAudit().createSchema();
        }
        // Users
        
        
        // Acld
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        long uptime = System.currentTimeMillis() - startTime;
        long daynumber = uptime / (1000 * 3600 * 24L);
        uptime = uptime - daynumber * 1000 * 3600 * 24L;
        long hourNumber = uptime / (1000 * 3600L);
        uptime = uptime - hourNumber * 1000 * 3600L;
        long minutenumber = uptime / (1000 * 60L);
        uptime = uptime - minutenumber * 1000 * 60L;
        long secondnumber = uptime / 1000L;
        sb.append("\"uptime\":\"");
        sb.append(daynumber + " day(s) ");
        sb.append(hourNumber + " hours(s) ");
        sb.append(minutenumber + " minute(s) ");
        sb.append(secondnumber + " seconds\"");
        sb.append(attributeAsJson("autocreate", autoCreateFeatures));
        sb.append(attributeAsJson("source", source));
        sb.append(attributeAsJson("version", version));
        if (getRepositoryFeatures() != null) {
            sb.append(objectAsJson("featuresStore", getRepositoryFeatures().toString()));
        }
        if (getRepositoryProperties() != null) {
            sb.append(objectAsJson("propertiesStore", getRepositoryProperties().toString()));
        }
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Reach target implementation of the featureStore.
     *
     * @return
     */
    public RepositoryFeatures getTargetRepositoryFeatures() {
        RepositoryFeatures rf = getRepositoryFeatures();
        return (rf instanceof CacheProxyFeatures) ?
               ((CacheProxyFeatures) rf).getTargetFeatureStore() : rf;
    }
    
    /**
     * Reach concrete implementation of the propertyStore.
     *
     * @return
     */
    public RepositoryProperties getTargetRepositoryProperties() {
        RepositoryProperties rp = getRepositoryProperties();
        return (rp instanceof CacheProxyProperties) ?
                ((CacheProxyProperties) rp).getTargetPropertyStore() : rp;
    }
    
    /**
     * try to fetch CacheProxy (cannot handled proxy CGLIB, ASM or any bytecode manipulation).
     *
     * @return
     */
    public Optional < CacheProxyFeatures > getRepositoryFeaturesCacheProxy() {
        RepositoryFeatures fs = getRepositoryFeatures();
        CacheProxyFeatures cacheProxy = null;
        if (fs instanceof CacheProxyFeatures) {
            cacheProxy = (CacheProxyFeatures) fs;
        }
        return Optional.ofNullable(cacheProxy);
    }
    
    /**
     * Access to cache proxy if caching enabled, null otherwize
     * @return
     */
    public Optional < CacheProxyProperties> getRepositoryPropertiesCacheProxy() {
        RepositoryProperties ps = getRepositoryProperties();
        CacheProxyProperties cacheProxy = null;
        if (ps instanceof CacheProxyProperties) {
            cacheProxy = (CacheProxyProperties) ps;
        }
        return Optional.ofNullable(cacheProxy);
    }

    // -------------------------------------------------------
    // -------------------    ACL STORE      -----------------
    // -------------------------------------------------------
    
    /**
     * Getter accessor for attribute 'repositoryAcl'.
     *
     * @return
     *       current value of 'repositoryAcl'
     */
    public RepositoryAccessControlLists getRepositoryAcl() {
        return repositoryAcl;
    }

    /**
     * Setter accessor for attribute 'repositoryAcl'.
     * @param repositoryAcl
     *      new value for 'repositoryAcl '
     */
    public void setRepositoryAcl(RepositoryAccessControlLists repositoryAcl) {
        this.repositoryAcl = repositoryAcl;
    }
    
    /**
     * NON Static to be use by Injection of Control.
     * 
     * @param fbs
     *            target store.
     */
    public FF4j repositoryAcl(RepositoryAccessControlLists aclStore) {
        setRepositoryAcl(aclStore);
        return this;
    }
    
    // -------------------------------------------------------------------------
    // ------------------- FEATURE STORE     -----------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Getter accessor for attribute 'repositoryFeatures'.
     *
     * @return
     *       current value of 'repositoryFeatures'
     */
    public RepositoryFeatures getRepositoryFeatures() {
        return repositoryFeatures;
    }

    /**
     * Setter accessor for attribute 'repositoryFeatures'.
     * @param repositoryFeatures
     *      new value for 'repositoryFeatures '
     */
    public void setRepositoryFeatures(RepositoryFeatures repositoryFeatures) {
        this.repositoryFeatures = repositoryFeatures;
    }
    
    /**
     * NON Static to be use by Injection of Control.
     * 
     * @param featureStore
     *            target store.
     */
    public FF4j repositoryFeatures(RepositoryFeatures featureStore) {
        setRepositoryFeatures(featureStore);
        return this;
    }
    
    // -------------------------------------------------------------------------
    // ------------------- PROPERTY STORE    -----------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Getter accessor for attribute 'repositoryProperties'.
     *
     * @return
     *       current value of 'repositoryProperties'
     */
    public RepositoryProperties getRepositoryProperties() {
        return repositoryProperties;
    }

    /**
     * Setter accessor for attribute 'repositoryProperties'.
     * @param repositoryProperties
     *      new value for 'repositoryProperties '
     */
    public void setRepositoryProperties(RepositoryProperties repositoryProperties) {
        this.repositoryProperties = repositoryProperties;
    }
    
    /**
     * Fluent Method to init ff4j.
     *
     * @param propertyStore
     *      current ff4j proposition
     * @return
     *      current ff4j instance
     */
    public FF4j repositoryProperties(RepositoryProperties propertyStore) {
        setRepositoryProperties(propertyStore);
        return this;
    }
    
    // -------------------------------------------------------------
    // ------------------- USER STORE        -----------------------
    // -------------------------------------------------------------
    
 // -------------------------------------------------------------------------
    // ------------------- PROPERTY STORE    -----------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Getter accessor for attribute 'repositoryProperties'.
     *
     * @return
     *       current value of 'repositoryProperties'
     */
    public RepositoryUsers getRepositoryUsers() {
        return repositoryUsers;
    }

    /**
     * Setter accessor for attribute 'repositoryProperties'.
     * @param repositoryProperties
     *      new value for 'repositoryProperties '
     */
    public void setRepositoryProperties(RepositoryUsers repositoryUsers) {
        this.repositoryUsers = repositoryUsers;
    }
    
    /**
     * Fluent Method to init ff4j.
     *
     * @param propertyStore
     *      current ff4j proposition
     * @return
     *      current ff4j instance
     */
    public FF4j repositoryUsers(RepositoryUsers propertyStore) {
        setRepositoryProperties(propertyStore);
        return this;
    }
    
    // -----------------------------------------------------------
    // ------------------- AUDIT TRAILS      ---------------------
    // -----------------------------------------------------------
    
    /**
     * Getter accessor for attribute 'auditTrail'.
     *
     * @return
     *       current value of 'auditTrail'
     */
    public AuditTrail getRepositoryEventAudit() {
        return repositoryEventAudit;
    }

    /**
     * Setter accessor for attribute 'auditTrail'.
     *
     * @param auditTrail
     *      new value for 'auditTrail '
     */
    public void setRepositoryEventAudit(AuditTrail auditTrail) {
        this.repositoryEventAudit = auditTrail;
        withAudit();
    }
    
    /**
     * Update & enable audit trail information.
     *
     * @param auditTrail
     *      
     * @return
     */
    public FF4j withAudit(AuditTrail auditTrail) {
        setRepositoryEventAudit(auditTrail);
        return this;
    }
    
    /**
     * Register listener to work on audit.
     *
     * @return
     *      current ff4j instance
     */
    public FF4j withAudit() {
        assertNotNull(getRepositoryEventAudit(), "Cannot register empty audit listerner");
        getRepositoryFeatures().registerAuditListener(getRepositoryEventAudit());
        getRepositoryProperties().registerAuditListener(getRepositoryEventAudit());
        return this;
    }
    
    /**
     * Unregister listener to work on audit.
     *
     * @return
     *      current ff4j instance
     */
    public FF4j withoutAudit() {
        getRepositoryFeatures().unRegisterAuditListener();
        getRepositoryProperties().unRegisterAuditListener();
        return this;
    }
    
    // -------------------------------------------------------------------------
    // ------------------- FEATURE USAGE     -----------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Enable features tracking (enable/disable/check).
     * 
     * @param featureUsage
     * @return
     */
    public FF4j repositoryEventFeaturesUsage(AbstractRepositoryFeatureUsage featureUsage) {
        setRepositoryEventFeaturesUsage(featureUsage);
        return withFeatureUsageTracking();
    }
    
    /**
     * Register listener for feature usage.
     *
     * @return
     *      current ff4j instance
     */
    public FF4j withFeatureUsageTracking() {
        registerListener(FeatureUsageEventListener.KEY_USAGETRACKING_LISTENER, getRepositoryEventFeaturesUsage());
        return this;
    }
    
    /**
     * Unregister listener for feature usage.
     *
     * @return
     *      current ff4j instance
     */
    public FF4j withoutFeatureUsageTracking() {
        unregisterListener(FeatureUsageEventListener.KEY_USAGETRACKING_LISTENER);
        return this;
    }

    /**
     * Getter accessor for attribute 'repositoryEventFeaturesUsage'.
     *
     * @return
     *       current value of 'repositoryEventFeaturesUsage'
     */
    public AbstractRepositoryFeatureUsage getRepositoryEventFeaturesUsage() {
        return repositoryEventFeaturesUsage;
    }

    /**
     * Setter accessor for attribute 'repositoryEventFeaturesUsage'.
     * @param repositoryEventFeaturesUsage
     * 		new value for 'repositoryEventFeaturesUsage '
     */
    public void setRepositoryEventFeaturesUsage(AbstractRepositoryFeatureUsage repositoryEventFeaturesUsage) {
        this.repositoryEventFeaturesUsage = repositoryEventFeaturesUsage;
    }
    
    // -------------------------------------------------------------------------
    // ------------------- GETTERS & SETTERS -----------------------------------
    // -------------------------------------------------------------------------
     
    // --- Autocreate
    
    /**
     * Enable autocreate for features.
     *
     * @return
     *      current instance
     */
    public FF4j withFeatureAutoCreate() {
        setAutoCreateFeatures(true);
        return this;
    }
    
    /**
     * Getter accessor for attribute 'autoCreateFeatures'.
     *
     * @return
     *       current value of 'autoCreateFeatures'
     */
    public boolean isAutoCreateFeatures() {
        return autoCreateFeatures;
    }

    /**
     * Setter accessor for attribute 'autoCreateFeatures'.
     * @param autoCreateFeatures
     *      new value for 'autoCreateFeatures '
     */
    public void setAutoCreateFeatures(boolean autoCreateFeatures) {
        this.autoCreateFeatures = autoCreateFeatures;
    }
    
    // --- Context
    
    /**
     * Obtain the current <code>FF4jExecutionContext</code>.
     *
     * @return the security context (never <code>null</code>)
     */
    public FF4jContext getContext() {
        if (null == context.get()) {
            context.set(new FF4jContext(this));
        }
        return context.get();
    }
    
    /**
     * Obtain the current <code>FF4jExecutionContext</code>.
     *
     * @return the security context (never <code>null</code>)
     */
    public void setContext(FF4jContext pcontext) {
        context.set(pcontext);
    }

    /**
     * Obtain the current <code>FF4jExecutionContext</code>.
     *
     * @return the security context (never <code>null</code>)
     */
    public void add2Context(FF4jContext pcontext) {
        getContext().getParameters().putAll(pcontext.getParameters());
    }
    
    /**
     * Explicitly clears the context value from the current thread.
     */
    public void clearContext() {
        context.remove();
    }

    // --- Version
    
    /**
     * Getter accessor for attribute 'version'.
     *
     * @return
     *       current value of 'version'
     */
    public String getVersion() {
        return version;
    }   
    
    // --- Source

    /**
     * Getter accessor for attribute 'source'.
     *
     * @return
     *       current value of 'source'
     */
    public Event.Source getSource() {
        return source;
    }
    
    /**
     * Setter accessor for attribute 'source'.
     * 
     * @param source
     *      new value for 'source '
     */
    public void setSource(Event.Source source) {
        this.source = source;
    }
    
    /**
     * Fluent API to get source.
     *
     * @param source
     *      current source
     * @return
     */
    public FF4j withSource(Event.Source source) {
        setSource(source);
        return this;
    }
    
    /**
     * Required for Spring namespace XML configuration file. Setter must exists
     *
     * @param fname
     *      target name
     */
    public void setFileName(String fname)    { /** empty setter for Spring framework */ }
    
    /**
     * Required for Spring namespace XML configuration file. Setter must exists
     *
     * @param mnger
     *      target mnger
     */
    public void setAuthManager(String mnger) { /** empty setter for Spring framework */}

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
     * Dispaly uptime for ff4j.
     *
     * @return
     *      current update in millis
     */
    public long getUptime() {
        return System.currentTimeMillis() - getStartTime();
    }    
    
}
