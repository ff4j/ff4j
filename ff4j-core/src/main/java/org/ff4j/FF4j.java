package org.ff4j;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OFF;
import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import org.ff4j.audit.EventBuilder;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.proxy.FeatureStoreAuditProxy;
import org.ff4j.audit.proxy.PropertyStoreAuditProxy;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.conf.FF4jConfiguration;
import org.ff4j.conf.FF4jConfigurationParser;
import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.InMemoryFeatureStore;

/**
 * Principal class stands as public api to work with FF4J.
 *
 * <ul>
 * <p>
 * <li>
 * It embedded a {@link FeatureStore} to record features status. By default, features are stored into memory but you would like
 * to persist them in an external storage (as database) and choose among implementations available in different modules (jdbc,
 * http...).
 * </p>
 * 
 * <p>
 * <li>It embed a {@link AuthorizationsManager} to add permissions and limit usage of features to granted people. FF4J does not
 * created roles, it's rely on external security provider as SpringSecurity Apache Shiro.
 * </p>
 * 
 * <p>
 * <li>It embed a {@link EventRepository} to monitoring actions performed on features.
 * </p>
 * 
 * </ul>
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FF4j {
    
    /** Initialization. */
    private final long startTime = System.currentTimeMillis();

    /** Version of ff4j. */
    private String version = getClass().getPackage().getImplementationVersion();
    
    /** Source of initialization (JAVA_API, WEBAPI, SSH, CONSOLE...). */
    protected String source = SOURCE_JAVA;
    
    // -- Stores --

    /** Storage to persist feature within {@link FeatureStore}. */
    private FeatureStore featureStore = new InMemoryFeatureStore();
    
    /** Storage to persist properties within {@link PropertyStore}. */
    private PropertyStore pStore = new InMemoryPropertyStore();
    
    /** Do not through {@link FeatureNotFoundException} exception but feature is required. */
    private boolean autocreate = false;
   
    /** Security policy to limit access through ACL with {@link AuthorizationsManager}. */
    private AuthorizationsManager authorizationsManager = null;

    // -- Audit --
    
    /** Capture information relative to audit. */
    private boolean enableAudit = false;
   
    /** Repository for audit event. */
    private EventRepository eventRepository = new InMemoryEventRepository();

    /** Event Publisher (thread pool, executor) to send data into {@link EventRepository} */
    private EventPublisher eventPublisher = null;
   
    /** This attribute indicates to stop the event publisher. */
    private volatile boolean shutdownEventPublisher;

    // -- Settings --
    
    /** Post Processing like audit enable. */
    private boolean initialized = false;

    /** Hold flipping execution context as Thread-safe data. */
    private ThreadLocal<FlippingExecutionContext> flippingExecutionContext = new ThreadLocal<>();
    
    /**
     * This attribute indicates when call the alter bean throw de {@link InvocationTargetException}
     * or the wrapped exception thrown by an invoked method or constructor
     */
    private boolean alterBeanThrowInvocationTargetException = true;
        
    /**
     * Default constructor to allows instantiation through IoC. The created store is an empty {@link InMemoryFeatureStore}.
     */
    public FF4j() {
        this.version = getClass().getPackage().getImplementationVersion();
        if (null == version) {
            this.version = "1.8.x";
        }
    }

    /**
     * @deprecated FF4j now read not only XML but also Yaml of JSON, Please provide
     * explicitly the Configuration
     * 
     * @code new FF4j(new XmlParser(),"ff4j.xml");
     * @code new FF4j(new YamlParser(),"ff4j.xml"); (need config lib)
     */
    public FF4j(String xmlFile) {
        this(new XmlParser(), xmlFile);
    }
    
    /**
     * @deprecated FF4j now read not only XML but also Yaml of JSON
     * strategy the parser implementation
     * @code new FF4j(new XmlParser(),"ff4j.xml");
     * @code new FF4j(new YamlParser(),"ff4j.xml"); (need config lib)
     */
    public FF4j(InputStream ins) {
        this(new XmlParser(), ins);
    }
    
    public FF4j(FF4jConfiguration config) {
        this();
        loadConfiguration(config);
    }
    
    public FF4j(FF4jConfigurationParser<?> configParser, String classPathFile) {
        loadConfiguration(configParser, classPathFile);
    }
    
    public FF4j(FF4jConfigurationParser<?> configParser, InputStream in) {
        loadConfiguration(configParser, in);
    }
    
    protected void loadConfiguration(
            FF4jConfigurationParser<?> parser, 
            String classPathFile) {
        if (null == classPathFile) {
            throw new IllegalArgumentException("Invalid file");
        }
        loadConfiguration(parser, 
                FF4j.class.getClassLoader().getResourceAsStream(classPathFile));
    }
    
    protected void loadConfiguration(
            FF4jConfigurationParser<?> parser, 
            InputStream xmlFileResourceAsStream) {
        if (xmlFileResourceAsStream == null) {
            throw new IllegalArgumentException("Cannot parse feature stream");
        }
        if (null == parser) {
            throw new IllegalArgumentException("Parser should not be null");
        }
        this.loadConfiguration(parser.parseConfigurationFile(xmlFileResourceAsStream));
    }
    
    protected void loadConfiguration(FF4jConfiguration config) {
        this.featureStore = new InMemoryFeatureStore(config);
        this.pStore = new InMemoryPropertyStore(config);
    }

    /**
     * Ask if flipped.
     * 
     * @param featureID
     *            feature unique identifier.
     * @return current feature status
     */
    public boolean check(String featureID) {
        return check(featureID, flippingExecutionContext.get());
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
            flipped = isAllowed(fp);
        }

        // If custom strategy has been defined, delegate flipping to
        if (flipped && fp.getFlippingStrategy() != null) {
            flipped = fp.getFlippingStrategy().evaluate(featureID, getFeatureStore(), executionContext);
        }
        
        // Update current context
        flippingExecutionContext.set(executionContext);
        
        // Any access is logged into audit system
        publishCheck(featureID, flipped);

        return flipped;
    }
    
    /**
     * Send target event to audit if expected.
     *
     * @param uid
     *      feature unique identifier
     * @param checked
     *      if the feature is checked or not
     */
    private void publishCheck(String uid, boolean checked) {
        if (isEnableAudit()) {
            getEventPublisher().publish(new EventBuilder(this)
                        .feature(uid)
                        .action(checked ? ACTION_CHECK_OK : ACTION_CHECK_OFF)
                        .build());
        }
    }

    /**
     * Overriding strategy on feature.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param flippingStrategy
     *            flipping strategy
     * @return
     *         check strategy
     */
    public boolean checkOverridingStrategy(String featureID, FlippingStrategy flippingStrategy) {
        return checkOverridingStrategy(featureID, flippingStrategy, flippingExecutionContext.get());
    }

    /**
     * Overriding strategy on feature.
     * 
     * @param featureID
     *            feature unique identifier.
     * @param strategy
     *             flipping strategy
     * @param executionContext
     *            current execution context
     * @return
     *      check strategy
     */
    public boolean checkOverridingStrategy(String featureID, FlippingStrategy strategy, FlippingExecutionContext executionContext) {
        Feature fp = getFeature(featureID);
        boolean flipped = fp.isEnable() && isAllowed(fp);
        if (strategy != null) {
            flipped = flipped && strategy.evaluate(featureID, getFeatureStore(), executionContext);
        }
        publishCheck(featureID, flipped);
        return flipped;
    }

    /**
     * Load SecurityProvider roles (e.g : SpringSecurity GrantedAuthorities)
     * 
     * @param feature
     *            target name of the feature
     * @return if the feature is allowed
     */
    public boolean isAllowed(Feature feature) {
        // No authorization manager, returning always true
        return (getAuthorizationsManager() == null) || 
        // if no permissions, the feature is public
        (feature.getPermissions().isEmpty()) ||
        // delegating evaluation to authorization manager
        // --> Fixing issues with retains modify incoming list
        // getAuthorizationsManager().isAllowed(feature.getPermissions());
        // <--
        getAuthorizationsManager().isAllowed(new HashSet<>(feature.getPermissions()));
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
    public Map < String, Property<?>> getProperties() {
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
        } catch (FeatureNotFoundException exception) {
            if (this.autocreate) {
                synchronized (this) {
                    if(!getFeatureStore().exist(featureID)){
                        getFeatureStore().create(new Feature(featureID, true));
                    }
                }
            } else {
            	throw exception;
            }
        }
        return this;
    }

    /**
     * Enable group.
     * 
     * @param groupName
     *            target groupName
     * @return current instance
     */
    public FF4j enableGroup(String groupName) {
        getFeatureStore().enableGroup(groupName);
        return this;
    }

    /**
     * Disable group.
     * 
     * @param groupName
     *            target groupName
     * @return current instance
     */
    public FF4j disableGroup(String groupName) {
        getFeatureStore().disableGroup(groupName);
        return this;
    }
    
    /**
     * Create new Feature.
     * 
     * @param fp
     *            unique feature identifier.
     */
    public FF4j createFeature(Feature fp) {
        getFeatureStore().create(fp);
        return this;
    }
    
    
    /**
     * Create new Property.
     * 
     * @param prop
     *          property bean
     */
    public FF4j createProperty(Property<?> prop) {
        getPropertiesStore().createProperty(prop);
        return this;
    }
    
    /**
     * Create new Feature.
     * 
     * @param featureName
     *       unique feature featureName
     * @param enable
     *      code if the feature should be enabled or not
     * @param description
     *      description
     */
    public FF4j createFeature(String featureName, boolean enable, String description) {
        return createFeature(new Feature(featureName, enable, description));
    }
    
    /**
     * Create new Feature.
     * 
     * @param featureName
     *            unique feature identifier.
     * @param enable
     *      code if the feature should be enabled or not
     */
    public FF4j createFeature(String featureName, boolean enable) {
        return createFeature(featureName, enable, "");
    }
    
    /**
     * Create new Feature.
     * 
     * @param featureName
     *            unique feature identifier.
     */
    public FF4j createFeature(String featureName) {
        return createFeature(featureName, false, "");
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
        } catch (FeatureNotFoundException exception) {
        	 if (this.autocreate) {
                 synchronized (this) {
                     if(!getFeatureStore().exist(featureID)){
                         getFeatureStore().create(new Feature(featureID, false));
                     }
                 }
             } else {
             	throw exception;
             }
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
        return getFeatureStore().exist(featureId);
    }

    /**
     * Check if target property exist.
     *
     * @param propertyName
     *           unique property identifier.
     * @return flag to check existence of
     */
    public boolean existProperty(String propertyName) {
        return getPropertiesStore().existProperty(propertyName);
    }

    /**
     * Check if any feature with the target group exist.
     *
     * @param groupName
     *           group identifier.
     * @return flag to check existence of
     */
    public boolean existGroup(String groupName) {
        return getFeatureStore().existGroup(groupName);
    }

    /**
     * The feature will be created automatically if the boolean, autocreate is enabled.
     * 
     * @param featureID
     *            target feature ID
     * @return target feature.
     */
    public Feature getFeature(String featureID) {
        Feature fp;
        try {
            fp = getFeatureStore().read(featureID);
        } catch (FeatureNotFoundException exception) {
            if (this.autocreate) {
                synchronized (this) {
                    if(!getFeatureStore().exist(featureID)){
                        fp = new Feature(featureID, false);
                        getFeatureStore().create(fp);
                    }
                    else{
                        fp = getFeatureStore().read(featureID);
                    }
                }
            } else {
                throw exception;
            }
        }
        return fp;
    }
    
    /**
     * Read property in Store
     * 
     * @param propertyName
     *           property name
     * @return target feature.
     */
    public Property<?> getProperty(String propertyName) {
        return getPropertiesStore().readProperty(propertyName);
    }

    /**
     * Read features of a group in Store
     *
     * @param groupName
     *            target group name
     * @return target features
     */
    public Map<String, Feature> getFeaturesByGroup(String groupName) {
        return getFeatureStore().readGroup(groupName);
    }

    /**
     * Read property in Store
     * 
     * @param propertyName
     *            target property name
     * @return target feature.
     */
    public String getPropertyAsString(String propertyName) {
       return getProperty(propertyName).asString();
    }
    
    /**
     * Help to import features.
     * 
     * @param features
     *      set of features.
     * @return
     *      a reference to this object (builder pattern).
     *
     * @since 1.6
     */
    public FF4j importFeatures(Collection < Feature> features) {
        getFeatureStore().importFeatures(features);
        return this;
    }
    
    /**
     * Help to import properties.
     * 
     * @param properties
     *      set of features.
     * @return
     *      a reference to this object (builder pattern).
     *
     * @since 1.6
     */
    public FF4j importProperties(Collection < Property<?>> properties) {
        if (properties != null) {
            for (Property<?> property : properties) {
                getPropertiesStore().createProperty(property);
            }
        }
        return this;
    }

    /**
     * Export Feature through FF4J.
     * 
     * @return
     *      feature stream as XML
     * @throws IOException
     *      error when reading features
     */
    public InputStream exportFeatures() throws IOException {
        return new XmlParser().exportFeatures(getFeatureStore().readAll());
    }

    /**
     * Enable auto-creation of features when not found.
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
     * Enable auto-creation of features when not found.
     * 
     * @return current instance
     */
    public FF4j autoCreate() {
        return autoCreate(true);
    }
    
    /**
     * Enable auditing of features when not found.
     * 
     * @return current instance
     */
    public FF4j audit() {
        return audit(true);
    }
            
    /**
     * Enable auditing of features when not found.
     * 
     * @return current instance
     */
    public FF4j audit(boolean val) {
         setEnableAudit(val);
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
        return this;
    }
    
    /**
     * Delete new Property.
     * 
     * @param propertyName
     *            unique property identifier.
     */
    public FF4j deleteProperty(String propertyName) {
        getPropertiesStore().deleteProperty(propertyName);
        return this;
    }
    
    /**
     * Enable a cache proxy.
     * 
     * @param cm
     *      current cache manager
     * @return
     *      current ff4j bean
     */
    public FF4j cache(FF4JCacheManager cm) {
        FF4jCacheProxy cp = new FF4jCacheProxy(getFeatureStore(), getPropertiesStore(), cm);
        setFeatureStore(cp);
        setPropertiesStore(cp);
        return this;
    }
    
    /**
     * Parse configuration file.
     *
     * @param fileName
     *      target file
     * @return
     *      current configuration as XML
     */
    public XmlConfig parseXmlConfig(String fileName) {
        InputStream xmlIN = getClass().getClassLoader().getResourceAsStream(fileName);
        if (xmlIN == null) {
            throw new IllegalArgumentException("Cannot parse XML file " + fileName + " - file not found");
        }
        return new XmlParser().parseConfigurationFile(xmlIN);
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        long uptime = System.currentTimeMillis() - startTime;
        long dayNumber = uptime / (1000 * 3600 * 24L);
        uptime = uptime - dayNumber * 1000 * 3600 * 24L;
        long hourNumber = uptime / (1000 * 3600L);
        uptime = uptime - hourNumber * 1000 * 3600L;
        long minuteNumber = uptime / (1000 * 60L);
        uptime = uptime - minuteNumber * 1000 * 60L;
        long secondNumber = uptime / 1000L;
        sb.append("\"uptime\":\"");
        sb.append(dayNumber).append(" day(s) ");
        sb.append(hourNumber).append(" hours(s) ");
        sb.append(minuteNumber).append(" minute(s) ");
        sb.append(secondNumber).append(" seconds\"");
        sb.append(", \"autocreate\":").append(isAutocreate());
        sb.append(", \"version\": \"").append(version).append("\"");
        
        // Display only if not null
        if (getFeatureStore() != null) {
            sb.append(", \"featuresStore\":");
            sb.append(getFeatureStore().toString());
        }
        if (getPropertiesStore() != null) {
            sb.append(", \"propertiesStore\":");
            sb.append(getPropertiesStore().toString());
        }
        if (getEventRepository() != null) {
            sb.append(", \"eventRepository\":");
            sb.append(getEventRepository().toString());
        }
        if (getAuthorizationsManager() != null) {
            sb.append(", \"authorizationsManager\":");
            sb.append(getAuthorizationsManager().toString());
        }
        sb.append("}");
        return sb.toString();
    }

    // -------------------------------------------------------------------------
    // ------------------- GETTERS & SETTERS -----------------------------------
    // -------------------------------------------------------------------------
       
    /**
     * Non-Static to be use by Injection of Control.
     * 
     * @param fbs
     *            target store.
     */
    public void setFeatureStore(FeatureStore fbs) {
        this.featureStore = fbs;
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
     * Setter accessor for attribute 'eventPublisher'.
     *
     * @param eventPublisher
     *            new value for 'eventPublisher '
     */
    public void setEventPublisher(EventPublisher eventPublisher) {
        this.eventPublisher = eventPublisher;
    }
    
    /**
     * Initialization of background components.
     */
    private synchronized void init() {
        
        // Execution Context
        FlippingExecutionContext context = new FlippingExecutionContext();
        this.flippingExecutionContext.set(context);
        
        // Event Publisher
        if (eventPublisher == null) {
            eventPublisher = new EventPublisher(eventRepository);
            this.shutdownEventPublisher = true;
        }
        
        // Audit is enabled, stores got a proxy for auditing
        if (isEnableAudit()) {
        	
        	if (featureStore != null && !(featureStore instanceof FeatureStoreAuditProxy)) {
                this.featureStore = new FeatureStoreAuditProxy(this, featureStore);
            }
            if (pStore != null && !(pStore instanceof PropertyStoreAuditProxy)) { 
                this.pStore = new PropertyStoreAuditProxy(this, pStore);
            }
        } else {
        	
        	 // Audit is disabled but could have been enabled before... removing PROXY if relevant
        	 if (featureStore != null && featureStore instanceof FeatureStoreAuditProxy) {
        		 this.featureStore = ((FeatureStoreAuditProxy) featureStore).getTarget();
        	 }
        	 if (pStore != null && pStore instanceof PropertyStoreAuditProxy) { 
        		 this.pStore = ((PropertyStoreAuditProxy) pStore).getTarget();
             }
        }
        
        // Flag as OK
        this.initialized = true;
    }
    
    /**
     * Create tables/collections/columns in DB (if required).
     */
    public void createSchema() {
        if (null != getFeatureStore()) {
            getFeatureStore().createSchema();
        }
        if (null != getPropertiesStore()) {
            getPropertiesStore().createSchema();
        }
        if (null != getEventRepository()) {
            getEventRepository().createSchema();
        }
    }
    
    /**
     * Access store as static way (single store).
     * 
     * @return current store
     */
    public FeatureStore getFeatureStore() {
        if (!initialized) {
            init();
        }
        return featureStore;
    }
    
    /**
     * Getter accessor for attribute 'eventPublisher'.
     * 
     * @return current value of 'eventPublisher'
     */
    public EventPublisher getEventPublisher() {
        if (!initialized) { 
            init();
        }
        return eventPublisher;
    }
    
    /**
     * Getter accessor for attribute 'pStore'.
     *
     * @return
     *       current value of 'pStore'
     */
    public PropertyStore getPropertiesStore() {
        if (!initialized) {
            init();
        }
        return pStore;
    }
    
    /**
     * Initialize flipping execution context.
     *
     * @return
     *      get current context
     */
    public FlippingExecutionContext getCurrentContext() {
        if (!initialized) {
            init();
        }
        
        if (null == this.flippingExecutionContext.get()) {
            this.flippingExecutionContext.set(new FlippingExecutionContext());
        }
        return this.flippingExecutionContext.get();
    }

    /**
     * Override flipping execution context.
     *
     * @param executionContext
     *      The new current context
     */
    public void setCurrentContext(FlippingExecutionContext executionContext) {
        this.flippingExecutionContext.set(executionContext);
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
     * Setter accessor for attribute 'pStore'.
     * @param pStore
     * 		new value for 'pStore '
     */
    public void setPropertiesStore(PropertyStore pStore) {
        this.pStore = pStore;
    }
    
    /**
     * Clear context.
     */
    public void removeCurrentContext() {
        this.flippingExecutionContext.remove();
    }

    /**
     * Getter accessor for attribute 'enableAudit'.
     *
     * @return
     *       current value of 'enableAudit'
     */
    public boolean isEnableAudit() {
        return enableAudit;
    }

    /**
     * Setter accessor for attribute 'enableAudit'.
     *
     * @param enableAudit
     * 		new value for 'enableAudit '
     */
    public void setEnableAudit(boolean enableAudit) {
    	this.enableAudit = enableAudit;
    	
    	// if you disable the audit : the auditProxy must be destroyed and use targets
    	initialized = false;
    }
    
    /**
     * Required for spring namespace and 'fileName' attribute on ff4j tag.
     *
     * @param filename
     *      filename name
     */
    public void setFileName(String filename){
        System.out.println(filename);
    }

    /**
     * Setter for auth manager.
     *
     * @param manager
     *      manager value
     */
    public void setAuthManager(String manager) {
        System.out.println(manager);
    }

    /**
     * Shuts down the event publisher if we actually started it (As opposed to
     * having it dependency-injected).
     */
    public void stop() {
        if (this.eventPublisher != null && this.shutdownEventPublisher) {
            this.eventPublisher.stop();
        }
    }

    /**
     * Getter accessor for attribute 'source'.
     *
     * @return
     *       current value of 'source'
     */
    public String getSource() {
        return source;
    }
    
    /**
     * Reach concrete implementation of the featureStore.
     *
     * @return
     *      feature store
     */
    public FeatureStore getConcreteFeatureStore() {
        return getConcreteFeatureStore(getFeatureStore());
    }
    
    /**
     * Reach concrete implementation of the propertyStore.
     *
     * @return
     *      property store
     */
    public PropertyStore getConcretePropertyStore() {
        return getConcretePropertyStore(getPropertiesStore());
    }
    
    /**
     * try to fetch CacheProxy (cannot handle proxy CGLIB, ASM or any bytecode manipulation).
     *
     * @return
     *      cache proxy
     */
    public FF4jCacheProxy getCacheProxy() {
        FeatureStore fs = getFeatureStore();
        // Pass through audit proxy if exists
        if (fs instanceof FeatureStoreAuditProxy) {
            fs = ((FeatureStoreAuditProxy) fs).getTarget();
        }
        if (fs instanceof FF4jCacheProxy) {
            return (FF4jCacheProxy) fs;
        }
        return null;
    }
    
    /**
     * Return concrete implementation.
     *
     * @param fs
     *      current featureStore
     * @return
     *      target featureStore
     */
    private FeatureStore getConcreteFeatureStore(FeatureStore fs) {
        if (fs instanceof FeatureStoreAuditProxy) {
            return getConcreteFeatureStore(((FeatureStoreAuditProxy) fs).getTarget());
        } else if (fs instanceof FF4jCacheProxy) {
            return getConcreteFeatureStore(((FF4jCacheProxy) fs).getTargetFeatureStore());
        }
        return fs;
    }
    
    /**
     * Return concrete implementation.
     *
     * @param ps
     *      current property store
     * @return
     *      target property store
     */
    private PropertyStore getConcretePropertyStore(PropertyStore ps) {
        if (ps instanceof PropertyStoreAuditProxy) {
            return getConcretePropertyStore(((PropertyStoreAuditProxy) ps).getTarget());
        } else if (ps instanceof FF4jCacheProxy) {
            return getConcretePropertyStore(((FF4jCacheProxy) ps).getTargetPropertyStore());
        }
        return ps;
    }
    
    /**
     * Enable Alter bean Throw InvocationTargetException, when enabled
     * the alter bean method always throw {@link InvocationTargetException}
     */
    public void enableAlterBeanThrowInvocationTargetException() {
        this.alterBeanThrowInvocationTargetException = true;
    }

    /**
     * Disable Alter bean Throw InvocationTargetException, when disabled
     * the alter bean method always throw the exception cause.
     */
    public void disableAlterBeanThrowInvocationTargetException() {
        this.alterBeanThrowInvocationTargetException = false;
    }

    /**
     * Getter accessor for attribute 'alterBeanThrowInvocationTargetException'.
     *
     * @return
     *       current value of 'alterBeanThrowInvocationTargetException'
     */
    public boolean isAlterBeanThrowInvocationTargetException() {
        return alterBeanThrowInvocationTargetException;
    }

    /**
     * Setter accessor for attribute 'flippingExecutionContext'.
     *
     * @param flippingExecutionContext
     * 		new value for 'flippingExecutionContext '
     */
    public void setFlippingExecutionContext(ThreadLocal<FlippingExecutionContext> flippingExecutionContext) {
        this.flippingExecutionContext = flippingExecutionContext;
    }
    
}
