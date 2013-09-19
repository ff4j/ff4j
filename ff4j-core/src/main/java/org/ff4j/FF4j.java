package org.ff4j;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.FeatureLoader;
import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.FlippingStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Main component of the framework, it allows to interact with features. It provides both static and direct access.
 * 
 * <p>It embedded a {@link FeatureStore} to record features statused. By default feature are stored into memory but you
 * would like to persist them in an external storage as database. There are different technologies for store, please check <pre>ff4j-store-*
 * components.</pre></p> 
 *
 * <p>It embedded a {@link AuthorizationsManager} to limit usage of features through a security filter.</p>
 * <br/><b>Caution :</b>FF4J does not created roles, it's rely on external security provider as SpringSecurity Apache Chiro.
 * 
 * <ul>Other conception concerns : 
 *  <li>Most of methods are static to simplify usage.
 *  <li>Most of methods return the instance to perform fluent api : <pre>FF4J.doSomething().doSomething(). and so on.</pre>
 * </ul>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4j {
	
	/** Logger for the class. */
	final static Logger logger = LoggerFactory.getLogger(FF4j.class);
	
	/** Store will handle feature. */
	private FeatureStore store = null;
	
	/** Security policy. */
	private AuthorizationsManager authorizationsManager = null;
	
	/** Do not through {@link FeatureNotFoundException} exception anymore but create it. */
	private boolean autocreate = false;
	
	/**
	 * Singleton pattern is now considered as an anti-pattern as you cannot implement 2 different behaviors in the same classpath
	 * for the same component. For example you would like to use 2 different configuration files.
	 * 
	 * Nevertheless singletons allow to propose static method and a simplier usage. In feature Flipping, in 95% of use case a Singleton
	 * is enough, but we provided both implementations to be as clean as possible.
	 **/
	private static FF4j singleton = null;
	
	
	
	/**
	 * Default constructor with no arguments. It allows instanciation through IoC.
	 */
	public FF4j() {
		if (singleton == null) singleton = this;
	}

	/**
	 * Allow creation through IoC and configuration file (if nothing defined default is <pre>ff4j.xml</pre>).
	 */
	public FF4j(String xmlFile) {
		this(new InMemoryFeatureStore(xmlFile));
	}
	
	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j(FeatureStore fs) {
		this();
		this.store = fs;
		singleton.store = fs;
		logger.debug("Initialization with store within constructor {}", fs.toString());
	}

	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j(FeatureStore fs, AuthorizationsManager authMng) {
		this(fs);
		this.authorizationsManager = authMng;
		singleton.authorizationsManager = authMng;
		logger.debug("Initialization with store & authManager within constructor {} {}", fs.toString(), authMng.toString());
	}
	
	/**
	 * Elegant way to ask for flipping.
	 *
	 * @param featureID
	 * 		feature unique identifier.
	 * @param executionContext
	 * 		current execution context
	 * @return
	 * 		current feature status
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
			flipped = flipped && fp.getFlippingStrategy().activate(featureID, executionContext);
		}
		return flipped;
	}
	
	/**
	 * Overriding strategy on feature.
	 *
	 * @param featureID
	 * 		feature unique identifier.
	 * @param executionContext
	 * 		current execution context
	 * @return
	 */
	public boolean isFlipped(String featureID, FlippingStrategy strats, Object... executionContext) {
		Feature fp = getFeature(featureID);
		boolean flipped = fp.isEnable();
		// If authorization manager provided, apply security filter
		if (flipped && getAuthorizationsManager() != null) {
			flipped = isAllowed(fp);
		}
		// If custom strategy has been defined, load
		if (flipped && strats != null) {
			flipped = strats.activate(featureID, executionContext);
		}
		return flipped;
	}
	
	/**
	 * 
	 * @param featureName
	 * @return
	 */
	public boolean isAllowed(Feature featureName) {
		// Load SecurityProvider roles (e.g : SpringSecurity GrantedAuthorities)
		if (featureName.getAuthorizations().isEmpty()) return true;
		Set<String> userRoles = getAuthorizationsManager().getAuthenticatedUserRoles();
		for (String expectedRole : featureName.getAuthorizations()) {
			if (userRoles.contains(expectedRole)) return true;
		}
		return false;
	}
	
	/**
	 * 
	 * @return
	 */
	public Map <String, Feature> getFeatures() {
		return getStore().readAll();
	}
	
	/**
	 * Enable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public FF4j enableFeature(String featureID) {
		getStore().enable(featureID);
		return this;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public FF4j createFeature(Feature fp) {
		getStore().create(fp);
		return this;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public FF4j createFeature(String featureName, boolean enable, String description) {
		getStore().create(new Feature(featureName, enable, description));
		return this;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public FF4j createFeature(String featureName) {
		getStore().create(new Feature(featureName, false, ""));
		return this;
	}
	
	/**
	 * Disable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public FF4j disableFeature(String featureID) {
		getStore().disable(featureID);
		return this;
	}
	
	/**
	 * The feature will be create automatically if the boolea, autocreate is enabled.
	 *
	 * @param featureID
	 * 		target feature ID
	 * @return
	 * 		target feature.
	 */
	public Feature getFeature(String featureID) {
		Feature fp = null;
		try {
			fp = getStore().read(featureID);
		} catch(FeatureNotFoundException fnfe) {
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
	 * Log flippingPoint status (debugging purposes).
	 */
	public FF4j logFeatures() {
		Map < String, Feature > mapOfFeatures = getStore().readAll();
		logger.info("Listing current '{}' features states", mapOfFeatures);
		for (Entry<String, Feature> feat : mapOfFeatures.entrySet()) {
			logger.info("-> " + feat.getValue().toString());
		}
		return this;
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
	
	/** {@inheritDoc} */
	public String toString() {
		return "FF4j [backingStore=" + store + ", authorizationsManager=" + authorizationsManager + "]";
	}
	
	// --- Static Access : Work only with a singleton => no impact on existing other beans (specially for InMemoryStore) 
	
	/**
	 * Singleton Pattern, access to singleton.
	 *
	 * @return
	 * 		singleton instance.
	 */
	public static synchronized FF4j getInstance() {
		if (singleton == null) {
			singleton = new FF4j();
		}
		return singleton;
	}
	
	/**
	 * Static initialisation of FeatureStore.
	 *
	 * @param fs
	 * 		feature store.
	 * @return
	 * 		current FF4j context
	 */
	public static void sInitStore(FeatureStore fs) {
		getInstance().setStore(fs);
		logger.info("Store has been initialized statically (initStore) {}", fs.toString());
	}
	
	/**
	 * Static initialization of AuthorizationManager.
	 *
	 * @param am
	 * 		authorization manager
	 * @return
	 * 		current FF4j context
	 */
	public static void sInitAuthorizationManager(AuthorizationsManager am) {
		getInstance().setAuthorizationsManager(am);
		logger.info("AuthManager has been initialized initStore {}", am.toString());
	}
	
	/**
	 * Calling the is Flipped method in a static way.
	 *
	 * @param featureID
	 * 		feature unique identifier.
	 * @param executionContext
	 * 		current execution context
	 * @return
	 */
	public static boolean sIsFlipped(String featureID, FlippingStrategy strats, Object... executionContext) {
		return getInstance().isFlipped(featureID, strats, executionContext);
	}
	
	/**
	 * Elegant way to ask for flipping.
	 *
	 * @param featureID
	 * 		feature unique identifier.
	 * @param executionContext
	 * 		current execution context
	 * @return
	 * 		current feature status
	 */
	public static boolean sIsFlipped(String featureID, Object... executionContext) {
		return getInstance().isFlipped(featureID, executionContext);
	}
	
	/**
	 * 
	 * @return
	 */
	public static Map <String, Feature> sGetFeatures() {
		return getInstance().getFeatures();
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void sCreateFeature(Feature fp) {
		getInstance().createFeature(fp);
	}
	
	/**
	 * Enable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void sEnableFeature(String featureID) {
		getInstance().enableFeature(featureID);
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void sCreateFeature(String featureName, boolean enable, String description) {
		getInstance().createFeature(featureName, enable, description);
	}

	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void sCreateFeature(String featureName) {
		getInstance().createFeature(featureName);
	}
	
	/**
	 * Disable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void sDisableFeature(String featureID) {
		getInstance().disableFeature(featureID);
	}
	
	/**
	 * The feature will be create automatically if the boolea, autocreate is enabled.
	 *
	 * @param featureID
	 * 		target feature ID
	 * @return
	 * 		target feature.
	 */
	public static Feature sGetFeature(String featureID) {
		return getInstance().getFeature(featureID);
	}
	
	/** {@inheritDoc} */
	public static boolean sIsAllowed(Feature featureName) {
		return getInstance().isAllowed(featureName);
	}

	/**
	 * Activate feature autocreation.
	 *
	 * @param bool
	 * 		boolean to activate auto-activation
	 */
	public static void sAutoCreateFeature(boolean bool) {
		getInstance().autocreate = bool;
	}
	
	/**
	 * Export Feature through FF4J.
	 * 
	 * @return
	 * @throws IOException
	 */
	public static InputStream sExportFeatures() throws IOException {
		return getInstance().exportFeatures();
	}
	
	/**
	 * Log flippingPoint status (debugging purposes).
	 */
	public static void sLogFeatures() {
		getInstance().logFeatures();
	}
	
	// -------- Accessors Getters & Setters --------------------------
	
	/**
	 * Access store as static way (single store).
	 *
	 * @return
	 * 		current store
	 */
	public FeatureStore getStore() {
		if (store == null) {
			logger.debug("Access getStore() without initialization, create default");
			this.store = new InMemoryFeatureStore();
		}
		return store;
	}

	/**
	 * NON Static to be use by Injection of Control.
	 *
	 * @param fbs
	 * 		target store.
	 */
	public void setStore(FeatureStore fbs) {
		this.store = fbs;
		logger.info("Store has been initialized with : {}", fbs.toString());
	}
	
	/**
	 * Setter accessor for attribute 'autocreate'.
	 * @param autocreate
	 * 		new value for 'autocreate '
	 */
	public void setAutocreate(boolean autocreate) {
		this.autocreate = autocreate;
	}	

	/**
	 * Getter accessor for attribute 'authorizationsManager'.
	 *
	 * @return
	 *       current value of 'authorizationsManager'
	 */
	public AuthorizationsManager getAuthorizationsManager() {
		return authorizationsManager;
	}

	/**
	 * Setter accessor for attribute 'authorizationsManager'.
	 *
	 * @param authorizationsManager
	 * 		new value for 'authorizationsManager '
	 */
	public void setAuthorizationsManager(AuthorizationsManager authorizationsManager) {
		this.authorizationsManager = authorizationsManager;
	}
	
}
