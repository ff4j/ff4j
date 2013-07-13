package org.ff4j;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.store.FeatureLoader;
import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.FlippingStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Providing static method to handle features.
 * 
 * <br/>FeatureFlipper is a bean in order to be mocked.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4j {
	
	/** Logger for Advisor. */
	final static Logger logger = LoggerFactory.getLogger(FF4j.class);
	
	/** Store will handle feature. */
	private FeatureStore backingStore = null;
	
	/** security policy. */
	private AuthorizationsManager authorizationsManager = null;
	
	/** Do not through {@link FeatureNotFoundException} exception anymore but create it. */
	private boolean autocreate = false;
	
	/** Single instance as static. */
	private static FF4j instance = null;
	
	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j() {
		instance = this;
		logger.debug("Initialization through default constructor");
	}
	

	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j(String xmlFile) {
		initStore(new InMemoryFeatureStore(xmlFile));
		logger.debug("Initialization with store within constructor {}", xmlFile);
	}
	
	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j(FeatureStore fs) {
		initStore(fs);
		logger.debug("Initialization with store within constructor {}", fs.toString());
	}

	/**
	 * Allow creation through IOc even of static access.
	 */
	public FF4j(FeatureStore fs, AuthorizationsManager secu) {
		initStore(fs);
		initAuthorizationManager(secu);
		logger.debug("Initialization with store & authManager within constructor {} {}", fs.toString(), secu.toString());
	}
	
	/**
	 * Synchronized getInstance.
	 *
	 * @return
	 * 		single instance.
	 */
	private static synchronized FF4j getInstance() {
		if (instance == null) {
			instance = new FF4j();
		}
		return instance;
	}
	
	/**
	 * Static initialisation of FeatureStore.
	 *
	 * @param fs
	 * 		feature store.
	 * @return
	 * 		current FF4j context
	 */
	public static FF4j initStore(FeatureStore fs) {
		getInstance().setStore(fs);
		logger.info("Store has been initialized statically (initStore) {}", fs.toString());
		return getInstance();
	}
	
	/**
	 * Static initialization of AuthorizationManager.
	 *
	 * @param am
	 * 		authorization manager
	 * @return
	 * 		current FF4j context
	 */
	public static FF4j initAuthorizationManager(AuthorizationsManager am) {
		getInstance().setAuthorizationsManager(am);
		logger.info("AuthManager has been initialized initStore {}", am.toString());
		return getInstance();
	}
	
	
	/**
	 * Elegant way to ask for flipping.
	 *
	 * @param featureID
	 * 		feature unique identifier.
	 * @param executionContext
	 * 		current execution context
	 * @return
	 */
	public static boolean isFlipped(String featureID, Object... executionContext) {
		Feature fp = getFeature(featureID);
		boolean flipped = fp.isEnable();
		// If authorization manager provided, apply security filter
		if (flipped && getAuthorizationsManager() != null) {
			flipped = flipped && getAuthorizationsManager().isAllowed(fp);
		}

		// If custom strategy has been defined, load
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
	public static boolean isFlipped(String featureID, FlippingStrategy strats, Object... executionContext) {
		Feature fp 		= getFeature(featureID);
		boolean flipped = fp.isEnable();
		// If authorization manager provided, apply security filter
		if (flipped && getAuthorizationsManager() != null) {
			flipped = getAuthorizationsManager().isAllowed(fp);
		}

		// If custom strategy has been defined, load
		if (flipped && strats != null) {
			flipped = strats.activate(featureID, executionContext);
		}
		return flipped;
	}

	/**
	 * Activate feature autocreation.
	 *
	 * @param bool
	 * 		boolean to activate auto-activation
	 */
	public static void autoCreateFeature(boolean bool) {
		getInstance().autocreate = bool;
	}
	
	/**
	 * 
	 * @return
	 */
	public static Map < String, Boolean > getFeaturesStatus() {
		Map < String, Boolean > bools = new HashMap<String, Boolean>();
		List < Feature > listOfFlip = new ArrayList<Feature>();
		listOfFlip.addAll(getStore().readAll().values());
		for (Feature fp : listOfFlip) {
			bools.put(fp.getUid(), fp.isEnable());
		}
		return bools;
	}
	
	/**
	 * 
	 * @return
	 */
	public static Map <String, Feature> getFeatures() {
		return getStore().readAll();
	}
	
	/**
	 * Enable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static FF4j enableFeature(String featureID) {
		getStore().enable(featureID);
		return instance;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static FF4j createFeature(Feature fp) {
		getStore().create(fp);
		return instance;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static FF4j createFeature(String featureName, boolean enable, String description) {
		getStore().create(new Feature(featureName, enable, description));
		return instance;
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static FF4j createFeature(String featureName) {
		getStore().create(new Feature(featureName, false, ""));
		return instance;
	}
	
	/**
	 * Disable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static FF4j disableFeature(String featureID) {
		getStore().disable(featureID);
		return instance;
	}
	
	/**
	 * The feature will be create automatically if the boolea, autocreate is enabled.
	 *
	 * @param featureID
	 * 		target feature ID
	 * @return
	 * 		target feature.
	 */
	public static Feature getFeature(String featureID) {
		Feature fp = null;
		try {
			fp = getStore().read(featureID);
		} catch(FeatureNotFoundException fnfe) {
			if (getInstance().autocreate) {
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
	public static FF4j logFeatures() {
		Map < String, Feature > mapOfFeatures = getStore().readAll();
		logger.info("Listing current '{}' features states", mapOfFeatures);
		for (Entry<String, Feature> feat : mapOfFeatures.entrySet()) {
			logger.info("-> " + feat.getValue().toString());
		}
		return instance;
	}
	
	/**
	 * Export Feature through FF4J.
	 * 
	 * @return
	 * @throws IOException
	 */
	public static InputStream exportFeatures() throws IOException {
		return FeatureLoader.exportFeatures(getStore().readAll());
	}
	
	/**
	 * Access store as static way (single store).
	 *
	 * @return
	 * 		current store
	 */
	public static FeatureStore getStore() {
		if (getInstance().backingStore == null) {
			logger.debug("Access getStore() without initialization, create default");
			getInstance().setStore(new InMemoryFeatureStore());
		}
		return getInstance().backingStore;
	}

	/**
	 * NON Static to be use by Injection of Control.
	 *
	 * @param fbs
	 * 		target store.
	 */
	public void setStore(FeatureStore fbs) {
		getInstance().backingStore = fbs;
		logger.info("Store has been initialized with : {}", fbs.toString());
	}
	
	
	/**
	 * Access AuthorisationManager through 
	 *
	 * @return
	 */
	public static AuthorizationsManager getAuthorizationsManager() {
		return getInstance().authorizationsManager;
	}
	
	/**
	 * NON Static to be use by Spring Injection of Control
	 * @param fbs
	 */
	public void setAuthorizationsManager(AuthorizationsManager authM) {
		getInstance().authorizationsManager = authM;
	}
	
	/**
	 * Setter accessor for attribute 'autocreate'.
	 * @param autocreate
	 * 		new value for 'autocreate '
	 */
	public void setAutocreate(boolean autocreate) {
		getInstance().autocreate = autocreate;
	}

	/** {@inheritDoc} */
	public String toString() {
		return "FF4j [backingStore=" + backingStore + ", authorizationsManager=" + authorizationsManager + "]";
	}

	
	
}
