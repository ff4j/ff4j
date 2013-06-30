package org.ff4j;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.ff4j.security.AuthorizationsManager;
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
public class Flipper {
	
	/** Logger for Advisor. */
	final static Logger logger = LoggerFactory.getLogger(Flipper.class);
	
	/** Store will handle feature. */
	private static FeatureStore backingStore = null;
	
	/** security policy. */
	private static AuthorizationsManager authorizationsManager = null;
	
	public Flipper() {
	}
	
	public Flipper(FeatureStore fs) {
		initStore(fs);
	}
	
	public Flipper(FeatureStore fs, AuthorizationsManager secu) {
		initStore(fs);
		setAuthorizationsManager(secu);
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
		Feature fp 		= getStore().read(featureID);
		boolean flipped = fp.isEnable();

		// If authorization manager provided, apply security filter
		if (flipped && authorizationsManager != null) {
			flipped = flipped && authorizationsManager.isAllowed(fp);
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
		Feature fp 		= getStore().read(featureID);
		boolean flipped = fp.isEnable();

		// If authorization manager provided, apply security filter
		if (flipped && authorizationsManager != null) {
			flipped = authorizationsManager.isAllowed(fp);
		}

		// If custom strategy has been defined, load
		if (flipped && strats != null) {
			flipped = strats.activate(featureID, executionContext);
		}
		return flipped;
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
	public static void enableFeature(String featureID) {
		getStore().enable(featureID);
	}
	
	/**
	 * Create new Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void createFeature(Feature fp) {
		getStore().create(fp);
	}
	
	/**
	 * Disable Feature.
	 *
	 * @param featureID
	 * 		unique feature identifier.
	 */
	public static void disableFeature(String featureID) {
		getStore().disable(featureID);
	}
	
	public static Feature getFeature(String featureId) {
		return getStore().read(featureId);
	}

	/**
	 * Log flippingPoint status (debugging purposes).
	 */
	public static void logFeatures() {
		Map < String, Feature > mapOfFeatures = getStore().readAll();
		logger.info("Listing current '{}' features states", mapOfFeatures);
		for (Entry<String, Feature> feat : mapOfFeatures.entrySet()) {
			logger.info("-> " + feat.getValue().toString());
		}
	}
	
	public static FeatureStore getStore() {
		// Defaut Backing Store
		if (backingStore == null) {
			logger.info("Initialized store to default : InMemory");
			backingStore = new InMemoryFeatureStore();
		}
		return Flipper.backingStore;
	}

	/**
	 * @param store
	 */
	public static void initStore(FeatureStore store) {
		Flipper.backingStore = store;
	}
	
		
	public static AuthorizationsManager getAuthorizationsManager() {
		return authorizationsManager;
	}
	
	/**
	 * @param store
	 */
	public static void initAuthManager(AuthorizationsManager authorizationsHandler) {
		Flipper.authorizationsManager = authorizationsHandler;
	}
	
	/**
	 * NON Static to be use by Spring Injection of Control
	 * @param fbs
	 */
	public void setAuthorizationsManager(AuthorizationsManager authorizationsHandler) {
		Flipper.initAuthManager(authorizationsHandler);
	}

	/**
	 * NON Static to be use by Spring Injection of Control
	 * @param fbs
	 */
	public void setStore(FeatureStore fbs) {
		Flipper.initStore(fbs);
	}
	
	
}
