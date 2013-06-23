package org.ff4j.store;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;

import org.ff4j.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Storing states of feature inmemory with initial values. Could be used mostly for testing purpose.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryFeatureStore implements FeatureStore {
		
	/** Logger for Advisor. */
	final static Logger LOG = LoggerFactory.getLogger(InMemoryFeatureStore.class);
	
	final static String CONF_FILENAME = "ff4j.xml";
	
	/** InMemory FlipPoint Map */
	protected LinkedHashMap< String, Feature > featuresMap = new LinkedHashMap<String, Feature>();
	
	/** Default. */
	public InMemoryFeatureStore() {
		this(CONF_FILENAME);
	}
	
	/** Default. */
	public InMemoryFeatureStore(String fileName) {
		try {
			LOG.info("Load features file with {}", fileName);
			InputStream xmlIN = getClass().getClassLoader().getResourceAsStream(fileName);
			this.featuresMap = FeatureLoader.loadFeatures(xmlIN);
		} catch (IOException e) {
			throw new IllegalArgumentException("Cannot initialized Memory Store", e);
		}
	}

	/** Default. */
	public InMemoryFeatureStore(LinkedHashMap< String, Feature > maps) {
		this.featuresMap = maps;
	}
	
	/** Default. */
	public InMemoryFeatureStore(Collection <Feature > collec) {
		if (collec != null && !collec.isEmpty()) {
			for (Feature feature : collec) {
				this.featuresMap.put(feature.getUid(), feature);
			}
		}
	}
	
	/** {@inheritDoc} */
	public void create(Feature fp) {
		if (exist(fp.getUid())) {
			LOG.error("FlipPoint '" + fp.getUid() + "' already exist");
			throw new FeatureAlreadyExistException(fp.getUid());
		}
		featuresMap.put(fp.getUid(), fp);
	}
	
	/** {@inheritDoc} */
	public void update(Feature fp) {
		Feature fpExist = read(fp.getUid());
		
		// Checking new roles
		Set<String> toBeAdded = new HashSet<String>();
		toBeAdded.addAll(fp.getAuthorizations());
		toBeAdded.removeAll(fpExist.getAuthorizations());
		for (String addee : toBeAdded) {
			// Will fail if invalid userrole
			grantRoleOnFeature(fpExist.getUid(), addee);
		}

		featuresMap.put(fp.getUid(), fp);
	}

	/** {@inheritDoc} */
	public void delete(String fpId) {
		if (!exist(fpId)) throw new FeatureNotFoundException(fpId);
		featuresMap.remove(fpId);
		LOG.info("FlipPoint '" + fpId+ "' deleted");
	}		

	/** {@inheritDoc} */
	public void grantRoleOnFeature(String flipId, String roleName) {
		LOG.info("Adding role '" + roleName+ "' to FlipPoint '" + flipId + "'");
		if (!exist(flipId)) throw new FeatureNotFoundException(flipId);
		featuresMap.get(flipId).getAuthorizations().add(roleName);
	}

	/** {@inheritDoc} */
	public void removeRoleFromFeature(String flipId, String roleName) {
		if (!exist(flipId)) throw new FeatureNotFoundException(flipId);
		featuresMap.get(flipId).getAuthorizations().remove(roleName);
	}
	
	/**
	 * Perform in memory modification on features map.
	 *
	 * @param featId
	 * 		feature unique identifier.
	 * @param status
	 * 		new status to apply to feature.
	 */
	protected void createOrUpdate(String featId, boolean status, String description, Set < String > auth) {
		LOG.debug("Setting '" + featId + "' to " + (status ? "enabled" : "disabled") + " ("+ description + ")" );
		featuresMap.put(featId, new Feature(featId, status, description, auth));
	}
	
	/** {@inheritDoc} */
	public boolean exist(String featId) {
		return featuresMap.containsKey(featId);
	}	

	/** {@inheritDoc} */
	public void enable(String featID) {
		if (!exist(featID)) throw new FeatureNotFoundException(featID);
		featuresMap.get(featID).enable();
	}

	/** {@inheritDoc} */
	public void disable(String featID) {
		if (!exist(featID)) throw new FeatureNotFoundException(featID);
		featuresMap.get(featID).disable();
	}
	
	/** {@inheritDoc} */
	public Feature read(String featureUid)
	throws FeatureNotFoundException {
		if (!featuresMap.containsKey(featureUid)) {
			throw new FeatureNotFoundException(featureUid);
		}
		return featuresMap.get(featureUid);
	}	

	/** {@inheritDoc} */
	public LinkedHashMap<String, Feature> readAll() {
		return featuresMap;
	}
	
	
}
