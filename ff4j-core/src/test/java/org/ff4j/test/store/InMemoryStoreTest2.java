package org.ff4j.test.store;

import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author clunven
 */
public class InMemoryStoreTest2 extends AbstractStoreTest {
	
	/** {@inheritDoc} */
	public FeatureStore initStore() throws Exception {
		return new InMemoryFeatureStore("ff4j-override.xml");
	}

}
