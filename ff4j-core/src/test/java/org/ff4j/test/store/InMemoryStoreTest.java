package org.ff4j.test.store;

import java.util.LinkedHashMap;

import junit.framework.Assert;

import org.ff4j.Feature;
import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Test;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author clunven
 */
public class InMemoryStoreTest extends AbstractStoreTest {
	
	/** {@inheritDoc} */
	public FeatureStore initStore() throws Exception {
		return new InMemoryFeatureStore();
	}
	
	@Test
	public void testUnitFeatureInitialization() {
		InMemoryFeatureStore imfs = new InMemoryFeatureStore(new Feature("default", true, "desc", null, new RandomFlipStrategy()));
		Assert.assertEquals(1, imfs.readAll().size());
	}
	
	@Test
	public void testUnitFeatureInitialization2() {
		LinkedHashMap <String, Feature> map1 = new LinkedHashMap<String, Feature>();
		map1.put("new", new Feature("new", true, "description"));
		map1.put("old", new Feature("old", true, "description"));
		new InMemoryFeatureStore(map1);
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testUnitFeatureInitialization3() {
		try {
			new InMemoryFeatureStore("invalid.xml");
			fail();
		} catch (IllegalArgumentException iae) {

		}
	}

}
