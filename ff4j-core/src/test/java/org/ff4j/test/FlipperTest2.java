package org.ff4j.test;

import junit.framework.Assert;

import org.ff4j.Flipper;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

public class FlipperTest2 {

	@Test
	public void test_initStore() {
		
		Flipper.initStore(new InMemoryFeatureStore("ff4j.xml"));
		
		Assert.assertTrue(Flipper.isFlipped("MoufMoufFeature"));
	}
}
