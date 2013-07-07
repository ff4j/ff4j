package org.ff4j.test;

import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Test;

public class FlipperTest {
	
	@Test
	public void testInit() {
		new FF4j();
		new FF4j(new InMemoryFeatureStore());
		new FF4j(new InMemoryFeatureStore(), new DefaultAuthorisationManager());
	}
	
	@Test
	public void testFlipped() {
		FF4j f = new FF4j();
		f.setStore(new InMemoryFeatureStore());
		FF4j.getFeatures();
		FF4j.getAuthorizationsManager();
		FF4j.getFeature("first");
		FF4j.logFeatures();
		
		FF4j.isFlipped("first");
		f.setAuthorizationsManager(new DefaultAuthorisationManager());
		FF4j.isFlipped("first", new RandomFlipStrategy(), "test");
		FF4j.getFeature("first").setFlippingStrategy(new RandomFlipStrategy());
		FF4j.isFlipped("first", new RandomFlipStrategy(), "test");
		
		FF4j.getFeature("first").setEnable(false);
		FF4j.isFlipped("first");
		FF4j.isFlipped("first", new RandomFlipStrategy(), "test");
		FF4j.getFeature("first").setFlippingStrategy(new RandomFlipStrategy());
		FF4j.isFlipped("first", new RandomFlipStrategy(), "test");
		
		
		
		
		
		
	}

}
