package org.ff4j.test;

import org.ff4j.Flipper;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Test;

public class FlipperTest {
	
	@Test
	public void testInit() {
		new Flipper();
		new Flipper(new InMemoryFeatureStore());
		new Flipper(new InMemoryFeatureStore(), new DefaultAuthorisationManager());
	}
	
	@Test
	public void testFlipped() {
		Flipper f = new Flipper();
		f.setStore(new InMemoryFeatureStore());
		Flipper.getFeatures();
		Flipper.getAuthorizationsManager();
		Flipper.getFeature("first");
		Flipper.logFeatures();
		
		Flipper.isFlipped("first");
		f.setAuthorizationsManager(new DefaultAuthorisationManager());
		Flipper.isFlipped("first", new RandomFlipStrategy(), "test");
		Flipper.getFeature("first").setFlippingStrategy(new RandomFlipStrategy());
		Flipper.isFlipped("first", new RandomFlipStrategy(), "test");
		
		Flipper.getFeature("first").setEnable(false);
		Flipper.isFlipped("first");
		Flipper.isFlipped("first", new RandomFlipStrategy(), "test");
		Flipper.getFeature("first").setFlippingStrategy(new RandomFlipStrategy());
		Flipper.isFlipped("first", new RandomFlipStrategy(), "test");
		
		
		
		
		
		
	}

}
