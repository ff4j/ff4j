package org.ff4j.test;

import static org.junit.Assert.fail;
import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

public class GettingStartedCode02 {

	@Test
	public void loadFromFile() {
		
		// Load from File
		new Flipper(new InMemoryFeatureStore("ff4j-simple.xml"));

		// Create new
		Flipper.createFeature(new Feature("AwesomeFeature2", true, "some desc"));
		
		// Testing feature...
		if (Flipper.isFlipped("AwesomeFeature")) {
			System.out.println("Yes it's great !!");
		} else {
			fail();
		}
		// Update the feature status
		Flipper.disableFeature("AwesomeFeature");
		
		// Testing feature...
		if (Flipper.isFlipped("AwesomeFeature")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
	}
}
