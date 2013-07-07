package org.ff4j.test;

import static org.junit.Assert.fail;
import org.ff4j.Feature;
import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

public class GettingStartedCode02 {

	@Test
	public void loadFromFile() {
		
		// Load from File
		new FF4j(new InMemoryFeatureStore("ff4j-simple.xml"));

		// Create new
		FF4j.createFeature(new Feature("AwesomeFeature2", true, "some desc"));
		
		// Testing feature...
		if (FF4j.isFlipped("AwesomeFeature")) {
			System.out.println("Yes it's great !!");
		} else {
			fail();
		}
		// Update the feature status
		FF4j.disableFeature("AwesomeFeature");
		
		// Testing feature...
		if (FF4j.isFlipped("AwesomeFeature")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
	}
}
