package org.ff4j.test.gettingstarted;

import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Test;

public class GettingStartedCode02 {

	@Test
	public void loadFromFile() {
		
		// Load from File
		new FF4j(new InMemoryFeatureStore("ff4j-simple.xml"));

		// Create new
		FF4j.sCreateFeature(new Feature("AwesomeFeature2", true, "some desc"));
		
		// Testing feature...
		if (FF4j.sIsFlipped("AwesomeFeature")) {
			System.out.println("Yes it's great !!");
		} else {
			fail();
		}
		// Update the feature status
		FF4j.sDisableFeature("AwesomeFeature");
		
		// Testing feature...
		if (FF4j.sIsFlipped("AwesomeFeature")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
	}
}
