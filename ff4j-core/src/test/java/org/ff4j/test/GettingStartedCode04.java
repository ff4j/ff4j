package org.ff4j.test;

import static org.junit.Assert.fail;

import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.junit.Test;

public class GettingStartedCode04 {
	
	@Test
	public void myFirstTest() {
		
		// New feature (every underlying concepts created - InMemry + NoSecurity)
		Flipper.createFeature(new Feature("AwesomeFeature2", true, "some desc"));
		
		// Testing feature...
		if (Flipper.isFlipped("AwesomeFeature2")) {
			System.out.println("Yes it's great !!");
		} else {
			fail();
		}
		
		// Update the feature status
		Flipper.disableFeature("AwesomeFeature2");
		
		// Testing feature...
		if (Flipper.isFlipped("AwesomeFeature2")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
		
	}

}
