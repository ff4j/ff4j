package org.ff4j.test.gettingstarted;

import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.junit.Test;

public class GettingStartedCode00 {
	
	@Test
	public void testSimplier() {
		FF4j.sCreateFeature(new Feature("AwesomeFeature1", true, "some desc"));
		if (FF4j.sIsFlipped("AwesomeFeature1")) {
			System.out.println("Hello");
		} else {
			fail();
		}
	}
}
