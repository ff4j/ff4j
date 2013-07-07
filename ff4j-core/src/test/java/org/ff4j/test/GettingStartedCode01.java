package org.ff4j.test;

import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.junit.Test;

public class GettingStartedCode01 {
	
	@Test
	public void helloTest() {
		
		// new FF4j(new InMemoryFeatureStore("ff4j.xml"));
		if (FF4j.isFlipped("first")) {
			System.out.println("Hello World !! Yes it Works !");
		} else {
			fail();
		}

		FF4j.disableFeature("first");

		if (FF4j.isFlipped("first")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
	}
}
