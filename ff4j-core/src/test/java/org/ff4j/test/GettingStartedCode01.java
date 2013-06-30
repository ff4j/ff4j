package org.ff4j.test;

import static org.junit.Assert.fail;
import org.ff4j.Flipper;
import org.junit.Test;

public class GettingStartedCode01 {
	
	@Test
	public void helloTest() {
		if (Flipper.isFlipped("first")) {
			System.out.println("Hello World !! Yes it Works !");
		} else {
			fail();
		}

		Flipper.disableFeature("first");

		if (Flipper.isFlipped("first")) {
			fail();
		} else {
			System.out.println("Back to reality !!");
		}
	}
}
