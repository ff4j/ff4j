package org.ff4j.test.gettingstarted;

import static org.ff4j.FF4j.sDisableFeature;
import static org.ff4j.FF4j.sIsFlipped;
import static org.junit.Assert.fail;

import org.ff4j.FF4j;
import org.junit.Test;

public class GettingStartedCode01 {

    @Test
    public void helloTest() {
        FF4j.sAutoCreateFeature(true);
        if (sIsFlipped("first")) {
            System.out.println("Hello World !! Yes it Works !");
        }
        sDisableFeature("first");

        if (!sIsFlipped("first")) {
            System.out.println("Back to reality !!");
        } else {
            fail();
        }
    }

}
