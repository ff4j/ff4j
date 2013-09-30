package org.ff4j.test;

import org.ff4j.FF4j;
import org.junit.Test;

public class GettingStartedCode {

    @Test
    public void myFirstTest() {

        // Default status disabled
        FF4j.sCreateFeature("someFeature");

        if (FF4j.sIsFlipped("someFeature")) {
            System.out.println("Feature is UP !");
        } else {
            System.out.println("Feature is DOWN !");
        }
    }

    @Test
    public void mySecondTest() {

        FF4j.sAutoCreateFeature(true);
        if (FF4j.sIsFlipped("feature2")) {
            System.out.println("Feature is UP !");
        } else {
            System.out.println("Feature is DOWN !");
        }
    }

}
