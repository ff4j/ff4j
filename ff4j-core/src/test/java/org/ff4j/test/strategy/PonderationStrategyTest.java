package org.ff4j.test.strategy;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.strategy.DarkLaunchStrategy;
import org.ff4j.strategy.PonderationStrategy;
import org.ff4j.test.AbstractFf4jTest;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit Testing
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PonderationStrategyTest extends AbstractFf4jTest {

    /** {@inheritDoc} */
    @Override
    public FF4j initFF4j() {
        // TOTO initialization through Java CODE ?
        return new FF4j("test-ponderationStrategy-ok.xml");
    }

    @Test
    public void testDefaultisHalf() {
        Feature f = ff4j.getFeature("pond_Null");
        Assert.assertEquals(1, f.getFlippingStrategy().getInitParams().size());
    }

    @Test
    public void testExpressionTo0AlwaysFalse() {
        Feature f = ff4j.getFeature("pond_0");
        Assert.assertEquals(new Double(0.0),
                Double.valueOf(((PonderationStrategy) f.getFlippingStrategy()).getInitParams().get("weight")));
        Assert.assertEquals(1, f.getFlippingStrategy().getInitParams().size());
        for (int i = 0; i < 10; i++) {
            assertFf4j.assertThatFeatureNotFlipped(f.getUid());
        }
    }

    @Test
    public void testExpressionTo1AlwaysTrue() {
        Feature f = ff4j.getFeature("pond_1");
        Assert.assertEquals(new Double(1.0),
                Double.valueOf(((PonderationStrategy) f.getFlippingStrategy()).getInitParams().get("weight")));
        Assert.assertEquals(1, f.getFlippingStrategy().getInitParams().size());
        Assert.assertEquals("1.0", f.getFlippingStrategy().getInitParams().get("weight"));

        for (int i = 0; i < 10; i++) {
            assertFf4j.assertThatFeatureFlipped(f.getUid());
        }
    }

    @Test
    public void testExpressionCustom() {
        Feature f = ff4j.getFeature("pond_6");
        Assert.assertEquals(new Double(0.6),
                Double.valueOf(((PonderationStrategy) f.getFlippingStrategy()).getInitParams().get("weight")));
        int nbOK = 0;
        int nbKO = 0;
        for (int i = 0; i < 1000; i++) {
            if (ff4j.check(f.getUid())) {
                nbOK++;
            } else {
                nbKO++;
            }
        }
        Assert.assertTrue("both result occured", nbOK > 0 && nbKO > 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testKOParameter() {
        new FF4j("test-ponderationStrategy-ko.xml");
    }

    @Test
    public void testInitializationThroughIOc() {
        PonderationStrategy pfs = new PonderationStrategy();
        pfs.setWeight(0.5);
    }
    
    @Test
    public void testInitializations() {
        PonderationStrategy pfs = new DarkLaunchStrategy();
        pfs.setWeight(0.5);
        new DarkLaunchStrategy(0.5);
    }
    
    

}
