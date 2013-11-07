package org.ff4j.test.strategy.el;

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

import junit.framework.TestCase;

import org.ff4j.FF4j;
import org.ff4j.strategy.el.ExpressionFlipStrategy;
import org.junit.Assert;
import org.junit.Test;

/**
 * Tests for {@link ExpressionFlipStrategy} class.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ExpressionFlipingStategyTest extends TestCase {

    /** current instance of FF4j */
    private final FF4j ff4j = new FF4j("ff4j-el.xml");

    @Test
    public void testExpression() throws Exception {
        Assert.assertNotNull(ff4j.getFeature("D"));
        Assert.assertNotNull(ff4j.getFeature("D").getFlippingStrategy());
        Assert.assertNotNull(ff4j.getFeature("D").getFlippingStrategy().getInitParams());
        boolean dFlipped = ff4j.isFlipped("D");
        Assert.assertTrue(dFlipped);
    }
    
    @Test
    public void testEnableC() {
        ff4j.enable("C");
        boolean dFlipped = ff4j.isFlipped("D");
        Assert.assertFalse(dFlipped);
    }

    @Test
    public void testEnableB() {
        ff4j.enable("B");
        boolean dFlipped = ff4j.isFlipped("D");
        Assert.assertTrue(dFlipped);
    }

    @Test
    public void testExplicitActivate() {
        ExpressionFlipStrategy efs = new ExpressionFlipStrategy();
        Assert.assertTrue(efs.activate("D", ff4j.getStore(), (Object[]) null));
        Assert.assertTrue(efs.activate("TOTO", ff4j.getStore(), (Object[]) null));
        Assert.assertTrue(efs.activate("D", ff4j.getStore(), "D"));
        Assert.assertFalse(efs.activate("D", ff4j.getStore(), "TOTO"));
    }

}
