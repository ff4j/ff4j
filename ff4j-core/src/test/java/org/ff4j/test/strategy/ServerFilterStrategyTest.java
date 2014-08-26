package org.ff4j.test.strategy;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.text.ParseException;

import org.junit.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.ServerFilterStrategy;
import org.ff4j.test.AbstractFf4jTest;
import org.junit.Test;

public class ServerFilterStrategyTest extends AbstractFf4jTest {

    @Override
    public FF4j initFF4j() {
        return new FF4j("test-serverFilterStrategy.xml");
    }


    @Test
    public void testFilterOK() throws ParseException {
        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        ServerFilterStrategy cStra = (ServerFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());

        // When (add correct client name)
        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.addValue(ServerFilterStrategy.SERVER_HOSTNAME, "dev01");

        // Then
        Assert.assertTrue(ff4j.check(F1, fex));
    }

    @Test
    public void testFilterInvalidClient() throws ParseException {
        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        ServerFilterStrategy cStra = (ServerFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());

        // When (add invalid client name)
        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.addValue(ServerFilterStrategy.SERVER_HOSTNAME, FEATURE_NEW);

        // Then
        Assert.assertFalse(ff4j.check(F1, fex));
    }


}
