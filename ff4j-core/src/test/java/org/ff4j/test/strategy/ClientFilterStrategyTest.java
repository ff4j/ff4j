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
import java.util.HashMap;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.strategy.BlackListStrategy;
import org.ff4j.strategy.ClientFilterStrategy;
import org.ff4j.strategy.ServerFilterStrategy;
import org.ff4j.strategy.WhiteListStrategy;
import org.ff4j.test.AbstractFf4jTest;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test for {@link ClientFilterStrategy} pass if client is int the whitelist.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ClientFilterStrategyTest extends AbstractFf4jTest {

    @Override
    public FF4j initFF4j() {
        return new FF4j("test-clientFilterStrategy.xml");
    }

    @Test
    public void testFilterOK() throws ParseException {
        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        org.ff4j.strategy.ClientFilterStrategy cStra = (ClientFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());

        // When (add correct client name)
        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.addValue(ClientFilterStrategy.CLIENT_HOSTNAME, "pierre");

        // Then
        Assert.assertTrue(ff4j.check(F1, fex));
        Assert.assertNotNull(cStra.toJson());
    }

    @Test
    public void testFilterInvalidClient() throws ParseException {
        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        org.ff4j.strategy.ClientFilterStrategy cStra = (ClientFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());


        // When (add invalid client name)
        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.addValue(ClientFilterStrategy.CLIENT_HOSTNAME, FEATURE_NEW);

        // Then
        Assert.assertFalse(ff4j.check(F1, fex));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFilterRequiredContext() {

        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        org.ff4j.strategy.ClientFilterStrategy cStra = (ClientFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());

        // Then FeatureContext is requires
        ff4j.check(F1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testFilterRequiredClientHostName() {

        // Given
        Feature f1 = ff4j.getFeature(F1);
        Assert.assertNotNull(f1.getFlippingStrategy());
        org.ff4j.strategy.ClientFilterStrategy cStra = (ClientFilterStrategy) f1.getFlippingStrategy();
        Assert.assertNotNull(cStra.getInitParams());
        Assert.assertEquals(1, cStra.getInitParams().size());
        Assert.assertTrue(f1.isEnable());

        // When
        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.addValue(FEATURE_NEW, FEATURE_NEW);

        // Then
        ff4j.check(F1, fex);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testInitialisationProgram2() {
        ClientFilterStrategy fs = new ClientFilterStrategy("Pierre, Paul, Jacques");
        fs.init("f1", null);
        fs.init("f1", new HashMap<String, String>());
        Assert.assertFalse(fs.getInitParams().containsKey("f2"));
        fs.assertRequiredParameter("f2");
    }
    
    @Test
    public void testInitialisationProgram() {
        FlippingStrategy fs = new ClientFilterStrategy("Pierre, Paul, Jacques");
        fs.init("f1", null);
        fs.init("f1", new HashMap<String, String>());        
        new WhiteListStrategy();
        new WhiteListStrategy("Pierre");
        
        // Working
        new BlackListStrategy();
        FlippingStrategy bl2 = new BlackListStrategy("Pierre");
        FlippingExecutionContext context = new FlippingExecutionContext();
        context.putString("clientHostName", "localhost");
        Assert.assertTrue(bl2.evaluate("f1", new InMemoryFeatureStore(), context));
        
        context.putString("clientHostName", "Pierre");
        Assert.assertFalse(bl2.evaluate("f1", new InMemoryFeatureStore(), context));
    }
    
    @Test
    public void testInitialisationProgramServer() {
        FlippingStrategy fs = new ServerFilterStrategy("serv1,serv2");
        fs.init("f1", null);
        fs.init("f1", new HashMap<String, String>());
        FlippingExecutionContext context = new FlippingExecutionContext();
        fs.evaluate("f1", new InMemoryFeatureStore(), context);
    }

}
