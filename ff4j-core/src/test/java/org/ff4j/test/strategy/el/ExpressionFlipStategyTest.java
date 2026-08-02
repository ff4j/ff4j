package org.ff4j.test.strategy.el;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.FF4j;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.el.ExpressionFlipStrategy;
import org.ff4j.test.AssertFf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link ExpressionFlipStrategy} class.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ExpressionFlipStategyTest {

    /** current instance of FF4j */
    private final FF4j ff4j = new FF4j(new XmlParser(),"test-expressionFlipStategy-ok.xml");

    /** Assertion. */
    private final AssertFf4j assertFF4j = new AssertFf4j(ff4j);

    @Test
    public void testExpression() throws Exception {
        Assertions.assertNotNull(ff4j.getFeature("D"));
        Assertions.assertNotNull(ff4j.getFeature("D").getFlippingStrategy());
        Assertions.assertNotNull(ff4j.getFeature("D").getFlippingStrategy().getInitParams());
        boolean dFlipped = ff4j.check("D");
        Assertions.assertTrue(dFlipped);
    }
    
    @Test
    public void testEnableC() {
        ff4j.enable("C");
        assertFF4j.assertThatFeatureNotFlipped("D");
    }

    @Test
    public void testEnableB() {
        ff4j.enable("B");
        assertFF4j.assertThatFeatureFlipped("D");
    }

    @Test
    public void testExplicitevaluate() {
        ExpressionFlipStrategy efs = new ExpressionFlipStrategy();

        Assertions.assertTrue(efs.evaluate("D", ff4j.getFeatureStore(), null));
        Assertions.assertTrue(efs.evaluate("TOTO", ff4j.getFeatureStore(), null));

        FlippingExecutionContext fex = new FlippingExecutionContext();
        fex.putString(ExpressionFlipStrategy.PARAM_EXPRESSION, "D");
        Assertions.assertTrue(efs.evaluate("D", ff4j.getFeatureStore(), fex));

        fex.putString(ExpressionFlipStrategy.PARAM_EXPRESSION, "TOTO");
        Assertions.assertFalse(efs.evaluate("D", ff4j.getFeatureStore(), fex));
    }

}
