package org.ff4j.test.strategy;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
 * %%
 * Copyright (C) 2013 Ff4J
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

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.strategy.RandomFlipStrategy;
import org.junit.Test;

/**
 * Unit Testing
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class RandomStrategyTest {

    /**
     * On chance over 2 to the power of 1000 that this test failed but assuming on 1000 tries should be both OK and KO.
     */
    @Test
    public void testRandomStrategy() {

        // Sample features
        Feature randomF = new Feature("default", true);
        randomF.setFlippingStrategy(new RandomFlipStrategy());

        // Initialize ff4j with the feature.
        FF4j ff4j = new FF4j().create(randomF);

        int nbOK = 0;
        int nbKO = 0;
        for (int i = 0; i < 1000; i++) {
            if (ff4j.isFlipped("default")) {
                nbOK++;
            } else {
                nbKO++;
            }
        }
        Assert.assertTrue("both result occured", nbOK > 0 && nbKO > 0);
    }
}
