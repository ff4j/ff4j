package org.ff4j.strategy;

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

import org.ff4j.store.FeatureStore;

/**
 * Randomly activate/desactivate feature
 * 
 * @author clunven
 */
public class RandomFlipStrategy implements FlippingStrategy {

    /** Return equiprobability as 50%. */
    private static final double HALF = 0.5;

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initValue) {
        // Nothing to do
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore currentStore, Object... executionContext) {
        return Math.random() > HALF;
    }

}
