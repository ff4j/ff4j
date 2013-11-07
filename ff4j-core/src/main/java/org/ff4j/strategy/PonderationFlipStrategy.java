package org.ff4j.strategy;

/*
 * #%L
 * ff4j-core
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

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;

/**
 * This strategy will flip feature as soon as the release date is reached.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PonderationFlipStrategy implements FlippingStrategy {

    /** Return equiprobability as 50%. */
    private static final double HALF = 0.5;

    /** Change threshold. */
    private double threshold = HALF;

    /**
     * Default Constructor.
     */
    public PonderationFlipStrategy() {}

    /**
     * Check that the threshold is a value proportion (0 < P < 1).
     */
    private void checkThreshold() {
        if (threshold < 0 || threshold > 1) {
            throw new IllegalArgumentException("The ponderation value is a percentage and should be set between 0 and 1");
        }
    }

    /**
     * Parameterized constructor.
     * 
     * @param threshold
     *            threshold
     */
    public PonderationFlipStrategy(double threshold) {
        this.threshold = threshold;
        checkThreshold();
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initValue) {
        if (initValue != null) {
            this.threshold = Double.valueOf(initValue).doubleValue();
            checkThreshold();
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore currentStore, Object... executionContext) {
        return Math.random() <= threshold;
    }

    /** {@inheritDoc} */
    @Override
    public String getInitParams() {
        return String.valueOf(threshold);
    }

    /**
     * Getter accessor for attribute 'threshold'.
     * 
     * @return current value of 'threshold'
     */
    public double getThreshold() {
        return threshold;
    }

    /**
     * Setter accessor for attribute 'threshold'.
     * 
     * @param threshold
     *            new value for 'threshold '
     */
    public void setThreshold(double threshold) {
        this.threshold = threshold;
        checkThreshold();
    }

}
