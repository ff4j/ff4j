package org.ff4j.strategy;

import java.io.Serializable;

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

import java.util.Map;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;

/**
 * This strategy will flip feature as soon as the release date is reached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class PonderationStrategy extends AbstractFlipStrategy implements Serializable {

    /** Serial number. */
    private static final long serialVersionUID = -2353911851539414159L;

    /** Return equiprobability as 50%. */
    private static final double HALF = 0.5;

    /** Threshold. */
    private static final String PARAM_WEIGHT = "weight";

    /** Change threshold. */
    private double weight = HALF;

    /**
     * Default Constructor.
     */
    public PonderationStrategy() {}

    /**
     * Parameterized constructor.
     * 
     * @param threshold
     *            threshold
     */
    public PonderationStrategy(double threshold) {
        this.weight = threshold;
        checkWeight();
        getInitParams().put(PARAM_WEIGHT, String.valueOf(threshold));
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParams) {
        super.init(featureName, initParams);
        if (initParams != null && initParams.containsKey(PARAM_WEIGHT)) {
            this.weight = Double.parseDouble(initParams.get(PARAM_WEIGHT));
        }
        checkWeight();
    }

    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String featureName, FeatureStore currentStore, FlippingExecutionContext executionContext) {
        return Math.random() <= weight;
    }

    /**
     * Check that the threshold is a value proportion (0 < P < 1).
     */
    private void checkWeight() {
        if (weight < 0 || weight > 1) {
            throw new IllegalArgumentException("The ponderation value is a percentage and should be set between 0 and 1");
        }
    }

    /**
     * Setter accessor for attribute 'weight'.
     * 
     * @param weight
     *            new value for 'weight '
     */
    public void setWeight(double weight) {
        this.weight = weight;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        this.initParams.put(PARAM_WEIGHT, String.valueOf(weight));
        return initParams;
    }

}
