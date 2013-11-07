package org.ff4j.strategy.el;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;

/**
 * Allow to parse target expression.
 * 
 * @author clunven
 */
public class ExpressionFlipStrategy implements FlippingStrategy {

    /** Cached init value. */
    private static Map<String, String> mapOfValue = new HashMap<String, String>();

    /** Cached syntax trees. */
    private static Map<String, ExpressionNode> cachedExpression = new HashMap<String, ExpressionNode>();

    /** Storing initial value. */
    private String init = null;

    /**
     * Default constructor using introspection.
     */
    public ExpressionFlipStrategy() {}

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initValue) {
        mapOfValue.put(featureName, initValue);
        this.init = initValue;
    }

    /** {@inheritDoc} */
    @Override
    public String getInitParams() {
        return init;
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore currentStore, Object... executionContext) {
        if (executionContext == null || executionContext.length == 0) {
            // Wait, you define an expression strategy but do not set EXPRESSION
            if (mapOfValue.containsKey(featureName)) {
                String expression = mapOfValue.get(featureName);
                if (!cachedExpression.containsKey(expression)) {
                    cachedExpression.put(expression, ExpressionParser.parseExpression(expression));
                }
                return cachedExpression.get(expression).evalue(getFeaturesStatus(currentStore));
            }
            return true;
        } else {
            String expression = (String) executionContext[0];
            cachedExpression.put(expression, ExpressionParser.parseExpression(expression));
            return cachedExpression.get(expression).evalue(getFeaturesStatus(currentStore));
        }
    }

    /**
     * Return status of all the features to calculate.
     * 
     * @param currentStore
     *            current store for features
     * @return current statuses for stores
     */
    private Map<String, Boolean> getFeaturesStatus(FeatureStore currentStore) {
        Map<String, Boolean> bools = new HashMap<String, Boolean>();
        List<Feature> listOfFlip = new ArrayList<Feature>();
        listOfFlip.addAll(currentStore.readAll().values());
        for (Feature fp : listOfFlip) {
            bools.put(fp.getUid(), fp.isEnable());
        }
        return bools;
    }

}
