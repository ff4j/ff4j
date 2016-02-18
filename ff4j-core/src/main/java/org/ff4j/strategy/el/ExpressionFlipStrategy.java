package org.ff4j.strategy.el;

import java.io.Serializable;

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
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * Allow to parse target expression.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ExpressionFlipStrategy extends AbstractFlipStrategy implements Serializable {

    /** Serial. */
    private static final long serialVersionUID = 4739173170455721752L;

    /** Expected parameter. */
    public static final String PARAM_EXPRESSION = "expression";

    /** Cached init value. */
    private static Map<String, String> mapOfValue = new HashMap<String, String>();

    /** Cached syntax trees. */
    private static Map<String, ExpressionNode> cachedExpression = new HashMap<String, ExpressionNode>();

    /**
     * Default constructor using introspection.
     */
    public ExpressionFlipStrategy() {}
    
    public ExpressionFlipStrategy(String featureName, String expression) {
        getInitParams().put(PARAM_EXPRESSION, expression);
        mapOfValue.put(featureName, expression);
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initValue) {
        super.init(featureName, initValue);
        assertRequiredParameter(PARAM_EXPRESSION);
        mapOfValue.put(featureName, initValue.get(PARAM_EXPRESSION));
    }

    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String featureName, FeatureStore currentStore, FlippingExecutionContext executionContext) {
        // If execution context specified overriding initvalue
        if ((null != executionContext) && executionContext.containsKey(PARAM_EXPRESSION)) {
            return evaluateExpression(executionContext.getString(PARAM_EXPRESSION), currentStore);
        } else if (mapOfValue.containsKey(featureName)) {
            // Else, check initial value of featureName (if exist)
            return evaluateExpression(mapOfValue.get(featureName), currentStore);
        }
        // FeatureName does not exit, no condition required
        return true;
    }

    /**
     * Evaluate expression, put it in cache is required.
     * 
     * @param expression
     *            target expression
     * @return expression evaluation value
     */
    private boolean evaluateExpression(String expression, FeatureStore currentStore) {
        if (!cachedExpression.containsKey(expression)) {
            cachedExpression.put(expression, ExpressionParser.parseExpression(expression));
        }
        return cachedExpression.get(expression).evalue(getFeaturesStatus(currentStore));
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
