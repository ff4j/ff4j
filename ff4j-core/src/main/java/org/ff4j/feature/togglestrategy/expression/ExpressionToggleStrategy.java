package org.ff4j.feature.togglestrategy.expression;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.feature.repository.FeaturesRepositorySupport;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.feature.togglestrategy.ToggleContext;
import org.ff4j.feature.togglestrategy.TogglePredicate;

/**
 * Allow to parse target expression.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ExpressionToggleStrategy extends AbstractToggleStrategy implements TogglePredicate, Serializable {

    /** Serial. */
    private static final long serialVersionUID = 4739173170455721752L;

    /** Expected parameter. */
    public static final String PARAM_EXPRESSION = "expression";

    /** Cached init value. */
    private static Map<String, String> mapOfValue = new HashMap<String, String>();

    /** Cached syntax trees. */
    private static Map<String, ExpressionNode> cachedExpression = new HashMap<String, ExpressionNode>();
    
    /** Feature Store reference. */
    public static FeaturesRepositorySupport currentStore;

    /**
     * Default constructor using introspection.
     */
    public ExpressionToggleStrategy() {}
    
    public ExpressionToggleStrategy(String featureName, String expression) {
        getParams().put(PARAM_EXPRESSION, expression);
        mapOfValue.put(featureName, expression);
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initValue) {
        super.init(featureName, initValue);
        assertParam(PARAM_EXPRESSION);
        mapOfValue.put(featureName, initValue.get(PARAM_EXPRESSION));
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        if (null == currentStore) {
            throw new IllegalStateException("Store must be setup");
        }
        // If execution context specified overriding initvalue
        if ((null != ctx) && ctx.containsKey(PARAM_EXPRESSION)) {
            return evaluateExpression(ctx.getString(PARAM_EXPRESSION).get(), currentStore);
        } else if (mapOfValue.containsKey(ctx.getFeature().getUid())) {
            // Else, check initial value of featureName (if exist)
            return evaluateExpression(mapOfValue.get(ctx.getFeature().getUid()), currentStore);
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
    private boolean evaluateExpression(String expression, FeaturesRepositorySupport currentStore) {
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
    private Map<String, Boolean> getFeaturesStatus(FeaturesRepositorySupport currentStore) {
        Map<String, Boolean> bools = new HashMap<String, Boolean>();
        currentStore.findAll().forEach(fp ->  bools.put(fp.getUid(), fp.isEnabled()));
        return bools;
    }

}
