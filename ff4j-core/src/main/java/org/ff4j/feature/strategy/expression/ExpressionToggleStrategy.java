package org.ff4j.feature.strategy.expression;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.FF4jContext;
import org.ff4j.feature.AbstractRepositoryFeatures;
import org.ff4j.feature.ToggleStrategy;
import org.ff4j.feature.strategy.AbstractToggleStrategy;

/**
 * Allow to parse target expression.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ExpressionToggleStrategy extends AbstractToggleStrategy implements ToggleStrategy, Serializable {

    /** Serial. */
    private static final long serialVersionUID = 4739173170455721752L;

    /** Expected parameter. */
    public static final String PARAM_EXPRESSION = "expression";

    /** Cached init value. */
    private static Map<String, String> mapOfValue = new HashMap<String, String>();

    /** Cached syntax trees. */
    private static Map<String, ExpressionNode> cachedExpression = new HashMap<String, ExpressionNode>();
    
    /** Feature Store reference. */
    public static AbstractRepositoryFeatures currentStore;

    /**
     * Default constructor using introspection.
     */
    public ExpressionToggleStrategy() {}
    
    public ExpressionToggleStrategy(String featureName, String expression) {
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
    public boolean isToggled(org.ff4j.feature.Feature feature, FF4jContext executionContext) {
        if (null == currentStore) {
            throw new IllegalStateException("Store must be setup");
        }
        // If execution context specified overriding initvalue
        if ((null != executionContext) && executionContext.containsKey(PARAM_EXPRESSION)) {
            return evaluateExpression(executionContext.getString(PARAM_EXPRESSION).get(), currentStore);
        } else if (mapOfValue.containsKey(feature.getUid())) {
            // Else, check initial value of featureName (if exist)
            return evaluateExpression(mapOfValue.get(feature.getUid()), currentStore);
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
    private boolean evaluateExpression(String expression, AbstractRepositoryFeatures currentStore) {
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
    private Map<String, Boolean> getFeaturesStatus(AbstractRepositoryFeatures currentStore) {
        Map<String, Boolean> bools = new HashMap<String, Boolean>();
        currentStore.findAll().forEach(fp ->  bools.put(fp.getUid(), fp.isEnable()));
        return bools;
    }

}
