package org.ff4j.feature.togglestrategy.expression;

import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.property.PropertyString;
import org.ff4j.property.evaluate.FF4jEvaluationContext;

import java.util.HashMap;
import java.util.Map;

/**
 * Allow parsing target expression. We need to be able to evaluate other features
 * from same repository, same application (eg: A = B & C). 
 * 
 * As such we need a reference to ff4j,proper repository and proper application name, 
 * the last 2 are part of the feature and ff4j is in the context.
 */
public class ExpressionToggleStrategy extends AbstractToggleStrategy {
	
    /** if provided override default feature one. */
    public static final String PARAM_NAMESPACE = "namespace";

    /** Expected parameter. */
    public static final String PARAM_EXPRESSION  = "expression";

    /** Cached syntax trees. */
    private static final Map<String, ExpressionNode> cachedExpression = new HashMap<>();

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param config
     *      configuration
     */
    public ExpressionToggleStrategy(Backend backend, Flag relatedFeature, FF4jEvaluationContext config) {
        super(backend, relatedFeature, config);
        // Parse Expression at load to test validity
        cachedExpression.put(PARAM_EXPRESSION,
                ExpressionParser.parseExpression(
                    config.getProperty(PARAM_EXPRESSION).getValueAsString()));
    }

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param expression
     *      expression
     */
    public ExpressionToggleStrategy(Backend backend, Flag relatedFeature, String expression) {
        super(backend, relatedFeature, new FF4jEvaluationContext(new PropertyString(PARAM_EXPRESSION, expression)));
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(FF4jEvaluationContext ctx) {
       return evaluateExpression(getConfig().getProperty(PARAM_EXPRESSION).getValueAsString());
    }

    /**
     * Evaluate expression, put it in cache is required.
     *
     * @param expression
     *            target expression
     * @return expression evaluation value
     */
    private boolean evaluateExpression(String expression) {
        if (!cachedExpression.containsKey(expression))
            cachedExpression.put(expression, ExpressionParser.parseExpression(expression));
        return cachedExpression.get(expression).evaluate(getFeaturesStatus());
    }

    /**
     * Return status of all the features to calculate.
     * @return current statuses for stores
     */
    private Map<String, Boolean> getFeaturesStatus() {
        String namespace = getConfig().getProperty(PARAM_NAMESPACE).getValueAsString();
        Map<String, Boolean> statusMap = new HashMap<>();
        // Locate Repository
        getBackend()
        // Get all features
        .findAllFeatures(namespace)
         // and only check toggle On and not the full test (would lead to cyclic)
        .forEach(f ->  statusMap.put(f.getUid(), f.isToggled()));
        return statusMap;
    }

}
