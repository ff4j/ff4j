package org.ff4j.feature.togglestrategy.expression;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.ff4j.feature.repository.FeaturesRepository;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.feature.togglestrategy.ToggleContext;
import org.ff4j.property.PropertyString;

/**
 * Allow to parse target expression.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ExpressionToggleStrategy extends AbstractToggleStrategy {

    /** Serial. */
    private static final long serialVersionUID = 4739173170455721752L;

    /** Expected parameter. */
    public static final String PARAM_EXPRESSION = "expression";
    public static final String REPO_REFERENCE   = "featureRepository";

    /** Cached init value. */
    private static Map<String, String> mapOfValue = new HashMap<String, String>();

    /** Cached syntax trees. */
    private static Map<String, ExpressionNode> cachedExpression = new HashMap<String, ExpressionNode>();

    /** Current expression. */
    public String expression = null;

    /** {@inheritDoc} */
    @Override
    public void initialize() {
        PropertyString propertyString = (PropertyString) getRequiredProperty(PARAM_EXPRESSION);
        expression = propertyString.asString();
        mapOfValue.put(featureUid, expression);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean test(ToggleContext ctx) {
        Optional<String> overrideDefltExp  = ctx.getString(PARAM_EXPRESSION);
        if (overrideDefltExp.isPresent()) {
            return evaluateExpression(ctx.getFf4j().getRepositoryFeatures(), overrideDefltExp.get());
        }
        return evaluateExpression(ctx.getFf4j().getRepositoryFeatures(), expression);
    }
    
    /**
     * Evaluate expression, put it in cache is required.
     * 
     * @param expression
     *            target expression
     * @return expression evaluation value
     */
    private boolean evaluateExpression(FeaturesRepository featureRepository, String expressions) {
        if (!cachedExpression.containsKey(expression)) {
            cachedExpression.put(expression, ExpressionParser.parseExpression(expressions));
        }
        return cachedExpression.get(expression).evalue(getFeaturesStatus(featureRepository));
    }

    /**
     * Return status of all the features to calculate.
     * 
     * @param currentStore
     *            current store for features
     * @return current statuses for stores
     */
    private Map<String, Boolean> getFeaturesStatus(FeaturesRepository currentStore) {
        Map<String, Boolean> bools = new HashMap<String, Boolean>();
        currentStore.findAll().forEach(fp ->  bools.put(fp.getUid(), fp.isEnabled()));
        return bools;
    }

}
