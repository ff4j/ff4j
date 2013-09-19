package org.ff4j.strategy.el;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.Feature;
import org.ff4j.store.FeatureStore;
import org.ff4j.strategy.FlippingStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Allow to parse target expression.
 *
 * @author clunven
 */
public class ExpressionFlipStrategy implements FlippingStrategy {

	/** Logger for Advisor. */
	final static Logger LOG = LoggerFactory.getLogger(ExpressionFlipStrategy.class);
	
	/** Cached init value. */
	private static Map <String, String > mapOfValue = new HashMap<String, String>();
	
	/** Cached syntax trees. */
	private static Map < String, ExpressionNode > cachedExpression = new HashMap<String, ExpressionNode>();
	
	/** Reference through bean ff4j to avoid static access. */
	private FeatureStore store;
	
	/** {@inheritDoc} */
	public boolean activate(String featureName, Object... executionContext) {
		if (executionContext == null || executionContext.length == 0) {
			LOG.warn("Wait, you define an expression strategy but do not set EXPRESSION");
			if (mapOfValue.containsKey(featureName)) {
				String expression = mapOfValue.get(featureName);
				if (!cachedExpression.containsKey(expression)) {
					cachedExpression.put(expression,  ExpressionParser.parseExpression(expression));
				} else {
					LOG.debug("Getting syntax tree from cache" + cachedExpression.get(expression));
				}
				return cachedExpression.get(expression).evalue(getFeaturesStatus());
			}
			return true;
		} else {
			String expression = (String) executionContext[0];
			if (!cachedExpression.containsKey(expression)) {
				cachedExpression.put(expression,  ExpressionParser.parseExpression(expression));
			} else {
				LOG.debug("Getting syntax tree from cache" + cachedExpression.get(expression));
			}
			return cachedExpression.get(expression).evalue(getFeaturesStatus());
		}
	}
	
	/**
	 * 
	 * @return
	 */
	private Map < String, Boolean > getFeaturesStatus() {
		Map < String, Boolean > bools = new HashMap<String, Boolean>();
		List < Feature > listOfFlip = new ArrayList<Feature>();
		listOfFlip.addAll(getStore().readAll().values());
		for (Feature fp : listOfFlip) {
			bools.put(fp.getUid(), fp.isEnable());
		}
		return bools;
	}
	
	/** {@inheritDoc} */
	public void init(String featureName, String initValue) {
		mapOfValue.put(featureName, initValue);
	}

	
	/**
	 * Getter accessor for attribute 'store'.
	 *
	 * @return
	 *       current value of 'store'
	 */
	public FeatureStore getStore() {
		// Best effort : if no store provided for this strategy use the singleton
		return (store != null) ? store : FF4j.getInstance().getStore();
	}
	

	/**
	 * Setter accessor for attribute 'store'.
	 * @param store
	 * 		new value for 'store '
	 */
	public void setStore(FeatureStore store) {
		this.store = store;
	}


}
