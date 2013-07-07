package org.ff4j.strategy;

/**
 * Each feature should implement the flipping strategy. (enabling/disabling will be handle by flipper.
 *
 * @author clunven
 */
public interface FlippingStrategy {
	
	/**
	 * Tell if flip should be realized.
	 *
	 * @param featureName
	 * 		target featureName
	 * @param executionContext
	 * 		custom params to make decision
	 * @return
	 * 		if flipping should be performed
	 */
	 boolean activate(String featureName, Object... executionContext);
	 
	 /**
	  * Allow to parameterized Flipping Strategy
	  * @param featureName
	  * 		current featureName
	  * @param initValue
	  * 		initial Value
	  */
	 void init(String featureName, String initValue);

}
