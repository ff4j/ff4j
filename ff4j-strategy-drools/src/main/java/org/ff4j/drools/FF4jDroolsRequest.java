package org.ff4j.drools;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;

/**
 * Wrapper Bean for ff4j context {@link FlippingExecutionContext} to be used as Drools Fact.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsRequest {
    
    /** Current feature on which apply the strategy. */
    private String featureName = "";
    
    /** Reference to feature store. */
    private FeatureStore featureStore;
    
    /** FF4J context to send facts into drools session. */
    private FlippingExecutionContext executionContext;
    
    /** Target result, expected to be updated by rule files. */
    private boolean toggled = false;
    
    /** Avoid loop and several evaluation. */
    private boolean evaluated = false;
    
    /**
     * Parameter of the execute method.
     *
     * @param featName
     *      feature name
     * @param store
     *      target store
     * @param ctx
     *      current contexte
     */
    public FF4jDroolsRequest(String featName, FeatureStore store, FlippingExecutionContext ctx) {
        this.featureName = featName;
        this.featureStore = store;
        this.executionContext = ctx;
    }

    /**
     * Getter accessor for attribute 'featureName'.
     *
     * @return
     *       current value of 'featureName'
     */
    public String getFeatureName() {
        return featureName;
    }

    /**
     * Setter accessor for attribute 'featureName'.
     * @param featureName
     * 		new value for 'featureName '
     */
    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    /**
     * Getter accessor for attribute 'featureStore'.
     *
     * @return
     *       current value of 'featureStore'
     */
    public FeatureStore getFeatureStore() {
        return featureStore;
    }

    /**
     * Setter accessor for attribute 'featureStore'.
     * @param featureStore
     * 		new value for 'featureStore '
     */
    public void setFeatureStore(FeatureStore featureStore) {
        this.featureStore = featureStore;
    }

    /**
     * Getter accessor for attribute 'executionContext'.
     *
     * @return
     *       current value of 'executionContext'
     */
    public FlippingExecutionContext getExecutionContext() {
        return executionContext;
    }

    /**
     * Setter accessor for attribute 'executionContext'.
     * @param executionContext
     * 		new value for 'executionContext '
     */
    public void setExecutionContext(FlippingExecutionContext executionContext) {
        this.executionContext = executionContext;
    }

    /**
     * Getter accessor for attribute 'toggle'.
     *
     * @return
     *       current value of 'toggle'
     */
    public boolean isToggled() {
        return toggled;
    }

    /**
     * Setter accessor for attribute 'toggle'.
     * @param toggle
     * 		new value for 'toggle '
     */
    public void setToggled(boolean toggle) {
        this.toggled = toggle;
    }

    /**
     * Getter accessor for attribute 'evaluated'.
     *
     * @return
     *       current value of 'evaluated'
     */
    public boolean isEvaluated() {
        return evaluated;
    }

    /**
     * Setter accessor for attribute 'evaluated'.
     * @param evaluated
     * 		new value for 'evaluated '
     */
    public void setEvaluated(boolean evaluated) {
        this.evaluated = evaluated;
    }
    
}
