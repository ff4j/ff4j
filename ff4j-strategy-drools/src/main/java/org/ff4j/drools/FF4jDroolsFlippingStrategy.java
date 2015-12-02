package org.ff4j.drools;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * Proposition of {@link FlippingStrategy} invoking the JBoss Drools rule Engine
 * to implement the strategy logic. We create a "Fact" with all context related to FF4j and
 * expect the rules to update this fact. In the end we drive the toggling with the 'toggled' attribute. 
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsFlippingStrategy extends AbstractFlipStrategy {

    /** key initParam. */
    private static final String KEY_BASE_NAME = "BASENAME";
    
    /** key initParam. */
    private static final String KEY_RULES_FILES = "RULES_FILES";
    
    /** target drools base name. */
    private String basename;

    /** list of files. */
    private Set<String> ruleFiles = new HashSet<>();

    /**
     * Constructor.
     */
    public FF4jDroolsFlippingStrategy() {
    }
    
    /**
     * Constructor.
     *
     * @param drlFiles
     *            drools rules files
     */
    public FF4jDroolsFlippingStrategy(String baseName) {
       this.basename = baseName;
       initParams.put(KEY_BASE_NAME, basename);
       FF4JDroolsService.initFromBaseName(basename);
    }
    
    /**
     * Constructor.
     *
     * @param drlFiles
     *            drools rules files
     */
    public FF4jDroolsFlippingStrategy(Set<String> files) {
        this.ruleFiles = files;
        initParams.put(KEY_RULES_FILES, files.toString());
        FF4JDroolsService.initFromRulesFiles(files);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        initParams.put(KEY_BASE_NAME, basename);
        return initParams;
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParams);
        if (!FF4JDroolsService.isInitialized()) {
            if (initParam.containsKey(KEY_BASE_NAME)) {
                this.basename = initParams.get(KEY_BASE_NAME);
                FF4JDroolsService.initFromBaseName(basename);
            } else if (initParam.containsKey(KEY_RULES_FILES)) {
                String exp = initParam.get(KEY_RULES_FILES);
                // TODO Convert from Stirng to HASSHET
                FF4JDroolsService.initFromBaseName(basename);
            } else {
                throw new IllegalArgumentException("Init param '" + KEY_BASE_NAME + "' is required to fetch Drools settings");
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String uid, FeatureStore store, FlippingExecutionContext ctx) {
        /*
         * To retrieve result for rules execution there are 2 ways: - Modifed an existing fact - Retrieve FacHandler from session
         * : session.getFactHandles(filter)
         * 
         * FF4J expects the fact {@link FF4JDroolsRequest} to be modified by the target rules. By default the status is 'false'.
         */
        return FF4JDroolsService.getInstance().evaluate(new FF4jDroolsRequest(uid, store, ctx));
    }

    /**
     * Getter accessor for attribute 'basename'.
     *
     * @return current value of 'basename'
     */
    public String getBasename() {
        return basename;
    }

    /**
     * Setter accessor for attribute 'basename'.
     * 
     * @param basename
     *            new value for 'basename '
     */
    public void setBasename(String basename) {
        this.basename = basename;
    }

}
