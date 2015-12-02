package org.ff4j.drools;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;

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
       // Do not initialize for each feature, it's a static contexte to be initialized once
       if (!FF4jDroolsService.isInitialized()) {
           FF4jDroolsService.initFromBaseName(basename);
       }
    }
    
    /**
     * Constructor.
     *
     * @param drlFiles
     *            drools rules files
     */
    public FF4jDroolsFlippingStrategy(Set<String> files) {
        this.ruleFiles = files;
        initParams.put(KEY_RULES_FILES, getRulesFileAsString());
        if (!FF4jDroolsService.isInitialized()) {
            FF4jDroolsService.initFromRulesFiles(files);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, String> getInitParams() {
        initParams.put(KEY_BASE_NAME, basename);
        initParams.put(KEY_RULES_FILES, getRulesFileAsString());
        return initParams;
    }
    
    /**
     * New in Java8, Stringjoiner enhance string concat..
     *
     * @return
     *      hasehet to string easily
     */
    private String getRulesFileAsString() {
        StringJoiner joiner = new StringJoiner(",");
        ruleFiles.forEach(e ->joiner.add(e));
        return  joiner.toString();
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParams);
        if (!FF4jDroolsService.isInitialized()) {
            
            if (initParam.containsKey(KEY_BASE_NAME)) {
                this.basename = initParams.get(KEY_BASE_NAME);
                FF4jDroolsService.initFromBaseName(basename);
            
            } else if (initParam.containsKey(KEY_RULES_FILES)) {
                String exp = initParam.get(KEY_RULES_FILES);
                this.ruleFiles = new HashSet <String > (Arrays.asList(exp.split(",")));
                FF4jDroolsService.initFromRulesFiles(ruleFiles);
            
            } else {
                throw new IllegalArgumentException("Init param '" + KEY_BASE_NAME + "' is required to fetch Drools settings");
            }
        }
    }

    /**
     * To retrieve result for rules execution there are 2 ways: 
     * - Modifed an existing fact 
     * - Retrieve FacHandler from session like session.getFactHandles(filter)
     * 
     * FF4J expects the fact {@link FF4JDroolsRequest} to be modified by the target rules. 
     * By default the status is 'false'.
     */
    @Override
    public boolean evaluate(String uid, FeatureStore store, FlippingExecutionContext ctx) {
        return FF4jDroolsService.getInstance().evaluate(new FF4jDroolsRequest(uid, store, ctx));
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

    /**
     * Getter accessor for attribute 'ruleFiles'.
     *
     * @return
     *       current value of 'ruleFiles'
     */
    public Set<String> getRuleFiles() {
        return ruleFiles;
    }

    /**
     * Setter accessor for attribute 'ruleFiles'.
     * @param ruleFiles
     * 		new value for 'ruleFiles '
     */
    public void setRuleFiles(Set<String> ruleFiles) {
        this.ruleFiles = ruleFiles;
    }

}
