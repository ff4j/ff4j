package org.ff4j.drools;

/*
 * #%L
 * ff4j-strategy-drools
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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
 * Proposition of {@link FlippingStrategy} delegating the evaluation of feature 
 * toggling to the JBoss Drools Rule Engine. 
 * 
 * <p>The {@link FlippingExecutionContext} is embeded in a Request {@link FF4jDroolsRequest} and 
 * insert as <code>Fact</code> in the Drools session.
 * 
 * <p>The rules are expected to update the request and espacially the status boolean <code>toggled</code>.
 * You can initialize the drools Engine with both the KbaseName or a set of DRL files.
 * 
 * <p>You can find a sample of rules below :
 * <pre>rule "f1_checkSampleThreshold"
 *   dialect "mvel"
 *   when
 *     $req : FF4jDroolsRequest( featureName == "f1", evaluated == false )
 *   then
 *     System.out.println("Evaluating F1 Strategy");
 *     System.out.println(store.getClass().getName());
 *     modify ($req) {
 *       evaluated = true,
 *       toggled = true
 *     };
 *  end
 * </pre>
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsFlippingStrategy extends AbstractFlipStrategy {

    /** key to be used in map initParam. */
    private static final String KEY_BASE_NAME = "basename";
    
    /** key to be used in map initParam. */
    private static final String KEY_RULES_FILES = "ruleFiles";
    
    /** (If initialized with the kmodule.xml file) State as the kSession name. */
    private String basename;

    /** (If initialized with drl rule files), State as the kSession name. */
    private Set<String> ruleFiles = new HashSet<>();

    /**
     * Keep default constructor to allow dependency injection.
     */
    public FF4jDroolsFlippingStrategy() {
    }
    
    /**
     * Constructor to work with kSession names.
     *
     * <p><pre><ksession name="ff4jDroolsStrategy"/></pre>
     *
     * @param baseName
     *           Ksession name locate in META-INF/kmodule.xml (convention)
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
     * Constructor to work with drl rule files.
     *
     * @param files
     *          rules files can be DRL, RDRL...
     */
    public FF4jDroolsFlippingStrategy(Set<String> files) {
        this.ruleFiles = files;
        initParams.put(KEY_RULES_FILES, getRulesFileAsString());
        if (!FF4jDroolsService.isInitialized()) {
            FF4jDroolsService.initFromRulesFiles(files);
        }
    }

    /**
     * Generate InitParameters to be used in json serialization.
     * 
     * @return
     *      the init param map
     */
    @Override
    public Map<String, String> getInitParams() {
        initParams.put(KEY_BASE_NAME, basename);
        initParams.put(KEY_RULES_FILES, getRulesFileAsString());
        return initParams;
    }
    
    /**
     * Utility HashSet < String > values separated by comma.
     * 
     * <p>New in Java8, Stringjoiner enhance string concatenation
     *
     * @return
     *      hasehet to string easily
     */
    private String getRulesFileAsString() {
        StringJoiner joiner = new StringJoiner(",");
        ruleFiles.forEach(e ->joiner.add(e));
        return  joiner.toString();
    }

    /** 
     * Initialization through initParam and not the constructor to be used by the
     * XML feature system.
     * 
     * @param featureName
     *      current feature unique identifier
     * @param initParam
     *      attribute defined through XML configuration for instance
     */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParam);
        
        if (!FF4jDroolsService.isInitialized()) {
            
            if (initParams.containsKey(KEY_BASE_NAME)) {
                this.basename = initParams.get(KEY_BASE_NAME);
                FF4jDroolsService.initFromBaseName(basename);
            
            } else if (initParams.containsKey(KEY_RULES_FILES)) {
                String exp = initParams.get(KEY_RULES_FILES);
                this.ruleFiles = new HashSet <> (Arrays.asList(exp.split(",")));
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
