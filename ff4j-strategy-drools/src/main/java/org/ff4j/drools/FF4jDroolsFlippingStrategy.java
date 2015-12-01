package org.ff4j.drools;

import java.util.Map;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;
import org.kie.api.KieServices;
import org.kie.api.runtime.KieContainer;
import org.kie.api.runtime.KieSession;

/**
 * Externalize the flipping strategy into
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jDroolsFlippingStrategy extends AbstractFlipStrategy {

    private static final String KEY_BASE_NAME = "BASENAME";

    /** Internal KIE Service. */
    private KieServices kieServices;

    /** Container for sessions. */
    private KieContainer kieContainer;

    /** Working Session. */
    private KieSession ksession;
    
    /** target drools base name. */
    private String basename;

    /**
     * Constructor.
     */
    public FF4jDroolsFlippingStrategy() {
        this(null);
    }

    /**
     * Constructor.
     *
     * @param drlFiles
     *            drools rules files
     */
    public FF4jDroolsFlippingStrategy(String baseName) {
        this.basename = baseName;
        kieServices  = KieServices.Factory.get();
        kieContainer = kieServices.getKieClasspathContainer();
        if (basename != null) {
            ksession = kieContainer.newKieSession(basename);
        }
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
        if (initParam.containsKey(KEY_BASE_NAME)) {
            ksession = kieContainer.newKieSession(initParam.get(KEY_BASE_NAME));
        } else {
            throw new IllegalArgumentException("Init param '" + KEY_BASE_NAME + "' is required to fetch Drools settings");
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String uid, FeatureStore store, FlippingExecutionContext ctx) {
        /*
         * To retrieve result for rules execution there are 2 ways:
         * - Modifed an existing fact
         * - Retrieve FacHandler from session : session.getFactHandles(filter)
         * 
         * FF4J expects the fact {@link FF4JDroolsRequest} to be modified by the target rules.
         * By default the status is 'false'.
         */
        final FF4jDroolsRequest droolsRequest = new FF4jDroolsRequest(uid, store, ctx);
        if (ksession == null) {
            throw new IllegalArgumentException("KnowledgeSession is null, "
                    + "please check KbaseName (" + basename + ") in kmodule.xml file");
        }
        ksession.setGlobal( "store", store );
        ksession.insert(droolsRequest);
       
        // Execute the rules
        ksession.fireAllRules();
        
        // Empty the stateful session
        ksession.dispose();
        
        return droolsRequest.isToggled();
    }

    /**
     * Getter accessor for attribute 'basename'.
     *
     * @return
     *       current value of 'basename'
     */
    public String getBasename() {
        return basename;
    }

    /**
     * Setter accessor for attribute 'basename'.
     * @param basename
     * 		new value for 'basename '
     */
    public void setBasename(String basename) {
        this.basename = basename;
    }

}
