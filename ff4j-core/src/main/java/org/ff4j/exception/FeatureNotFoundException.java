package org.ff4j.exception;

/**
 * Store could be parameterized to through exception when Feature not found.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureNotFoundException extends RuntimeException {

    /** serial. */
    private static final long serialVersionUID = -232699648959802172L;

    /**
     * Parameterized constructor.
     * 
     * @param featureName
     *            feature to be processed
     **/
    public FeatureNotFoundException(String featureName) {
        super(featureName + " does not exist in store");
    }

}
