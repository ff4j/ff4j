package org.ff4j.exception;

/**
 * Store could be parameterized to through exception when Feature not found.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureAlreadyExistException extends RuntimeException {

	/** serial. */
	private static final long serialVersionUID = -232699648959802172L;

	/**
	 * Parameterized constructor.
	 * 
	 * @param featureName
	 *            feature to be processed
	 * @param cause
	 *            parent exception.
	 */
	public FeatureAlreadyExistException(String featureName, Throwable cause) {
		super(featureName + " already exist in store, duplicate uid", cause);
	}

	/**
	 * Parameterized constructor.
	 * 
	 * @param featureName
	 *            feature to be processed
	 **/
	public FeatureAlreadyExistException(String featureName) {
		super(featureName + " already exist in store, duplicate uid");
	}

}
