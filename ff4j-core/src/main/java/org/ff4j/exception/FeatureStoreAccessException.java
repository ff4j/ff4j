package org.ff4j.exception;

/**
 * Represents errors occuring during access to FlippingPoints.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreAccessException extends RuntimeException {

	/** serial. */
	private static final long serialVersionUID = -5745511622056588959L;

	/**
	 * Parameterized constructor.
	 * 
	 * @param featureName
	 *            message to be processed
	 * @param cause
	 *            parent exception.
	 */
	public FeatureStoreAccessException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Parameterized constructor.
	 * 
	 * @param featureName
	 *            message to be processed
	 * @param cause
	 *            parent exception.
	 */
	public FeatureStoreAccessException(String message) {
		super(message);
	}
	
}
