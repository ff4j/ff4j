package org.ff4j;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.security.AuthorizationsManager;
import org.ff4j.strategy.FlippingStrategy;

/**
 * Represents a feature flag identified by an unique identifier.
 * 
 * <p>Features Flags or Features Toggle have been introduced by Martin Fowler for continuous delivery perspective.
 * It consists of enable/disable some functionalities at runtime.
 * 
 * <p><b>SecurityManagement :</b> Even a feature is enabled, you can limit its usage to a group of users 
 * (for instance BETA Tester) before wide over all your users. 
 * </p>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class Feature {

	/** Unique Feature Identifier */
	private String uid;
	
	/** Status of target feature, can be enable and disable. */
	private boolean enable = false;
	
	/** Short description of the feature, use it for information. */ 
	private String description;
	
	/** if not empty and @see {@link AuthorizationsManager} provided, limit usage to this roles. */
	private Set < String > authorizations = new HashSet<String>();
	
	/** Custom behaviour to define if feature if enable or not e.g. A/B Testing capabilities. */
	private FlippingStrategy flippingStrategy;

	/**
	 * Simplest constructor initializing feature to disable.
	 *
	 * @param uid
	 * 		unique feature name (required)
	 */
	public Feature(final String uid) {
		this(uid, false, "");
	}
	
	/**
	 * Simple constructor initializing feature with status enable/disable.
	 * 
	 * @param uid
	 * 		unique feature name (required)
	 * @param pactive
	 * 		initial feature state
	 */
	public Feature(final String uid, final boolean penable) {
		this(uid, penable, "");
	}
	
	/**
	 * Simplest Constructor (without security concerns) 
	 * 
	 * @param uid
	 * 		unique feature name (required)
	 * @param pactive
	 * 		initial feature state
	 * @param pDescription
	 * 		description of feature.
	 */
	public Feature(final String uid, final boolean penable, final String pdescription) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
		}
		this.uid         = uid;
		this.enable      = penable;
		this.description = pdescription;
	}
			
	/**
	 * Constructor with limited access roles definitions
	 *
	 * @param uid
	 * 		unique feature name (required)
	 * @param pactive
	 * 		initial feature state
	 * @param pDescription
	 * 		description of feature.
	 * @param auths
	 * 		limited roles to use the feature even if enabled
	 */
	public Feature(final String uid, final boolean penable, final String pdescription, final Collection < String > auths) {
		this(uid, penable, pdescription);
		if (auths != null && !auths.isEmpty()) {
			this.authorizations = new HashSet<String>(auths);
		}
	}
	
	/**
	 * Constructor with limited access roles definitions
	 *
	 * @param uid
	 * 		unique feature name (required)
	 * @param pactive
	 * 		initial feature state
	 * @param pDescription
	 * 		description of feature.
	 * @param auths
	 * 		limited roles to use the feature even if enabled
	 */
	public Feature(final String uid, final boolean penable, final String pdescription, final Collection < String > auths, final FlippingStrategy strat) {
		this(uid, penable, pdescription, auths);
		if (strat != null) {
			this.flippingStrategy = strat;
		}
	}

	/** {@inheritDoc} */
	public String toString() {
		StringBuilder sb = new StringBuilder("Feature '"+ this.uid + "'");
		sb.append("is '" + (enable ? "enabled":"disabled")  + "'");
		if (authorizations != null && !authorizations.isEmpty()) {
			sb.append(" roles:");
			sb.append(authorizations.toString());
		}
		if (description != null && !description.isEmpty()) {
			sb.append(", desc:" + description);
		}
		return sb.toString();
	}
	
	/**
	 * Enable target feature
	 */
	public void enable() {
		this.enable = true;
	}
	
	/**
	 * Disable target feature
	 */
	public void disable() {
		this.enable = false;
	}
	
	/**
	 * Toggle target feature (from enable to disable and vice versa)
	 */
	public void toggle() {
		this.enable = !this.enable;
	}

	/**
	 * Getter accessor for attribute 'uid'.
	 *
	 * @return
	 *       current value of 'uid'
	 */
	public String getUid() {
		return uid;
	}

	/**
	 * Setter accessor for attribute 'uid'.
	 * @param uid
	 * 		new value for 'uid '
	 */
	public void setUid(String uid) {
		this.uid = uid;
	}

	/**
	 * Getter accessor for attribute 'enable'.
	 *
	 * @return
	 *       current value of 'enable'
	 */
	public boolean isEnable() {
		return enable;
	}

	/**
	 * Setter accessor for attribute 'enable'.
	 *
	 * @param enable
	 * 		new value for 'enable '
	 */
	public void setEnable(boolean enable) {
		this.enable = enable;
	}

	/**
	 * Getter accessor for attribute 'description'.
	 *
	 * @return
	 *       current value of 'description'
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Setter accessor for attribute 'description'.
	 *
	 * @param description
	 * 		new value for 'description '
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * Getter accessor for attribute 'authorizations'.
	 *
	 * @return
	 *       current value of 'authorizations'
	 */
	public Set<String> getAuthorizations() {
		return authorizations;
	}

	/**
	 * Setter accessor for attribute 'authorizations'.
	 * @param authorizations
	 * 		new value for 'authorizations '
	 */
	public void setAuthorizations(Set<String> authorizations) {
		this.authorizations = authorizations;
	}

	/**
	 * Getter accessor for attribute 'flippingStrategy'.
	 *
	 * @return
	 *       current value of 'flippingStrategy'
	 */
	public FlippingStrategy getFlippingStrategy() {
		return flippingStrategy;
	}

	/**
	 * Setter accessor for attribute 'flippingStrategy'.
	 * @param flippingStrategy
	 * 		new value for 'flippingStrategy '
	 */
	public void setFlippingStrategy(FlippingStrategy flippingStrategy) {
		this.flippingStrategy = flippingStrategy;
	}
	
}
