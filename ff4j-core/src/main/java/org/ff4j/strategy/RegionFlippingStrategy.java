package org.ff4j.strategy;

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * This strategy will flip feature as soon as the release date is reached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class RegionFlippingStrategy extends AbstractFlipStrategy implements Serializable {

	private static final long serialVersionUID = -3447089693117748712L;

	/**
	 * initial parameter.
	 */

	public static final String INIT_PARAMNAME_REGIONS = "europeCountries";

	/**
	 * current user attribute
	 */
	public static final String PARAMNAME_USER_REGION = "region";

	/**
	 * Initial Granted Regions.
	 */
	private Set<String> setOfGrantedRegions;

	public RegionFlippingStrategy() {
		this.setOfGrantedRegions = new HashSet<String>();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void init(String featureName, Map<String, String> initValue) {
		super.init(featureName, initValue);
		assertRequiredParameter(INIT_PARAMNAME_REGIONS);
		String[] arrayOfRegions = initValue.get(INIT_PARAMNAME_REGIONS).split(",");
		setOfGrantedRegions.addAll(Arrays.asList(arrayOfRegions));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean evaluate(String fName, FeatureStore fStore, FlippingExecutionContext ctx) {
		// true means required here
		String userRegion = ctx.getString(PARAMNAME_USER_REGION, true);
		return setOfGrantedRegions.contains(userRegion);
	}
}
