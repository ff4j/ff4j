package org.ff4j.strategy;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;

/**
 * This strategy will check region
 * and flipped only if it's contained in expected list.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class RegionFlippingStrategy extends AbstractFlipStrategy implements Serializable {

	private static final long serialVersionUID = -3447089693117748712L;

	/**
	 * initial parameter.
	 */

	public static final String INIT_PARAMNAME_REGIONS = "environments";

	/**
	 * Parameter to be checked in context
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
