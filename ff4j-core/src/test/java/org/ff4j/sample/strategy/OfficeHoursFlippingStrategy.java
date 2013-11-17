package org.ff4j.sample.strategy;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

import java.util.Calendar;
import java.util.Map;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlipStrategy;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * Sample custom {@link FlipStrategy} for testing purposes.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class OfficeHoursFlippingStrategy extends AbstractFlipStrategy {

    /** Start Hour. */
    private int start;

    /** Hend Hour. */
    private int end;

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initValue) {
        super.init(featureName, initValue);
        start = new Integer(initValue.get("startDate"));
        end = new Integer(initValue.get("endDate"));
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore cuurentStore, Object... executionContext) {
        int currentHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
        return (currentHour >= start && currentHour < end);
    }

}
