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

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;

public class OfficeHoursFlippingStrategy implements FlippingStrategy {

    private int start;
    private int end;

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initValue) {
        String[] inits = initValue.split("-");
        start = new Integer(inits[0]);
        end = new Integer(inits[1]);
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore cuurentStore, Object... executionContext) {
        int currentHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
        return (currentHour >= start && currentHour < end);
    }

    @Override
    public String getInitParams() {
        // TODO Auto-generated method stub
        return null;
    }
}
