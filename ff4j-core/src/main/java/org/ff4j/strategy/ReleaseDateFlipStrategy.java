package org.ff4j.strategy;

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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;

/**
 * The feature will be flipped after release date is reached.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ReleaseDateFlipStrategy implements FlippingStrategy {

    /** Pattern to create a release Date. */
    private static final SimpleDateFormat SDF = new SimpleDateFormat("YYYY-MM-DD-HH:mm");

    /** Release Date. */
    private Date releaseDate = null;

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, String initParam) {
        try {
            this.releaseDate = SDF.parse(initParam);
        } catch (ParseException e) {
            throw new IllegalArgumentException("Cannot parse release date, invalid format correct is 'YYYY-MM-DD-HH:mm'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getInitParams() {
        return SDF.format(this.releaseDate);
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore store, Object... executionContext) {
        // No use of featureName
        // No use of featureStore
        // No use of executionContext
        return new Date().after(releaseDate);
    }

}
