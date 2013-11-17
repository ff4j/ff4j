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
import java.util.Map;

import org.ff4j.core.FeatureStore;

/**
 * The feature will be flipped after release date is reached.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ReleaseDateFlipStrategy extends AbstractFlipStrategy {

    /** Pattern to create a release Date. */
    public static final SimpleDateFormat SDF = new SimpleDateFormat("yyyy-MM-DD-HH:mm");

    /** Constant for release Date. */
    private static final String PARAMNAME_RELEASEDATE = "releaseDate";

    /** Release Date. */
    private Date releaseDate = new Date();

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParam);
        assertRequiredParameter(PARAMNAME_RELEASEDATE);
        try {
            this.releaseDate = SDF.parse(initParam.get(PARAMNAME_RELEASEDATE));
        } catch (ParseException e) {
            throw new IllegalArgumentException("Cannot parse release date, invalid format correct is 'YYYY-MM-DD-HH:mm'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean activate(String featureName, FeatureStore store, Object... executionContext) {
        // No use of featureName
        // No use of featureStore
        // No use of executionContext
        return new Date().after(releaseDate);
    }

    /**
     * Getter accessor for attribute 'releaseDate'.
     * 
     * @return current value of 'releaseDate'
     */
    public Date getReleaseDate() {
        return releaseDate;
    }

    /**
     * Setter accessor for attribute 'releaseDate'.
     * 
     * @param releaseDate
     *            new value for 'releaseDate '
     */
    public void setReleaseDate(Date releaseDate) {
        this.releaseDate = releaseDate;
    }

}
