package org.ff4j.strategy.time;

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
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * The feature will be flipped after release date is reached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class ReleaseDateFlipStrategy extends AbstractFlipStrategy {
    
    public static final String DATE_PATTERN = "yyyy-MM-dd-HH:mm";
    
    /** Pattern to create a release Date. */
    public static final SimpleDateFormat SDF = new SimpleDateFormat(DATE_PATTERN);

    /** Constant for release Date. */
    private static final String PARAMNAME_RELEASEDATE = "releaseDate";

    /** Release Date. */
    private Date releaseDate = new Date();

    /**
     * Default constructor for introspection.
     */
    public ReleaseDateFlipStrategy() {}

    /**
     * Initialization with a date expression.
     * 
     * @param date
     */
    public ReleaseDateFlipStrategy(String strDate) {
        try {
            this.releaseDate = SDF.parse(strDate);
        } catch (ParseException e) {
            throw new IllegalArgumentException("Cannot parse release date, invalid format correct is '" + DATE_PATTERN + "'", e);
        }
        getInitParams().put(PARAMNAME_RELEASEDATE, strDate);
    }

    /**
     * Initialisation with a date.
     * 
     * @param releaseDate
     */
    public ReleaseDateFlipStrategy(Date releaseDate) {
        this.releaseDate = releaseDate;
        getInitParams().put(PARAMNAME_RELEASEDATE, SDF.format(releaseDate));
    }

    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParam);
        assertRequiredParameter(PARAMNAME_RELEASEDATE);
        try {
            this.releaseDate = SDF.parse(initParam.get(PARAMNAME_RELEASEDATE));
        } catch (ParseException e) {
            throw new IllegalArgumentException("Cannot parse release date, invalid format correct is '" + DATE_PATTERN + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String featureName, FeatureStore store, FlippingExecutionContext executionContext) {
        // No use of featureName
        // No use of featureStore
        // No use of executionContext
        return new Date().after(releaseDate);
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
