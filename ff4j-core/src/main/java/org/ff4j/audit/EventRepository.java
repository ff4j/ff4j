package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.Map;
import java.util.Set;

import org.ff4j.audit.graph.Curve;
import org.ff4j.audit.graph.PieGraph;

/**
 * Persistence store for {@link Event} messages.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface EventRepository {

    /**
     * Save event into store synchronously.
     * 
     * @param e
     *            target event to store
     * @return if saving is OK
     */
    boolean saveEvent(Event e);

    /**
     * Get total hit of feature on a period of time.
     * 
     * @param startTime
     *            start time of window
     * @param endTime
     *            end time of window
     * @return
     */
    PieGraph getTotalHitsPie(long startTime, long endTime);
    
    /**
     * Get a pie of dedicated feature.
     * @param featureId
     *      target feature if
     * @param startTime
     *      target start time
     * @param endTime
     *      target end time
     * @return
     *      target pie
     */
    PieGraph getFeatureHitsPie(String featureId, long startTime, long endTime);

    /**
     * Search event of hit to dedicated featureName.
     * 
     * @param featureName
     *            target featureName
     * @param nbRecord
     *            number of measure
     * @param startTime
     *            starttime for measure
     * @param endTime
     *            endtime for measure
     * @return curve for dedicated feature
     */
    Curve getFeatureHitsCurve(String featureName, long startTime, long endTime, int nbRecord);
    
    /**
     * Get hit curves.
     *
     * @param featNameSet
     *            target feature name set
     * @param nbslot
     *            number of measure
     * @param startTime
     *            starttime for measure
     * @param endTime
     *            endtime for measure
     * @return map of curves
     */
    Map<String, Curve> getHitCurves(Set<String> featNameSet, long startTime, long endTime, int nbslot);

    /**
     * Distribution of all activity over time.
     * 
     * @param nbRecord
     *            total number of measure
     * @param startTime
     *            target start time
     * @param endTime
     *            target end time
     * @return total curve
     */
    Curve getTotalHitsCurve(long startTime, long endTime, int nbRecord);

    /**
     * List available curves.
     * 
     * @return list of curves
     */
    Set<String> getCurveList();
    
    /**
     * Get all events.
     * 
     * @return all event in the repository
     */
    int getTotalEventCount();

}
