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

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ff4j.audit.graph.Curve;

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
     * Get list of events.
     * 
     * @return list of event
     */
    List<Event> getAllEvents();

    /**
     * Search Event Repository and Extract a curve.
     * 
     * @param featureNameList
     *            target feature name list
     * @param interval
     *            interval for 2 points (in millis)
     * @param startTime
     *            start timestamp for search
     * @param endTime
     *            end timestamp for search
     * @return
     */
    Map<String, Curve> getHitCurves(Set<String> featureNameList, long interval, long startTime, long endTime);

    /**
     * Search event of hit to dedicated featureName.
     * 
     * @param featureName
     *            target featureName
     * @param interval
     *            interval between 2 measures
     * @param startTime
     *            starttime for measure
     * @param endTime
     *            endtime for measure
     * @return curve for dedicated feature
     */
    Curve getHitCurve(String featureName, long interval, long startTime, long endTime);

}
