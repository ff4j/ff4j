package org.ff4j.audit.repository;

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

import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;

/**
 * Persistence store for {@link Event} messages.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface EventRepository {

    /** total hit count. */
    String TITLE_PIE_HITCOUNT = "Total Hit Counts";
    
    /** distribution. */
    String TITLE_BARCHAR_HIT = "HitCounts Distribution";
    
    /**
     * Save event into store synchronously.
     * 
     * @param e
     *            target event to store
     * @return if saving is OK
     */
    boolean saveEvent(Event e);
    
    /**
     * List feature names monitored.
     *
     * @return
     *      target list of features
     */
    Set < String> getFeatureNames();

    /**
     * Get total hit of feature on a period of time.
     * 
     * @param startTime
     *            start time of window
     * @param endTime
     *            end time of window
     * @return
     */
    PieChart getHitsPieChart(long startTime, long endTime);
    
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
    BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot);
    
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
    BarChart getHitsBarChart(long startTime, long endTime, int nbslot);
    
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
    PieChart getFeatureHitsPie(String featureId, long startTime, long endTime);
   
    /**
     * Get all events.
     * 
     * @return all event in the repository
     */
    int getTotalEventCount();
  
}
