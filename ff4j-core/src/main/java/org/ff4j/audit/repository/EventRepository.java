package org.ff4j.audit.repository;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.TimeSeriesChart;

/**
 * Persistence store for {@link Event} messages.
 * 
 * @author Cedrick Lunven (@clunven)
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
     * Retrieve an event by its unique identifer.
     *
     * @param uuid
     *      get detail from a single event by its ID
     * @param timestamp
     *      help to find the UUID (optional).
     * @return
     *      target event detail
     */
    Event getEventByUUID(String uuid, Long timestamp);
    
    /**
     * Count hit ratio of features between 2 dates. This will be used for different charts.
     *
     * @param startTime
     *          start date
     * @param endTime
     *          end time
     * @return
     */
    Map < String, MutableHitCount > getFeatureUsageHitCount(EventQueryDefinition query);
    
    /**
     * Draw a pie chart where each sector is for a feature. The value of each sector is the
     * number of execution of the feature during the period of time.
     * 
     * Pie : Sector/Feature, value- number of check OK
     * 
     * @param startTime
     *            start time of window
     * @param endTime
     *            end time of window
     * @return
     */
    PieChart getFeatureUsagePieChart(EventQueryDefinition query);
    
    /**
     * Get hit curves.
     *
     * @param filteredFeatureNames
     *            target feature name set
     * @param nbslot
     *            number of measure
     * @param startTime
     *            starttime for measure
     * @param endTime
     *            endtime for measure
     * @return map of curves
     */
    BarChart getFeatureUsageBarChart(EventQueryDefinition query);
    
    /**
     * Create measure over time.
     *
     * @param startTime
     *      time to begin measures
     * @param endTime
     *      time to end measures
     * @param nbPoints
     *      number of points.
     * @param filteredFeatures
     *      if you want to filtered feature usage
     * @return
     */
    TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu);
    
    /**
     * Get all events.
     * 
     * @return all event in the repository
     */
    int getFeatureUsageTotalHitCount(EventQueryDefinition query);
    
    /**
     * Search over events.
     *
     * @return
     *      a list of events
     */
    EventSeries searchFeatureUsageEvents(EventQueryDefinition query);
    
    /**
     * Purge feature usage.
     *
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     */
    void purgeFeatureUsage(EventQueryDefinition query);
    
    /**
     * Count hit for each host.
     *
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return the hitcount
     */
    Map < String, MutableHitCount > getHostHitCount(EventQueryDefinition query);
    
    /**
     * Use hit getHostHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    PieChart getHostPieChart(EventQueryDefinition query);
    
    /**
     * Use hit getHostHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    BarChart getHostBarChart(EventQueryDefinition query);
    
    /**
     * Count hit for each host.
     *
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return the hitcount
     */
    Map < String, MutableHitCount > getUserHitCount(EventQueryDefinition query);
    
    /**
     * Use hit getHostHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    PieChart getUserPieChart(EventQueryDefinition query);
    
    /**
     * Use hit getHostHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    BarChart getUserBarChart(EventQueryDefinition query);
    
    /**
     * Count hit for each source (api...).
     *
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return the hitcount
     */
    Map < String, MutableHitCount > getSourceHitCount(EventQueryDefinition query);
    
    /**
     * Use hit getSourceHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    PieChart getSourcePieChart(EventQueryDefinition query);
    
    /**
     * Use hit getSourceHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    BarChart getSourceBarChart(EventQueryDefinition query);
  
    /**
     * Display audit trail as list of Event.
     *
     * @param startTime
     *      time to begin measures
     * @param endTime
     *      time to end measures
     * @return
     *      target list of event
     */
    EventSeries getAuditTrail(EventQueryDefinition query);
    
    /**
     * Purge audit trail.
     *
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     */
    void purgeAuditTrail(EventQueryDefinition query);
    
    /**
     * Initialize target database with expected schema if needed.
     */
    void createSchema();
}
