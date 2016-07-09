package org.ff4j.audit.chart;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.audit.MutableHitCount;

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

/**
 * Information for timeSerie.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class TimeSeriesChart extends AbstractChart {
    
    /** Serial. */
    private static final long serialVersionUID = 4131401051473272099L;
    
    /** Target Simple Date format. */
    private SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd-HH:mm:ss");

    /** Init Once. */
    private List < String > timeSlots = new ArrayList<String>();
    
    /** SerieID -> Serie (label/color) + 1 point per slot. */
    private Map < String, Serie< Map < String, MutableHitCount >>> series = 
            new HashMap<String, Serie<Map<String,MutableHitCount>>>();
    
    /**
     * Create new Serie with existing slots.
     *
     * @param idSerie
     *      target serie id
     */
    public void createNewSerie(String idSerie) {
        // Init new Serie
        Serie< Map < String, MutableHitCount >> newSerie = new Serie<Map<String,MutableHitCount>>(idSerie);
      
        // Populate slots
        Map < String, MutableHitCount > val = new HashMap<String, MutableHitCount>();
        for (String slot : timeSlots) {
            val.put(slot, new MutableHitCount());
        }
        newSerie.setValue(val);
    }
    
    /**
     * Getter accessor for attribute 'sdf'.
     *
     * @return
     *       current value of 'sdf'
     */
    public SimpleDateFormat getSdf() {
        return sdf;
    }

    /**
     * Setter accessor for attribute 'sdf'.
     * @param sdf
     * 		new value for 'sdf '
     */
    public void setSdf(SimpleDateFormat sdf) {
        this.sdf = sdf;
    }

    /**
     * Getter accessor for attribute 'timeSlots'.
     *
     * @return
     *       current value of 'timeSlots'
     */
    public List<String> getTimeSlots() {
        return timeSlots;
    }

    /**
     * Setter accessor for attribute 'timeSlots'.
     * @param timeSlots
     * 		new value for 'timeSlots '
     */
    public void setTimeSlots(List<String> timeSlots) {
        this.timeSlots = timeSlots;
    }

    /**
     * Getter accessor for attribute 'series'.
     *
     * @return
     *       current value of 'series'
     */
    public Map<String, Serie<Map<String, MutableHitCount>>> getSeries() {
        return series;
    }

    /**
     * Setter accessor for attribute 'series'.
     * @param series
     * 		new value for 'series '
     */
    public void setSeries(Map<String, Serie<Map<String, MutableHitCount>>> series) {
        this.series = series;
    }
    
    
}
