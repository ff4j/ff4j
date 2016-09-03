package org.ff4j.audit.chart;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.utils.JsonUtils;

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
    
    /** SerieID -> Serie (label/color/value) value=<slotID, nombre de point> */
    private Map < String, Serie < Map < String, MutableHitCount >>> series = 
            new HashMap<String, Serie<Map<String,MutableHitCount>>>();
    
    /**
     * Default constuctor
     */
    public TimeSeriesChart() {
    }
    
    /**
     * Parameterized constructor.
     *
     * @param from
     *      starting date
     * @param to
     *      ending date
     * @param units
     *      time slot
     */
    public TimeSeriesChart(long from, long to, TimeUnit units) {
        this.initSlots(from, to, units);
    }
    
    /**
     * Create slots and initiate data structure.
     *
     * @param startTime
     *      period start date
     * @param endTime
     *      period end date
     * @param units
     *      current units
     * @return
     * 
     */
    public void initSlots(long from, long to, TimeUnit units) {
        long slotWitdh = 0;
        switch (units) {
            case MINUTES:
                slotWitdh = 1000 * 60;
                this.sdf = new SimpleDateFormat("yyyyMMdd-HH:mm");
            break;
            case HOURS:
                slotWitdh = 1000 * 60 * 60;
                this.sdf = new SimpleDateFormat("yyyyMMdd-HH");
            break;
            case DAYS:
                slotWitdh = 1000 * 60 * 60 * 24;
                this.sdf =  new SimpleDateFormat("yyyyMMdd");
            break;
            default:
                slotWitdh = 1000;
                this.sdf = new SimpleDateFormat("yyyyMMdd-HH:mm:ss");
            break;
        }
        // Create slots for the timeSeries base ones
        int nbslot = new Long(1 + (to - from) / slotWitdh).intValue();
        for (int i = 0; i < nbslot; i++) {
            long startSlotTime = from + slotWitdh * i;
            String slotLabel   = sdf.format(new Date(startSlotTime));
            getTimeSlots().add(slotLabel);
        }
    }
    
    /**
     * Add Event to chart.
     *
     * @param evt
     *      current event
     */
    public void addEvent(Event evt) {
        if (!series.containsKey(evt.getName())) {
            createNewSerie(evt.getName());
        }
        String targetSlot = sdf.format(new Date(evt.getTimestamp()));
        Serie < Map <String, MutableHitCount > > targetSerie = series.get(evt.getName());
        if (targetSerie != null) {
            MutableHitCount mhc = targetSerie.getValue().get(targetSlot);
            if (mhc != null) {
                mhc.inc();
            }
        }
    }
    
    
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
        series.put(idSerie, newSerie);
    }
    

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"slots\" : " + JsonUtils.collectionAsJson(timeSlots));
        StringBuilder sbNames  = new StringBuilder("[");
        StringBuilder sbColors = new StringBuilder("[");
        StringBuilder sbValues = new StringBuilder("[");
        boolean first = true;
        for (Serie< Map < String, MutableHitCount >> serie : series.values()) {
            // Name
            sbNames.append(first ? "" : ",");
            sbNames.append(JsonUtils.valueAsJson(serie.getLabel()));
            // Color
            sbColors.append(first ? "" : ",");
            sbColors.append(JsonUtils.valueAsJson("#" + serie.getColor()));
            // Values
            sbValues.append(first ? "" : ",");
            sbValues.append(serie.getValue().values().toString());
            first = false;
        }
        sbNames.append("]");
        sbColors.append("]");
        sbValues.append("]");
        sb.append(", \"serieNames\" : " + sbNames.toString());
        sb.append(", \"serieColors\" : " + sbColors.toString());
        sb.append(", \"serieValues\" : " + sbValues.toString());
        sb.append("}");
        return sb.toString();
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
