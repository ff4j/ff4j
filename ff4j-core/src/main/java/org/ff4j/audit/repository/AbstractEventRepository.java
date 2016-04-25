package org.ff4j.audit.repository;

import java.text.SimpleDateFormat;
import java.util.ArrayList;

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

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.utils.TimeUtils;

import static org.ff4j.audit.EventConstants.*;

/**
 * Superclass implementing the custom serialization.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractEventRepository implements EventRepository { 
   
    /** {@inheritDoc} */
    public BarChart getFeaturesUsageOverTime(long startTime, long endTime, int nbslot) {
        return getFeaturesUsageOverTime(getFeatureNames(), startTime, endTime, nbslot);
    }
     
    protected boolean isEventInInterval(EventQueryDefinition query, Event evt) {
		return (query.getFrom() == null) || (query.getFrom() <= evt.getTimestamp()) &&
			   (query.getTo()   == null) || (query.getTo() >= evt.getTimestamp());
    }
    
    /** {@inheritDoc} */
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");

    
        // Tomorrow 00:00
        Calendar c2 = Calendar.getInstance();
        c2.setTime(new Date(System.currentTimeMillis() + 1000 * 3600 * 24));
        c2.set(Calendar.HOUR_OF_DAY, 0);
        c2.set(Calendar.MINUTE, 0);
        c2.set(Calendar.SECOND, 0);
        
        // Create PIE
        PieChart pie = featuresListDistributionPie(TimeUtils.getTodayMidnightTime(),
        		TimeUtils.getTomorrowMidnightTime());
        sb.append(",\"todayHitsPie\": " + pie.toJson());
        
        // Create BARCHART
        BarChart barChart = getFeaturesUsageOverTime(TimeUtils.getTodayMidnightTime(), 
        		TimeUtils.getTomorrowMidnightTime(), 24);
        sb.append(",\"todayHitsBarChart\": " + barChart.toJson());

        // Total Count
        int total = 0;
        for(PieSector sector : pie.getSectors()) {
            total += sector.getValue();
        }
        sb.append(",\"todayHitTotal\":" + total);
        sb.append("}");
        return sb.toString();
    }
    
    protected BarChart initFeaturesOverTimeBarchart(Set < String > featNameSet, long startTime, long endTime, int nbslot) {
        long slotWitdh = (endTime - startTime) / nbslot;
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
        List <String> labels = new ArrayList<String>();
        for (int i = 0; i < nbslot; i++) {
            labels.add(sdf.format(new Date(startTime + slotWitdh * i)));
        }
        return new BarChart(TITLE_BARCHAR_HIT, labels, new ArrayList<String>(featNameSet));
    }
    
    
    /** {@inheritDoc} */
    public List<Event> search(EventQueryDefinition query) {
		List < Event > targetEvents = new ArrayList<Event>();
		return targetEvents;
    }
    
    protected boolean isEventInInterval(Event evt, long startTime, long endTime) {
        return (evt.getTimestamp() > startTime) && (evt.getTimestamp() < endTime);
    }
    
    protected boolean isEventOK(Event evt, long startTime, long endTime) {
        return isEventInInterval(evt, startTime, endTime) && ACTION_CHECK_OK.equals(evt.getAction());
    }
        
}
