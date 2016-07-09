package org.ff4j.audit.chart;

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
import java.util.TreeMap;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventSeries;

/**
 * Continous lines (response times, hits)
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class SparklineChart extends AbstractChart {

    /** Serial. */
    private static final long serialVersionUID = 2235564369635471213L;
    
    /** SerieId, -> label/color + serie Values */
    private Map < String, Serie < EventSeries > > events = new TreeMap<String, Serie <EventSeries>>();
    
    /**
     * Default constructor.
     */
    public SparklineChart(String title) {
        super(title);
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(" \"title\" : \"" +  getTitle() + "\", ");
        sb.append(" \"series\" : { ");
        if (events != null && events.isEmpty()) {
            boolean firstSerie = true;
            for (String serieId : events.keySet()) {
                Serie<EventSeries> currentSerie = events.get(serieId);
                if (!firstSerie) {
                    sb.append(", ");
                }
                sb.append("{ \"label\":\"" + currentSerie.getLabel() + "\",");
                sb.append("  \"color\":\"" + currentSerie.getColor() + "\",");
                sb.append("  \"" +  serieId + "\": [");
                boolean first = true;
                for (Event event : currentSerie.getValue()) {
                    if (!first) {
                        sb.append(", ");
                    }
                    sb.append("{ \"" +  event.getTimestamp() + "\": " + event.getValue() + "}");
                    first = false;
                }
                sb.append("] }"); // end of values & series
                firstSerie = false;
            }
            sb.append("}"); // end of current series map
        }
        sb.append("}"); // end of target
        return sb.toString();
    }
    
}
