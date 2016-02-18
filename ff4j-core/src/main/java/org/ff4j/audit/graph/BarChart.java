package org.ff4j.audit.graph;

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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.utils.Util;
/**
 * Bean representing a histogram graph.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class BarChart extends AbstractGraphFF4j {
    
    /** serial. */
    private static final long serialVersionUID = -7438492625518407540L;
    
    /** Data to be displayed. */
    private Map < String, BarSeries > series = new LinkedHashMap<String, BarSeries>();
    
    /** Labels on X axis. */
    private List < String > labels = new ArrayList<String>();
    
    /**
     * Default constructor.
     */
    public BarChart() {
    }
    
    /**
     * Constructor to set up graph.
     * @param title
     *      graph title
     * @param barCount
     *      number of division within graph
     */
    public BarChart(String title, List < String > labels, List < String > seriesName) {
        this.labels = labels;
        setTitle(title);
        // Series
        List < String > colors = Util.getColorsGradient(seriesName.size());
        for (int idx = 0;idx < seriesName.size() ;idx++) {
            series.put(seriesName.get(idx), new BarSeries(seriesName.get(idx), colors.get(idx), labels.size()));
        }
    }
    
    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(" \"title\" : \"" +  getTitle() + "\", ");
        sb.append(" \"series\" : [");
        boolean first = true;
        for(Map.Entry<String, BarSeries> bs : series.entrySet()) {
            if (!first) {
                sb.append(",");
            }
            sb.append(bs.getValue());
            first= false;
        }
        sb.append("],");
        sb.append(" \"labels\" : [");
        
        first = true;
        for(String label : labels) {
            if (!first) {
                sb.append(",");
            }
            sb.append(" \"" + label + "\"");
            first= false;
        }
        sb.append("] }");
        return sb.toString();
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
 
    /**
     * Getter accessor for attribute 'labels'.
     *
     * @return
     *       current value of 'labels'
     */
    public List<String> getLabels() {
        return labels;
    }

    /**
     * Setter accessor for attribute 'labels'.
     * @param labels
     * 		new value for 'labels '
     */
    public void setLabels(List<String> labels) {
        this.labels = labels;
    }

    /**
     * Getter accessor for attribute 'series'.
     *
     * @return
     *       current value of 'series'
     */
    public Map<String, BarSeries> getSeries() {
        return series;
    }

    /**
     * Setter accessor for attribute 'series'.
     * @param series
     * 		new value for 'series '
     */
    public void setSeries(Map<String, BarSeries> series) {
        this.series = series;
    }
    
}
