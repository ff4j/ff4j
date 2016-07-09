package org.ff4j.audit.chart;

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
import java.util.List;

/**
 * Bean representing a histogram graph.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class BarChart extends AbstractChart {
    
    /** serial. */
    private static final long serialVersionUID = -7438492625518407540L;
    
    /** sector for the graph. */
    private List < Serie < Integer > > chartBars = new ArrayList< Serie < Integer > >();
    
    /**
     * Default constructor.
     */
    public BarChart(String title) {
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
        sb.append(" \"bars\" : [");
        if (null != chartBars) {
            boolean first = true;
            for (Serie<Integer> bar : chartBars) {
                if (!first) {
                    sb.append(", ");
                }
                sb.append(bar.toString());
                first = false;
            }
        }
        sb.append("] }");
        return sb.toString();
    }

    /**
     * Getter accessor for attribute 'chartBars'.
     *
     * @return
     *       current value of 'chartBars'
     */
    public List< Serie < Integer > > getChartBars() {
        return chartBars;
    }
 
    
}
