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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Bean representing a pie chart.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PieChart extends AbstractChart implements Serializable {
    
    /** serial. */
    private static final long serialVersionUID = -6019282228665603275L;
   
    /** sector for the graph. */
    private List < Serie < Integer > > sectors = new ArrayList<Serie < Integer >>();

    /**
     * Constructor with title.
     *
     * @param t
     *      target title.
     */
    public PieChart(String t) {
        super(t);
    }

    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(" \"title\" : \"" +  getTitle() + "\", ");
        sb.append(" \"sectors\" : [");
        if (null != sectors) {
            boolean first = true;
            for (Serie < Integer > pieSector : sectors) {
                if (!first) {
                    sb.append(", ");
                }
                sb.append(pieSector.toString());
                first = false;
            }
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
     * Getter accessor for attribute 'sectors'.
     *
     * @return
     *       current value of 'sectors'
     */
    public List<Serie < Integer >> getSectors() {
        return sectors;
    }
}
