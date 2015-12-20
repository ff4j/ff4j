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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Element of the bar series.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class BarSeries implements Serializable {

    /** serial. */
    private static final long serialVersionUID = -8669264222408815943L;

    /** Label for this series. */
    private String label = "N/A";
    
    /** color for this series. */
    private String color = "FFFFFF";
    
    /** value. */
    private List < Double > values = new ArrayList<Double>();
   
    /**
     * Default constructor.
     */
    public BarSeries(String label,  String color, int nbValue) {
        this.label = label;
        this.color = color;
        for (int idx = 0;idx<nbValue;idx++) {
            values.add(new Double(0));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /**
     * Convert current object to Json String.
     * @return
     *      target json object
     */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(" \"label\" : \"" +  getLabel() + "\", ");
        sb.append(" \"color\" : \"" +  getColor() + "\", ");
        sb.append(" \"values\" : [");
        boolean first = true;
        for(Double val : values) {
            if (!first) {
                sb.append(",");
            }
            sb.append(val);
            first= false;
        }
        sb.append("] }");
        return sb.toString();
    } 
    
    /**
     * Increment hit ratio for this slot.
     * 
     * @param offset
     *            offset of point
     */
    public void incrCount(int offset) {
        getValues().set(offset, getValues().get(offset)+1);
    }

    /**
     * Getter accessor for attribute 'label'.
     *
     * @return
     *       current value of 'label'
     */
    public String getLabel() {
        return label;
    }

    /**
     * Setter accessor for attribute 'label'.
     * @param label
     * 		new value for 'label '
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * Getter accessor for attribute 'color'.
     *
     * @return
     *       current value of 'color'
     */
    public String getColor() {
        return color;
    }

    /**
     * Setter accessor for attribute 'color'.
     * @param color
     * 		new value for 'color '
     */
    public void setColor(String color) {
        this.color = color;
    }

    /**
     * Getter accessor for attribute 'values'.
     *
     * @return
     *       current value of 'values'
     */
    public List<Double> getValues() {
        return values;
    }

    /**
     * Setter accessor for attribute 'values'.
     * @param values
     * 		new value for 'values '
     */
    public void setValues(List<Double> values) {
        this.values = values;
    }
    
}
