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

import java.io.Serializable;

/**
 * Chart item.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class Serie < V > implements Serializable {
    
    /** serial. */
    private static final long serialVersionUID = 4114123301942844821L;

    /** label. */
    private String label = "n/a";
    
    /** value. */
    private V value;
    
    /** color. */
    private String color = "FFFFFF";
    
    /**
     * Initialization throughlabel
     *
     * @param l
     */
    public Serie(String l) {
        this.label = l;
    }
    
    /**
     * Constructor.
     *
     * @param l
     *      label
     * @param v
     *      value
     */
    public Serie(String l, V v) {
        this(l);
        this.value = v;
    }

    /**
     * Constructor.
     *
     * @param l
     *      label
     * @param v
     *      value
     */
    public Serie(String l, V v, String color) {
        this(l,v);
        this.color = color;
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{ \"label\" : \"" + getLabel() + "\", ");
        sb.append(" \"value\" : " + getValue() + ", ");
        sb.append(" \"color\" : \"#" + getColor() + "\" }");
        return sb.toString();
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
     *      new value for 'label '
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public V getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     *      new value for 'value '
     */
    public void setValue(V value) {
        this.value = value;
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
     *      new value for 'color '
     */
    public void setColor(String color) {
        this.color = color;
    }   

    
    

}
