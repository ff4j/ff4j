package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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

import org.ff4j.audit.chart.Serie;
import org.ff4j.services.constants.CommonConstants;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class BarSeriesApiBean implements Serializable {

    private static final long serialVersionUID = 8703972617439641703L;

    private String label = CommonConstants.N_A;

    private String color = CommonConstants.HTML_WHITE;

    private Double value = 0D;

    public BarSeriesApiBean() {
        super();
    }

    public BarSeriesApiBean(Serie<Double> barSeries) {
        this.label  = barSeries.getLabel();
        this.color  = barSeries.getColor();
        this.value = barSeries.getValue();
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
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public Double getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(Double value) {
        this.value = value;
    }

}
