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

import org.ff4j.audit.graph.BarSeries;
import org.ff4j.services.constants.CommonConstants;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class BarSeriesApiBean implements Serializable {

    private static final long serialVersionUID = 8703972617439641703L;

    private String label = CommonConstants.N_A;

    private String color = CommonConstants.HTML_WHITE;

    private List<Double> values = new ArrayList<>();

    public BarSeriesApiBean() {
        super();
    }

    public BarSeriesApiBean(BarSeries barSeries) {
        this.label = barSeries.getLabel();
        this.color = barSeries.getColor();
        this.values = barSeries.getValues();
    }

    public String getLabel() {
        return label;
    }

    public String getColor() {
        return color;
    }

    public List<Double> getValues() {
        return values;
    }
}
