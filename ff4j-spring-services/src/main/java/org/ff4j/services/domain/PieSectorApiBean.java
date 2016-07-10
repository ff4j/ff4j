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
public class PieSectorApiBean implements Serializable {

    private static final long serialVersionUID = -8998722757094848417L;

    private String label = CommonConstants.N_A;

    private int value = 0;

    private String color = CommonConstants.HTML_WHITE;

    public PieSectorApiBean() {
        super();
    }

    public PieSectorApiBean(Serie<Integer> sector) {
        this.label = sector.getLabel();
        this.value = sector.getValue();
        this.color = sector.getColor();
    }

    public String getLabel() {
        return label;
    }

    public double getValue() {
        return value;
    }

    public String getColor() {
        return color;
    }
}
