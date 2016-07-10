package org.ff4j.services.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.Serie;

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



/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class PieChartApiBean implements Serializable {

    private static final long serialVersionUID = 3177966921214178831L;

    private String title;

    private List<PieSectorApiBean> sectors = new ArrayList<PieSectorApiBean>();

    public PieChartApiBean() {
        super();
    }

    public PieChartApiBean(PieChart pie) {
        title = pie.getTitle();
        for (Serie<Integer> sector : pie.getSectors()) {
            sectors.add(new PieSectorApiBean(sector));
        }
    }

    public String getTitle() {
        return title;
    }

    public List<PieSectorApiBean> getSectors() {
        return sectors;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setSectors(List<PieSectorApiBean> sectors) {
        this.sectors = sectors;
    }
}
