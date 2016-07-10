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
import java.util.ArrayList;
import java.util.List;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class BarChartApiBean implements Serializable {

    private static final long serialVersionUID = -4014082937210867980L;

    private String title;

    private List<String> labels = new ArrayList<String>();

    private List<BarSeriesApiBean> series = new ArrayList<BarSeriesApiBean>();

    public BarChartApiBean() {
        super();
    }

    public BarChartApiBean(org.ff4j.audit.chart.BarChart barChart) {
        this.title = barChart.getTitle();
    }

    public String getTitle() {
        return title;
    }

    public List<String> getLabels() {
        return labels;
    }

    public List<BarSeriesApiBean> getSeries() {
        return series;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setLabels(List<String> labels) {
        this.labels = labels;
    }

    public void setSeries(List<BarSeriesApiBean> series) {
        this.series = series;
    }
}
