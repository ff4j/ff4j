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

import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.repository.EventRepository;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class EventRepositoryApiBean implements Serializable {

    private static final long serialVersionUID = -3365322115944400241L;

    private String type;

    private int hitCount;

    private PieChartApiBean eventsPie;

    private BarChartApiBean barChart;

    public EventRepositoryApiBean() {
        super();
    }

    public EventRepositoryApiBean(EventRepository evtRepository, Long start, Long end) {
        type = evtRepository.getClass().getName();
        EventQueryDefinition query = new EventQueryDefinition();
        if (start != null) {
            query.setFrom(start);
        }
        if (end != null) {
            query.setTo(end);
        }
        // Create PIE
        eventsPie = new PieChartApiBean(evtRepository.getFeatureUsagePieChart(query));
        // Create BARCHART
        barChart = new BarChartApiBean(evtRepository.getFeatureUsageBarChart(query));
        // Total Count
        for (PieSectorApiBean sector : eventsPie.getSectors()) {
            hitCount += sector.getValue();
        }
    }

    public String getType() {
        return type;
    }

    public int getHitCount() {
        return hitCount;
    }

    public PieChartApiBean getEventsPie() {
        return eventsPie;
    }

    public BarChartApiBean getBarChart() {
        return barChart;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setHitCount(int hitCount) {
        this.hitCount = hitCount;
    }

    public void setEventsPie(PieChartApiBean eventsPie) {
        this.eventsPie = eventsPie;
    }

    public void setBarChart(BarChartApiBean barChart) {
        this.barChart = barChart;
    }
}
