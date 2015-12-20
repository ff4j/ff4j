package org.ff4j.web.api.resources.domain;

/*
 * #%L
 * ff4j-webapi
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Bean of target monitoring.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "FeatureMonitoringApiBean", description = "monitoring data of dedicated feature")
@JsonInclude(Include.NON_NULL)
public class FeatureMonitoringApiBean {
    
    /** Name of feature. */
    @ApiModelProperty( value = "name of feature", required = true )
    @JsonProperty("featureName")
    private String featureName = null;
    
    /** total hit count. */
    @ApiModelProperty( value = "total hit of the monitoring", required = true )
    @JsonProperty("hitCount")
    private int hitCount = 0;
    
    /** Hit Pie. */
    @ApiModelProperty( value = "pie of features", required = true )
    @JsonProperty("eventsPie")
    private PieChartApiBean eventsPie;
    
    /** Bar Chart. */
    @ApiModelProperty( value = "barChart for activity", required = true )
    @JsonProperty("barChart")
    private BarChartApiBean barChart;
    
    /**
     * Constructor.
     *
     * @param uid
     *      unique identifer of feature.
     */
    public FeatureMonitoringApiBean(String uid) {
        this.featureName = uid;
    }

    /**
     * Getter accessor for attribute 'featureName'.
     *
     * @return
     *       current value of 'featureName'
     */
    public String getFeatureName() {
        return featureName;
    }

    /**
     * Setter accessor for attribute 'featureName'.
     * @param featureName
     * 		new value for 'featureName '
     */
    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    /**
     * Getter accessor for attribute 'eventsPie'.
     *
     * @return
     *       current value of 'eventsPie'
     */
    public PieChartApiBean getEventsPie() {
        return eventsPie;
    }

    /**
     * Setter accessor for attribute 'eventsPie'.
     * @param eventsPie
     * 		new value for 'eventsPie '
     */
    public void setEventsPie(PieChartApiBean eventsPie) {
        this.eventsPie = eventsPie;
    }

    /**
     * Getter accessor for attribute 'barChart'.
     *
     * @return
     *       current value of 'barChart'
     */
    public BarChartApiBean getBarChart() {
        return barChart;
    }

    /**
     * Setter accessor for attribute 'barChart'.
     * @param barChart
     * 		new value for 'barChart '
     */
    public void setBarChart(BarChartApiBean barChart) {
        this.barChart = barChart;
    }

    /**
     * Getter accessor for attribute 'hitCount'.
     *
     * @return
     *       current value of 'hitCount'
     */
    public int getHitCount() {
        return hitCount;
    }

    /**
     * Setter accessor for attribute 'hitCount'.
     * @param hitCount
     * 		new value for 'hitCount '
     */
    public void setHitCount(int hitCount) {
        this.hitCount = hitCount;
    }
  
}
