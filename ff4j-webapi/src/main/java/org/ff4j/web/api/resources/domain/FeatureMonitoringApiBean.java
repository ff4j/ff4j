package org.ff4j.web.api.resources.domain;

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
