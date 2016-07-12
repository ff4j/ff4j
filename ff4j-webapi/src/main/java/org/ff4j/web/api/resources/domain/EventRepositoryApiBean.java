package org.ff4j.web.api.resources.domain;

import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.repository.EventRepository;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Bean to represent the event repository.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "eventRepositoryApiBean", description = "resource representation of monitoring resources" )
public class EventRepositoryApiBean {
    
    /** ClassName of the event repository. */
    @ApiModelProperty( value = "type of the repository", required = true )
    @JsonProperty("type")
    private String type;
    
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
     * Default constructor.
     */
    public EventRepositoryApiBean() {
    }
            
    public EventRepositoryApiBean(EventRepository evtRepository, Long start, Long end) {
        type = evtRepository.getClass().getCanonicalName();
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

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     * 		new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
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

}
