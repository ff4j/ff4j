package org.ff4j.web.api.resources.domain;

import org.ff4j.audit.chart.Serie;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Target bean to display a bar series.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "barSeries", description = "resource representation of a bar series" )
public class BarSeriesApiBean {
    
    /** Label for this series. */
    @ApiModelProperty( value = "label for this sebar series", required = false )
    @JsonProperty("label")
    private String label = "N/A";
    
    /** color for this series. */
    @ApiModelProperty( value = "target color", required = false )
    @JsonProperty("color")
    private String color = "FFFFFF";
    
    /** value. */
    @ApiModelProperty( value = "target values", required = false )
    @JsonProperty("values")
    private Double value = 0D;
    
    /**
     * Constructor by copy.
     *
     * @param barSeries
     *      barSeries.
     */
    public BarSeriesApiBean(Serie<Double> barSeries) {
        this.label = barSeries.getLabel();
        this.color = barSeries.getColor();
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

}
