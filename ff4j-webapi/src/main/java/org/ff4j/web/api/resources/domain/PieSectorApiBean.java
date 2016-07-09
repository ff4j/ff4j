package org.ff4j.web.api.resources.domain;

import org.ff4j.audit.chart.Serie;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Target bean to display a pie sector.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "pieSector", description = "resource representation of a pie sector" )
@JsonInclude(Include.NON_NULL)
public class PieSectorApiBean {
    
    /** label. */
    @ApiModelProperty( value = "label for this sector", required = false )
    @JsonProperty("label")
    private String label = "n/a";
    
    /** value. */
    @ApiModelProperty( value = "target value", required = false )
    @JsonProperty("value")
    private double value = 0.0;
    
    /** color. */
    @ApiModelProperty( value = "target color", required = false )
    @JsonProperty("color")
    private String color = "FFFFFF";
    
    /**
     * Constructor by copy.
     *
     * @param sector
     *      sector.
     */
    public PieSectorApiBean(Serie<Integer> sector) {
        this.label = sector.getLabel();
        this.value = sector.getValue();
        this.color = sector.getColor();
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
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public double getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(double value) {
        this.value = value;
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
