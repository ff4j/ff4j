package org.ff4j.services.domain;

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
