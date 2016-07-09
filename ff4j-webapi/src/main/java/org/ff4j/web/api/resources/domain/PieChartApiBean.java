package org.ff4j.web.api.resources.domain;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.ArrayList;
import java.util.List;

import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.Serie;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Representation of a pieChart.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "pieChart", description = "resource representation of a pie chart" )
@JsonInclude(Include.NON_NULL)
public class PieChartApiBean {
    
    /** title of the graph. */
    @ApiModelProperty( value = "title of the graph", required = false )
    @JsonProperty("title")
    private String title = null;
    
    /** sector for the graph. */
    @ApiModelProperty( value = "sectors of the pie graph", required = false )
    @JsonProperty("sectors")
    private List < PieSectorApiBean > sectors = new ArrayList<PieSectorApiBean>();
    
    /**
     * Constructor for the API.
     *
     * @param pie
     *      target pie 
     */
    public PieChartApiBean(PieChart pie) {
        title = pie.getTitle();
        for (Serie<Integer> pieSector : pie.getSectors()) {
            sectors.add(new PieSectorApiBean(pieSector));
        }
    }

    /**
     * Getter accessor for attribute 'title'.
     *
     * @return
     *       current value of 'title'
     */
    public String getTitle() {
        return title;
    }

    /**
     * Setter accessor for attribute 'title'.
     * @param title
     * 		new value for 'title '
     */
    public void setTitle(String title) {
        this.title = title;
    }

    /**
     * Getter accessor for attribute 'sectors'.
     *
     * @return
     *       current value of 'sectors'
     */
    public List<PieSectorApiBean> getSectors() {
        return sectors;
    }

    /**
     * Setter accessor for attribute 'sectors'.
     * @param sectors
     * 		new value for 'sectors '
     */
    public void setSectors(List<PieSectorApiBean> sectors) {
        this.sectors = sectors;
    }

}
