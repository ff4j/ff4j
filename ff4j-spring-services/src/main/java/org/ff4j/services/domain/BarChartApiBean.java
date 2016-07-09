package org.ff4j.services.domain;

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
