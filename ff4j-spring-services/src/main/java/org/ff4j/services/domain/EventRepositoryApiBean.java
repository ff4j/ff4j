package org.ff4j.services.domain;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;

import org.ff4j.audit.chart.Serie;
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
        type = evtRepository.getClass().getCanonicalName();
        Long computedStart = start;
        Long computedEnd = end;
        // Today
        if (start == null) {
            Calendar c = Calendar.getInstance();
            c.set(Calendar.HOUR_OF_DAY, 0);
            c.set(Calendar.MINUTE, 0);
            c.set(Calendar.SECOND, 0);
            computedStart = c.getTimeInMillis();
        }
        // Tomorrow 00:00
        if (end == null) {
            Calendar c2 = Calendar.getInstance();
            c2.setTime(new Date(System.currentTimeMillis() + 1000 * 3600 * 24));
            c2.set(Calendar.HOUR_OF_DAY, 0);
            c2.set(Calendar.MINUTE, 0);
            c2.set(Calendar.SECOND, 0);
            computedEnd = c2.getTimeInMillis();
        }
        // Create PIE
        org.ff4j.audit.chart.PieChart pie = evtRepository.getFeatureUsagePieChart(computedStart, computedEnd);
        eventsPie = new PieChartApiBean(pie);
        // Create BARCHART
        org.ff4j.audit.chart.BarChart bc = evtRepository.getFeatureUsageBarChart(computedStart, computedEnd);
        barChart = new BarChartApiBean(bc);
        // Total Count
        for (Serie<Integer> sector : pie.getSectors()) {
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
