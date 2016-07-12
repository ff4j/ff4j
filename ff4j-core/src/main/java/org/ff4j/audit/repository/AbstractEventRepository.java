package org.ff4j.audit.repository;

/*
 * #%L
 * ff4j-core
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


import static org.ff4j.audit.EventConstants.TITLE_BARCHAR_HIT;
import static org.ff4j.audit.EventConstants.TITLE_PIE_HITCOUNT;

import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.Serie;
import org.ff4j.utils.Util;

/**
 * Superclass implementing the custom serialization.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class AbstractEventRepository implements EventRepository {
    
    /** Create key. */
    protected static final SimpleDateFormat KDF = new SimpleDateFormat("yyyyMMdd");
    
    protected String getTitle(EventQueryDefinition q) {
        return " FROM " + getKeyDate(q.getFrom()) + " TO " + getKeyDate(q.getTo());
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureUsagePieChart(EventQueryDefinition q) {
        PieChart pieGraph = renderPieChartRainBow(getFeatureUsageHitCount(q));
        pieGraph.setTitle(TITLE_PIE_HITCOUNT + getTitle(q));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getHostPieChart(EventQueryDefinition q) {
        PieChart pieGraph = renderPieChartGreenGradient(getHostHitCount(q));
        pieGraph.setTitle("HitCount HOST FROM " + getTitle(q));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getSourcePieChart(EventQueryDefinition q) {
        PieChart pieGraph = renderPieChartGreenGradient(getSourceHitCount(q));
        pieGraph.setTitle("HitCount SOURCE FROM "  + getTitle(q));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getUserPieChart(EventQueryDefinition q) {
        PieChart pieGraph = renderPieChartGreenGradient(getUserHitCount(q));
        pieGraph.setTitle("HitCount USER FROM " + getTitle(q));
        return pieGraph;
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    protected PieChart renderPieChartGreenGradient(Map < String, MutableHitCount > hitRatio) {
        List < String > colors = Util.getColorsGradient(hitRatio.size() + 1);
        return renderPieChart(hitRatio, colors.subList(1, colors.size()));
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    protected PieChart renderPieChartRainBow(Map < String, MutableHitCount > hitRatio) {
        List < String > colors = Util.generateHSVGradient("ee1100", "442299", hitRatio.size());
        return renderPieChart(hitRatio, colors);
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private PieChart renderPieChart(Map < String, MutableHitCount > hitRatio, List < String > colors) {
        PieChart pieChart = new PieChart("n/a");
        int idxColor = 0;
        for (String key : hitRatio.keySet()) {
            Serie<Integer> ps = new Serie<Integer>(key, hitRatio.get(key).get(), colors.get(idxColor));
            pieChart.getSectors().add(ps);
            idxColor++;
        }
        return pieChart;
    }
    
    public BarChart renderBarChartRainbow(Map < String, MutableHitCount > hitRatio) {
        List < String > colors = Util.generateHSVGradient("ee1100", "442299", hitRatio.size());
        return renderBarChart(hitRatio, colors);
    }
    
    public BarChart renderBarChartGreenGradient(Map < String, MutableHitCount > hitRatio) {
        List < String > colors = Util.getColorsGradient(hitRatio.size());
        return renderBarChart(hitRatio, colors);
    }
            
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private BarChart renderBarChart(Map < String, MutableHitCount > hitRatio, List < String > colors) {
        BarChart barChart = new BarChart("n/a");
        int idxColor = 0;
        for (String key : hitRatio.keySet()) {
            Serie<Integer> bar = new Serie<Integer>(key, new Double(hitRatio.get(key).get()).intValue(), colors.get(idxColor));
            barChart.getChartBars().add(bar);
            idxColor++;
        }
        return barChart;
    }

    
    public void orderBarDecrecent(BarChart barChart) {
        Comparator<Serie<Integer>> c = new Comparator<Serie<Integer>>() {
            public int compare(Serie<Integer> o1, Serie<Integer> o2) {
                return o1.getValue() - o2.getValue();
            }
        };
        Collections.sort(barChart.getChartBars(), c);
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getFeatureUsageBarChart(EventQueryDefinition q) {
        BarChart barChart = renderBarChartRainbow(getFeatureUsageHitCount(q));
        barChart.setTitle(TITLE_BARCHAR_HIT + getTitle(q));
        orderBarDecrecent(barChart);
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getHostBarChart(EventQueryDefinition q) {
        BarChart barChart = renderBarChartGreenGradient(getHostHitCount(q));
        barChart.setTitle("BarChart 'host' FROM "  + getTitle(q));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getSourceBarChart(EventQueryDefinition q) {
        BarChart barChart = renderBarChartGreenGradient(getSourceHitCount(q));
        barChart.setTitle("BarChart 'Source' FROM " + getTitle(q));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getUserBarChart(EventQueryDefinition q) {
        BarChart barChart = renderBarChartGreenGradient(getUserHitCount(q));
        barChart.setTitle("BarChart 'User' FROM "  + getTitle(q));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public int getFeatureUsageTotalHitCount(EventQueryDefinition q) {
        Map < String, MutableHitCount > hitRatio = getFeatureUsageHitCount(q);
        int total = 0;
        if (hitRatio != null) {
            for (MutableHitCount hc : hitRatio.values()) {
                total += hc.get();
            }
        }
        return total;
    }
    
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
        EventQueryDefinition q = new EventQueryDefinition();
        sb.append(",\"todayHitsPieChart\": " + getFeatureUsagePieChart(q).toJson());
        sb.append(",\"todayHitsBarChart\": " + getFeatureUsageBarChart(q).toJson());
        getFeatureUsageHistory(q, TimeUnit.HOURS);
        sb.append(",\"todayTotalHitCount\":" + getFeatureUsageTotalHitCount(q));
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Utility.
     *
     * @param evt
     *      current evenement
     * @param startTime
     *      begin time
     * @param endTime
     *      end time
     * @return
     *      if the event is between dates
     */
    protected boolean isEventInInterval(Event evt, long startTime, long endTime) {
        return (evt.getTimestamp() >= startTime) && (evt.getTimestamp() <= endTime);
    }   
    
    /**
     * Format a timestamp to create a Key.
     *
     * @param time
     *      current tick
     * @return
     *      date as Key
     */
    protected String getKeyDate(long time) {
        return KDF.format(new Date(time));
    }
    
    /**
     * Will get a list of all days between 2 dates.
     *
     * @param startTime
     *      tip start
     * @param endTime
     *      tip end
     * @return
     *      list of days
     */
    protected Set < String > getCandidateDays(long startTime, long endTime) {
        if (startTime > endTime) {
            throw new IllegalArgumentException("EndTime must be after start time");
        }
        Set < String > resultKeys = new TreeSet<String>();
        String endKey = getKeyDate(endTime);
        resultKeys.add(endKey);
        long time = startTime;
        while (!endKey.equals(getKeyDate(time))) {
            resultKeys.add(getKeyDate(time));
            time += 3600 * 1000 * 24;
        }
        return resultKeys;
    }    
}
