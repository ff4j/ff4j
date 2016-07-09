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
import static org.ff4j.utils.TimeUtils.getTodayMidnightTime;
import static org.ff4j.utils.TimeUtils.getTomorrowMidnightTime;

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
    
    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureUsagePieChart(long startTime, long endTime) {
        PieChart pieGraph = renderPieChartRainBow(getFeatureUsageHitCount(startTime, endTime));
        pieGraph.setTitle(TITLE_PIE_HITCOUNT + " FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getHostPieChart(long startTime, long endTime) {
        PieChart pieGraph = renderPieChartGreenGradient(getHostHitCount(startTime, endTime));
        pieGraph.setTitle("HitCount HOST FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getSourcePieChart(long startTime, long endTime) {
        PieChart pieGraph = renderPieChartGreenGradient(getSourceHitCount(startTime, endTime));
        pieGraph.setTitle("HitCount SOURCE FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getUserPieChart(long startTime, long endTime) {
        PieChart pieGraph = renderPieChartGreenGradient(getUserHitCount(startTime, endTime));
        pieGraph.setTitle("HitCount USER FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
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
        List < String > colors = Util.getColorsGradient(hitRatio.size());
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
    public BarChart getFeatureUsageBarChart(long startTime, long endTime) {
        BarChart barChart = renderBarChartRainbow(getFeatureUsageHitCount(startTime, endTime));
        barChart.setTitle(TITLE_BARCHAR_HIT +" FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        orderBarDecrecent(barChart);
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getHostBarChart(long startTime, long endTime) {
        BarChart barChart = renderBarChartGreenGradient(getHostHitCount(startTime, endTime));
        barChart.setTitle("BarChart 'host' FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getSourceBarChart(long startTime, long endTime) {
        BarChart barChart = renderBarChartGreenGradient(getSourceHitCount(startTime, endTime));
        barChart.setTitle("BarChart 'Source' FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getUserBarChart(long startTime, long endTime) {
        BarChart barChart = renderBarChartGreenGradient(getUserHitCount(startTime, endTime));
        barChart.setTitle("BarChart 'User' FROM " + getKeyDate(startTime) + " TO " + getKeyDate(endTime));
        return barChart;
    }
    
    /** {@inheritDoc} */
    @Override
    public int getFeatureUsageTotalHitCount(long startTime, long endTime) {
        Map < String, MutableHitCount > hitRatio = getFeatureUsageHitCount(startTime, endTime);
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
        long start = getTodayMidnightTime();
        long end   = getTomorrowMidnightTime();
        sb.append(",\"todayHitsPieChart\": " + getFeatureUsagePieChart(start, end).toJson());
        sb.append(",\"todayHitsBarChart\": " + getFeatureUsageBarChart(start, end).toJson());
        getFeatureUsageHistory(start, end, TimeUnit.HOURS);
        sb.append(",\"todayTotalHitCount\":" + getFeatureUsageTotalHitCount(start, end));
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
