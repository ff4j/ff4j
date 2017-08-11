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
    
    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureUsagePieChart(EventQueryDefinition q) {
        return renderPieChartRainBow(TITLE_PIE_HITCOUNT + getTitle(q), getFeatureUsageHitCount(q));
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getHostPieChart(EventQueryDefinition q) {
        return renderPieChartGradient("HostNames " + getTitle(q), getHostHitCount(q), "00AB8B", "EEFFEE");
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getSourcePieChart(EventQueryDefinition q) {
       return renderPieChartGradient("Sources "  + getTitle(q), getSourceHitCount(q), "AB008B", "FFEEEE");
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getUserPieChart(EventQueryDefinition q) {
       return renderPieChartGradient("Users " + getTitle(q), getUserHitCount(q), "008BAB", "EEEEFF");
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    protected PieChart renderPieChartGradient(String title, Map < String, MutableHitCount > hitRatio, String fromColor, String toColor) {
       return renderPieChart(title, hitRatio, Util.generateRGBGradient(fromColor, toColor, hitRatio.size()));
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    protected PieChart renderPieChartRainBow(String title, Map < String, MutableHitCount > hitRatio) {
        return renderPieChart(title, hitRatio, Util.generateHSVGradient("ee1100", "442299", hitRatio.size()));
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private PieChart renderPieChart(String title, Map < String, MutableHitCount > hitRatio, List < String > colors) {
        PieChart pieChart = new PieChart(title);
        int idxColor = 0;
        for (String key : hitRatio.keySet()) {
            Serie<Integer> ps = new Serie<Integer>(key, hitRatio.get(key).get(), colors.get(idxColor));
            pieChart.getSectors().add(ps);
            idxColor++;
        }
        return pieChart;
    }
    
    
    protected BarChart renderBarChartRainbow(String title, Map < String, MutableHitCount > hitRatio) {
        return renderBarChart(title, hitRatio, Util.generateHSVGradient("ee1100", "442299", hitRatio.size()));
    }
    
    protected BarChart renderBarChartGradient(String title, Map < String, MutableHitCount > hitRatio, String colorFrom, String colorTo) {
        return renderBarChart(title, hitRatio, Util.generateRGBGradient(colorFrom, colorTo, hitRatio.size()));
    }
    
    /**
     * Generation of title.
     *
     * @param q
     *      current query
     * @return
     *      title formated with slot.
     */
    protected String getTitle(EventQueryDefinition q) {
        return " FROM <b>" + getKeyDate(q.getFrom()) + "</b> TO <b>" + getKeyDate(q.getTo()) + "</b>";
    }
            
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private BarChart renderBarChart(String title, Map < String, MutableHitCount > hitRatio, List < String > colors) {
        BarChart barChart = new BarChart(title);
        int idxColor = 0;
        for (String key : hitRatio.keySet()) {
            Serie<Integer> bar = new Serie<Integer>(key, new Double(hitRatio.get(key).get()).intValue(), colors.get(idxColor));
            barChart.getChartBars().add(bar);
            idxColor++;
        }
        orderBarDecrecent(barChart);
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
        return renderBarChartRainbow(TITLE_BARCHAR_HIT + getTitle(q), getFeatureUsageHitCount(q));
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getHostBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("BarChart 'host' FROM "  + getTitle(q), getHostHitCount(q), "00AB8B", "EEFFEE");
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getSourceBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("BarChart 'Source' FROM " + getTitle(q), getSourceHitCount(q), "AB008B", "FFEEEE");
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getUserBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("BarChart 'User' FROM " + getTitle(q), getUserHitCount(q),  "008BAB", "EEEEFF");
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
        sb.append("\"type\":\"" + this.getClass().getName() + "\"");
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
