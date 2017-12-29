package org.ff4j.chart;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.event.EventConstants.TITLE_PIE_HITCOUNT;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.ff4j.event.EventQueryDefinition;
import org.ff4j.monitoring.HitCount;
import org.ff4j.monitoring.RepositoryEventFeatureUsage;

/**
 * Create graphs and charts base on data into UsageService.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class ChartRenderHelper {
    
    /** Start Color. */
    private static final String START_COLOR = "00AB8B";
    
    /** End Color. */
    private static final String END_COLOR = "EEFFEE";
    
    /** Constant for colors. */
    private static final String COLOR_SOURCE_START = "AB008B";
    
    /** Constant for colors. */
    private static final String COLOR_SOURCE_END = "FFEEEE";
    
    /** Constant for colors. */
    private static final String COLOR_USER_START = "008BAB";
    
    /** Constant for colors. */
    private static final String COLOR_USER_END = "EEEEFF";
    
    /** Constant for colors. */
    private static final String COLOR_HOST_START = "00AB8B";
    
    /** Constant for colors. */
    private static final String COLOR_HOST_END = "EEFFEE";
    
    /** Constant for colors. */
    private static final String COLOR_RAINBOW_START = "ee1100";
    
    /** Constant for colors. */
    private static final String COLOR_RAINBOW_END = "442299";
    
    /** Current usage service to leverage on. */
    private RepositoryEventFeatureUsage usageService;
    
    public ChartRenderHelper() {
    }
    
    public ChartRenderHelper(RepositoryEventFeatureUsage usageService) {
        this.usageService = usageService;
    }
    
    /**
     * Dedicated gradient for ff4j console (Pie Chart).
     *
     * @param nbsectors
     *      target sectors
     * @return
     *      color gradient
     */
    public static List < String > getColorsGradient(int nbsectors) {
        return generateRGBGradient(START_COLOR, END_COLOR, nbsectors);
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private PieChart renderPieChart(String title, Map < String, HitCount > hitRatio, List < String > colors) {
        // Create Pie Chart
        PieChart pieChart = new PieChart(title);
        // Color each bar with a color
        int idxColor = 0;
        for (String key : hitRatio.keySet()) {
            Serie<Integer> ps = new Serie<Integer>(key, hitRatio.get(key).get(), colors.get(idxColor));
            pieChart.getSectors().add(ps);
            idxColor++;
        }
        return pieChart;
    }
    
    /**
     * Convert hicount into pieChart.
     *
     * @param hitRatio
     *      current hist ratio
     * @return
     *      pie chart
     */
    private BarChart renderBarChart(String title, Map < String, HitCount > hitRatio, List < String > colors) {
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
    
    private void orderBarDecrecent(BarChart barChart) {
        Comparator<Serie<Integer>> c = new Comparator<Serie<Integer>>() {
            public int compare(Serie<Integer> o1, Serie<Integer> o2) {
                return o1.getValue() - o2.getValue();
            }
        };
        Collections.sort(barChart.getChartBars(), c);
    }
    
    protected String getTitle(EventQueryDefinition q) {
        return " FROM <b>" + getUsageService().getKeyDate(q.getFrom()) + "</b> TO <b>" + getUsageService().getKeyDate(q.getTo()) + "</b>";
    }
    
    protected BarChart renderBarChartRainbow(String title, Map < String, HitCount > hitRatio) {
        return renderBarChart(title, hitRatio, generateHSVGradient(COLOR_RAINBOW_START, COLOR_RAINBOW_END, hitRatio.size()));
    }
    
    protected BarChart renderBarChartGradient(String title, Map < String, HitCount > hitRatio, String colorFrom, String colorTo) {
        return renderBarChart(title, hitRatio, generateRGBGradient(colorFrom, colorTo, hitRatio.size()));
    }
    
    protected PieChart renderPieChartGradient(String title, Map < String, HitCount > hitRatio, String fromColor, String toColor) {
       return renderPieChart(title, hitRatio, generateRGBGradient(fromColor, toColor, hitRatio.size()));
    }
    
    protected PieChart renderPieChartRainBow(String title, Map < String, HitCount > hitRatio) {
        return renderPieChart(title, hitRatio, generateHSVGradient(COLOR_RAINBOW_START, COLOR_RAINBOW_END, hitRatio.size()));
    }
    
    /**
     * Draw a pie chart where each sector is for a feature. The value of each sector is the
     * number of execution of the feature during the period of time.
     * 
     * Pie : Sector/Feature, value- number of check OK
     * 
     * @param startTime
     *            start time of window
     * @param endTime
     *            end time of window
     * @return
     */
    public PieChart getPieChart(EventQueryDefinition query) {
        return renderPieChartRainBow(TITLE_PIE_HITCOUNT + getTitle(query), getUsageService().getHitCount(query));
    }
    
    /**
     * Get hit curves.
     *
     * @param filteredFeatureNames
     *            target feature name set
     * @param nbslot
     *            number of measure
     * @param startTime
     *            starttime for measure
     * @param endTime
     *            endtime for measure
     * @return map of curves
     */
    public BarChart getBarChart(EventQueryDefinition query) {
        return renderBarChartRainbow(TITLE_PIE_HITCOUNT + getTitle(query), getUsageService().getHitCount(query));
    }
    
    /**
     * Use hit getHostHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    public PieChart getHostPieChart(EventQueryDefinition q) {
        return renderPieChartGradient("Hosts "  + getTitle(q),  
                getUsageService().getHostHitCount(q), COLOR_HOST_START, COLOR_HOST_END);
    }
    
    /**
     * Use hit getHostHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    public BarChart getHostBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("Hosts "  + getTitle(q),  
                getUsageService().getHostHitCount(q), COLOR_HOST_START, COLOR_HOST_END);
    }
    
    /**
     * Use hit getHostHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    public PieChart getUserPieChart(EventQueryDefinition q) {
        return renderPieChartGradient("Users "  + getTitle(q),  
                getUsageService().getUserHitCount(q), COLOR_USER_START, COLOR_USER_END);
    }
    
    /**
     * Use hit getHostHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    public BarChart getUserBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("Users "  + getTitle(q),  
                getUsageService().getUserHitCount(q), COLOR_USER_START, COLOR_USER_END);
    }

    /**
     * Use hit getSourceHitCount() to draw a pie chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target Pie
     */
    public PieChart getSourcePieChart(EventQueryDefinition q) {
       return renderPieChartGradient("Sources "  + getTitle(q), 
               getUsageService().getSourceHitCount(q), COLOR_SOURCE_START, COLOR_SOURCE_END);
    }
    
    /**
     * Use hit getSourceHitCount() to draw a bar chart.
     * 
     * @param starTime
     *      begin date
     * @param endTime
     *      end time
     * return target bar
     */
    public BarChart getSourceBarChart(EventQueryDefinition q) {
        return renderBarChartGradient("Sources "  + getTitle(q),  
                getUsageService().getSourceHitCount(q),  COLOR_SOURCE_START, COLOR_SOURCE_END);
    }
    
    /**
     * Create measure over time.
     *
     * @param startTime
     *      time to begin measures
     * @param endTime
     *      time to end measures
     * @param nbPoints
     *      number of points.
     * @param filteredFeatures
     *      if you want to filtered feature usage
     * @return
     */
    public TimeSeriesChart getHistory(EventQueryDefinition query, TimeUnit tu) {
        TimeSeriesChart tsc = getUsageService().getFeatureUsageHistory(query, tu);
        List < String > colors = generateHSVGradient("ee1100", "442299", tsc.getSeries().size());
        int idxColor = 0;
        for (Map.Entry<String, Serie<Map<String, HitCount>>> serie : tsc.getSeries().entrySet()) {
            serie.getValue().setColor(colors.get(idxColor));
            idxColor++;
        }
        return tsc;
    }
    
    
    /**
     * This code build the color gradient between 2 colors with defined step.
     * @param codeFrom
     *      color source
     * @param codeTo
     *      color destination
     * @param nbDivision
     *      number of steps
     * @return
     *      the list of colors
     */
    public static List < String > generateRGBGradient(String codeFrom, String codeTo, int nbDivision) {
        List < String > colors = new ArrayList<String>();
        if (nbDivision < 1) {
           nbDivision = 1;
        }
        nbDivision++;
        int r1 = Integer.parseInt(codeFrom.substring(0, 2), 16);
        int g1 = Integer.parseInt(codeFrom.substring(2, 4), 16);
        int b1 = Integer.parseInt(codeFrom.substring(4, 6), 16);
        int r2 = Integer.parseInt(codeTo.substring(0, 2), 16);
        int g2 = Integer.parseInt(codeTo.substring(2, 4), 16);
        int b2 = Integer.parseInt(codeTo.substring(4, 6), 16);
        int rDelta = (r2 - r1) / nbDivision;
        int gDelta = (g2 - g1) / nbDivision;
        int bDelta = (b2 - b1) / nbDivision;
        for (int idx = 0;idx < nbDivision;idx++) {
            String red   = Integer.toHexString(r1 + rDelta * idx);
            String green = Integer.toHexString(g1 + gDelta * idx);
            String blue  = Integer.toHexString(b1 + bDelta * idx);
            colors.add(red + green + blue);
        }
        return colors.subList(1, colors.size());
    }
    
    public static List < String > generateHSVGradient(String codeFrom, String codeTo, int nbDivision) {
        int r1 = Integer.parseInt(codeFrom.substring(0, 2), 16);
        int g1 = Integer.parseInt(codeFrom.substring(2, 4), 16);
        int b1 = Integer.parseInt(codeFrom.substring(4, 6), 16);
        int r2 = Integer.parseInt(codeTo.substring(0, 2), 16);
        int g2 = Integer.parseInt(codeTo.substring(2, 4), 16);
        int b2 = Integer.parseInt(codeTo.substring(4, 6), 16);
        float[] startHSB = Color.RGBtoHSB(r1, g1, b1, null);
        float[] endHSB   = Color.RGBtoHSB(r2, g2, b2, null);
        float brightness = (startHSB[2] + endHSB[2]) / 2;
        float saturation = (startHSB[1] + endHSB[1]) / 2;
        float hueMax = 0;
        float hueMin = 0;
        if (startHSB[0] > endHSB[0]) {
            hueMax = startHSB[0];
            hueMin = endHSB[0];
        } else {
            hueMin = startHSB[0];
            hueMax = endHSB[0];
        }
        List < String > colors = new ArrayList<String>();
        for (int idx = 0;idx < nbDivision;idx++) {
            float hue = ((hueMax - hueMin) * idx/nbDivision) + hueMin;
            int rgbColor = Color.HSBtoRGB(hue, saturation, brightness);
            colors.add(Integer.toHexString(rgbColor).substring(2));
        }
        return colors;
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + getUsageService().getClass().getName() + "\"");
        EventQueryDefinition q = new EventQueryDefinition();
        sb.append(",\"todayHitsPieChart\": " + getPieChart(q).toJson());
        sb.append(",\"todayHitsBarChart\": " + getBarChart(q).toJson());
        sb.append(",\"todayHitsTimeChart\": " + getHistory(q, TimeUnit.HOURS).toJson());
        sb.append(",\"todayTotalHitCount\":" + getUsageService().getTotalHitCount(q));
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Getter accessor for attribute 'usageService'.
     *
     * @return
     *       current value of 'usageService'
     */
    public RepositoryEventFeatureUsage getUsageService() {
        if (usageService == null) {
            throw new IllegalStateException("UsageService has not been iniatlized");
        }
        return usageService;
    }

    /**
     * Setter accessor for attribute 'usageService'.
     * @param usageService
     * 		new value for 'usageService '
     */
    public void setUsageService(RepositoryEventFeatureUsage usageService) {
        this.usageService = usageService;
    }
    

}
