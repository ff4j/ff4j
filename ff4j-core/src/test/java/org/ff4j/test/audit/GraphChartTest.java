package org.ff4j.test.audit;

import java.util.ArrayList;

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


import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.BarSeries;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.audit.graph.SparklineChart;
import org.ff4j.audit.graph.SparklineCurve;
import org.ff4j.audit.graph.SparklinePoint;
import org.ff4j.utils.Util;
import org.junit.Test;

public class GraphChartTest {
    
    @Test
    public void testBarCharts() {
        BarChart bc = new BarChart();
        bc.setLabels(Util.list("lbl1", "lbl2"));
        Map < String, BarSeries> series = new HashMap<String, BarSeries>();
        BarSeries b1 = new BarSeries("lbl1", "red", 3);
        b1.getValues().add(1D);
        b1.getValues().add(2D);
        b1.getValues().add(3D);
        b1.setColor("red");b1.getColor();
        b1.setLabel("lbl1");b1.getLabel();
        b1.setValues(Arrays.asList(1D,2D,3D));b1.getValues();
        series.put("lbl1", b1);
        series.put("lbl2", b1);
        bc.setSeries(series);
        bc.toString();
    }
    
    @Test
    public void testPieCharts() {
        PieChart pc = new PieChart("TItle");
        pc.setTitle("Title2");
        List < PieSector > sectors = new ArrayList<PieSector>();
        PieSector ps1 = new PieSector("a", 1d);
        ps1.setLabel("lbl1");ps1.getLabel();
        ps1.setColor("red");ps1.getColor();
        ps1.setValue(1D);ps1.getValue();
        ps1.toString();
        sectors.add(ps1);
        sectors.add(ps1);
        sectors.add(ps1);
        pc.setSectors(sectors);
        pc.getSectors().add(ps1);
        
        PieSector ps2 = new PieSector("b", 1d);
        pc.getSectors().add(ps2);
        pc.toString();
    }
    
    @Test
    public void testSparkLineCharts() {
        SparklineCurve sc = new SparklineCurve("title");
        sc = new SparklineCurve("title2", System.currentTimeMillis(), (System.currentTimeMillis()+5), 5);
        sc.setInterval(1);sc.getInterval();
        sc.setNbRecords(3);sc.getNbRecords();
        sc.setLineColor("red");sc.getLineColor();
        sc.setTitle("Ttitle");sc.getTitle();
        sc.incrCount(1);
        List < SparklinePoint > lPoints = new ArrayList<SparklinePoint>();
        SparklinePoint p1 = new SparklinePoint(1, 1);
        p1.setX(1);p1.getX();
        p1.setY(1);p1.getY();
        p1.toString();
        lPoints.add(p1);
        lPoints.add(p1);
        lPoints.add(p1);
        sc.setListOfPoint(lPoints);sc.getListOfPoint();
        sc.toString();
        
        new SparklineChart(); 
    }

}
