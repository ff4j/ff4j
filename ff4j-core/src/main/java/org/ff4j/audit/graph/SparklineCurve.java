package org.ff4j.audit.graph;

/*
 * #%L
 * ff4j-core
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

/**
 * Bean to handle CurveDATA for rendering graph.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class SparklineCurve {

    /** Label for this curve. */
    private String title;

    /** Target line color. */
    private String lineColor;
    
    /** Number of points to be computed. */
    private int nbRecords = 0;
   
    /** bandwith of an interval. */
    private long interval = 0;

    /** List of point to drow for this curve. */
    private List<SparklinePoint> listOfPoint = new ArrayList<SparklinePoint>();

    /**
     * Constructor to create curve from copy
     */
    public SparklineCurve(String title) {
        this.title = title;
    }
    
    /**
     * Initialize a timeSeries
     * 
     * @param title
     *            title of the graph
     * @param startTime
     *            start of measure
     * @param endTime
     *            end of measure
     * @param interval
     *            interval between 2 measures
     */
    public SparklineCurve(String title, long startTime, long endTime, int nbPoints) {
        this(title);
        if (endTime > startTime) {
            this.nbRecords = nbPoints;
            this.interval = (endTime - startTime) / nbPoints;
            for (int slot = 0; slot < nbPoints; slot++) {
                getListOfPoint().add(new SparklinePoint(interval * slot, 0));
            }
        }
    }

    /**
     * Increment hit ratio for this slot.
     * 
     * @param offset
     *            offset of point
     */
    public void incrCount(int offset) {
        SparklinePoint p = getListOfPoint().get(offset);
        p.setY(p.getY() + 1);
    }

    /**
     * Getter accessor for attribute 'lineColor'.
     * 
     * @return current value of 'lineColor'
     */
    public String getLineColor() {
        return lineColor;
    }

    /**
     * Setter accessor for attribute 'lineColor'.
     * 
     * @param lineColor
     *            new value for 'lineColor '
     */
    public void setLineColor(String lineColor) {
        this.lineColor = lineColor;
    }

    /**
     * Getter accessor for attribute 'listOfPoint'.
     * 
     * @return current value of 'listOfPoint'
     */
    public List<SparklinePoint> getListOfPoint() {
        return listOfPoint;
    }

    /**
     * Setter accessor for attribute 'listOfPoint'.
     * 
     * @param listOfPoint
     *            new value for 'listOfPoint '
     */
    public void setListOfPoint(List<SparklinePoint> listOfPoint) {
        this.listOfPoint = listOfPoint;
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

  

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(" \"title\" : \"" + getTitle() + "\", ");
        sb.append(" \"points\" : [");
        boolean first = true;
        for(SparklinePoint p : listOfPoint) {
            if (!first) {
                sb.append(",");
            }
            sb.append("{ \"x\" : " + p.getX() + ", \"y\" : " + p.getY() + "}");
            first = false;
        }
        sb.append("] }");
        return sb.toString();
    }

    /**
     * Getter accessor for attribute 'nbRecords'.
     *
     * @return
     *       current value of 'nbRecords'
     */
    public int getNbRecords() {
        return nbRecords;
    }

    /**
     * Setter accessor for attribute 'nbRecords'.
     * @param nbRecords
     * 		new value for 'nbRecords '
     */
    public void setNbRecords(int nbRecords) {
        this.nbRecords = nbRecords;
    }

    /**
     * Getter accessor for attribute 'interval'.
     *
     * @return
     *       current value of 'interval'
     */
    public long getInterval() {
        return interval;
    }

    /**
     * Setter accessor for attribute 'interval'.
     * @param interval
     * 		new value for 'interval '
     */
    public void setInterval(long interval) {
        this.interval = interval;
    }
}
