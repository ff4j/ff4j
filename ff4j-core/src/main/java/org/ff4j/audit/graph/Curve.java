package org.ff4j.audit.graph;

import java.util.ArrayList;
import java.util.List;

/**
 * Bean to handle CurveDATA for rendering graph.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class Curve {

    /** Label for this curve. */
    private String title;

    /** Target line color. */
    private String lineColor;

    /** List of point to drow for this curve. */
    private List<Point> listOfPoint = new ArrayList<Point>();

    /** Type of curve to be drawn. */
    private CurveType curveType = CurveType.SPARKLINE;

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
    public Curve(String title, long startTime, long endTime, long interval) {
        if (endTime > startTime) {
            long nbslots = ((endTime - startTime) / interval) + 1;
            for (int slot = 0; slot < nbslots; slot++) {
                getListOfPoint().add(new Point(interval * slot, 0));
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
        Point p = getListOfPoint().get(offset);
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
    public List<Point> getListOfPoint() {
        return listOfPoint;
    }

    /**
     * Setter accessor for attribute 'listOfPoint'.
     * 
     * @param listOfPoint
     *            new value for 'listOfPoint '
     */
    public void setListOfPoint(List<Point> listOfPoint) {
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

    /**
     * Getter accessor for attribute 'curveType'.
     * 
     * @return current value of 'curveType'
     */
    public CurveType getCurveType() {
        return curveType;
    }

    /**
     * Setter accessor for attribute 'curveType'.
     * 
     * @param curveType
     *            new value for 'curveType '
     */
    public void setCurveType(CurveType curveType) {
        this.curveType = curveType;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "Curve [curveType=" + curveType + ", points=" + listOfPoint + "]";
    }
}
